# Script for calculating t-v B0 from Pac herring outputs

library(here)
library(openMSE)
source(here("R","0_settings.R")) #contains functions calc_tv_B0, getM and getFec

# Read in WCVI Pac herring OM and histMSE
histmses <- readRDS(here("MSEs","hist_hMSEs.rda")) # need this because om doesn't have B0
stocks <- names(histmses)
nstocks <- length(stocks)

for(j in 1:nstocks){

    StockDirOM <- here(SpDirOM, paste(stocks[j]))
    if(!file.exists(StockDirOM)) message("Stop. No OMs found. Please run 1_make-oms.R first. \n")
    StockDirMSE <- here(SpDirMSE, paste(stocks[j]))
    if(!file.exists(StockDirMSE)) dir.create(StockDirMSE, recursive=TRUE)
    StockDirFigs <- here(SpDirFigs, paste(stocks[j]))
    if(!file.exists(StockDirFigs)) dir.create(StockDirFigs, recursive=TRUE)

    histmse <- histmses[[1]]
    om <- readRDS(here(StockDirOM, "OMscenarios.rda"))[[1]]
    nyears <- om@nyears

    # returns all replicates
    inputM   <- getM(om,"scenario 1",age=3,type="mean", quant=FALSE) # A matrix of long-term mean M Dim: nrow=nreps, ncol=nyears
    inputFec <- getFec(om,"scenario 1",type="mean") # A matrix of annual long-term mean fecundity-at-age (same for all reps). Dim: nrow=nages, ncol=nyears
    spawn_time_frac <- histmse@OMPars$spawn_time_frac

    nsim<- length(spawn_time_frac)

    simno <- 1

    # 1. Figure out what years MSEtool was using for M to calculate original phie0 and B0
    # Reproduce original phi0 with choice of years for meanM and meanFec
    # Looks like MSEtool is using mean of first 4 years to calculate B0,
    # with mean of first two years of fecundity
    Pars <- list(
      B0=histmse@OMPars$SSB0[simno], # A vector of B0 needed to get S-R  alpha and beta. Length=nreps
      R0=histmse@OMPars$R0[simno],
      Steep=histmse@OMPars$hs[simno],
      meanM=inputM[simno,4],# Trying to reproduce what MSEtool is doing - this is the mean of first 4 years
      meanFec=inputFec[,2], # Trying to reproduce what MSEtool is doing - this is the mean of first 2 years
      spawn_frac=spawn_time_frac[simno]
    )
    Output <- calc_tv_B0(Pars)

    # Now look at unfished replacement line (1/phie0)
    # Original stock recruit parameters from MSEtool hist object
      R0   <- Output$R0_orig
      phi0 <- Output$phie0_orig
      h <- histmse@OMPars$hs[simno]
      B0 <- phi0 * R0
      #test
      B0==Output$B0_orig # yes

      # Assume fixed a and b SRR pars
      a <- MSEtool::SRalphaconv(h, phi0)
      b <- MSEtool::SRbetaconv(h, R0, phi0)
      # test that calc_TV_B0 is returning the same values
      round(a,5)==round(Output$SRalpha,5) # yes
      round(b,5)==round(Output$SRbeta,5)  # yes

      # New phi0 implied by M  -- in this case we are testing what MSEtool is doing and trying to reproduce
      # the unfished replacement line
      phi0_new <- Output$phie0_new # this would normally be phie0 with the new M but
                                  # in this case we are trying to give it the same mean M as the default in MSEtool
      R0_new <- R0conv(a, b, phi0_new)
      B0_new <- phi0_new * R0_new
      # test - should be the same because M is the same
      round(R0_new,4)==round(Output$R0_new,4)
      round(B0_new,4)==round(Output$B0_new,4)

      # Plot SRR
      B <- seq(0, 1.5* B0, 0.01)
      R <- (a * B )/ (1 + b * B)
      plot(B, R, typ = "l", main="Reproducing the OM SRR with historic mean M and fecundity")

      abline(a = 0, b = 1/phi0, lty = 2) # Original replacement line and R0/B0
      points(B0, R0)

      abline(a = 0, b = 1/phi0_new, lty = 2, col = 2) # Unfished reference points with new replacement line
      points(B0_new, R0_new, col = 2)

    # OK. now look at alternative values of M within a replicate
    # Start with my alternative values of mean M in the final historical year
    B0type <- c("hist","mean","recent")

    # Plot over a range of M values:length(B0type)
    Outlist <- list()
    for(k in 1:length(B0type)){
      MeanM <- getM(om,"scenario 1",age=3,type=B0type[k], quant=FALSE)[simno,nyears]
      Pars <- list(
        B0=histmse@OMPars$SSB0[simno], # A vector of B0 needed to get S-R  alpha and beta. Length=nreps
        R0=histmse@OMPars$R0[simno],
        Steep=histmse@OMPars$hs[simno],
        meanM=MeanM,
        meanFec=inputFec[,2], # use the same fec as was used to get original B0?
        spawn_frac=spawn_time_frac[simno]
      )
      Outlist[[k]] <- calc_tv_B0(Pars)

      # Plot SRR
      if(k==1){
        B <- seq(0, 1.5*Outlist[[k]]$B0_orig, 0.01)
        R <- Outlist[[k]]$SRalpha * B / (1 + Outlist[[k]]$SRbeta * B)
        plot(B, R, typ = "l")

        abline(a = 0, b = 1/Outlist[[k]]$phie0_orig, lty = 2, lwd=2) # Original replacement line and R0/B0
        points(Outlist[[k]]$B0_orig, Outlist[[k]]$R0_orig)
      }

      abline(a = 0, b = 1/Outlist[[k]]$phie0_new, lty = 2, col = k+1, lwd=2) # Unfished reference points with new replacement line
      points(Outlist[[k]]$B0_new, Outlist[[k]]$R0_new, col = k+1)
    }
    legend("bottomright", legend=c("OM original",B0type), lty=2, lwd=2, col=1:(length(B0type)+1), bty="n")

    ###########################################################################################################
    # ALTERNATIVE ASSUMPTION: R0 stays the same --> implies different SR parameters and a new SRR
    Outlist <- list()
    for(k in 1:length(B0type)){
      MeanM <- getM(om,"Scenario 1",age=3,type=B0type[k], quant=FALSE)[simno,nyears]
      Pars <- list(
        B0=histmse@OMPars$SSB0[simno], # A vector of B0 needed to get S-R  alpha and beta. Length=nreps
        R0=histmse@OMPars$R0[simno],
        Steep=histmse@OMPars$hs[simno],
        meanM=MeanM,
        meanFec=inputFec[,2], # use the same fec as was used to get original B0?
        spawn_frac=spawn_time_frac[simno]
      )
      Outlist[[k]] <- calc_tv_B0(Pars)

      R0 <- Outlist[[k]]$R0_orig
      print(R0)
      phi0_new <- Outlist[[k]]$phie0_new
      B0_new <- R0*phi0_new
      a_new <- MSEtool::SRalphaconv(h, phi0_new)
      b_new <- MSEtool::SRbetaconv(h, R0, phi0_new)
      B <- seq(0, 1.5* B0, 0.01)
      R_new <- a_new * B / (1 + b_new * B)

      # Plot SRR
      if(k==1){
        plot(B, R_new, typ = "l", ylim=c(0,700))
        abline(a = 0, b = 1/phi0_new, lty = 2, col = 1) # Unfished reference points with new replacement line
        points(B0_new, R0, col = k+1)
      }
      lines(B, R_new, col=k+1)
      abline(a = 0, b = 1/Outlist[[k]]$phie0_new, lty = 2, col = k+1, lwd=2) # Unfished reference points with new replacement line
      points(B0_new, R0, col = k+1)

    }
    abline(h=Outlist[[k]]$R0_orig, lty = 2, col = 1, lwd=0.5) # Unfished reference points with new replacement line
    legend("bottomright", legend=c("OM original",B0type), lty=1, lwd=2, col=1:(length(B0type)+1), bty="n")

} # End stock j

