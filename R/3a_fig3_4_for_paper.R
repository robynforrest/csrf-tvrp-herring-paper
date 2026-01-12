# Make the figures for the paper - this is called from 0_run-analyses.R but
# can stand alone
# January 2, 2026
# Robyn Forrest

library(here)
source(here("R","0_settings.R")) # this includes the function calc_tv_B0

# Load the historical MSEs.
# The OM scenarios will be loaded in the loop

histMSEs <- readRDS(here("MSEs", "hist_hMSEs.rda"))
stocks <- names(histMSEs)
nstocks <- length(stocks)

# Create lists for putting figures
fig3 <- list()
fig3_alternative <- list()
fig4 <- list()

# FIGURE 2.ANALYTICAL RELATIONSHIP BETWEEN M AND B0
for(j in 1:nstocks){
  cat("~~~ Plotting Fig 3 for", paste(stocks[j]), "~~~\n")

  nsim <- histMSEs[[j]]@OM@nsim
  nyears <- histMSEs[[j]]@OM@nyears
  yind <- histMSEs[[j]]@OM@nyears+(1:histMSEs[[j]]@OM@proyears) #projection years

  # Make directories
  StockDirOM    <- here("OMs", paste(stocks[j]))
  StockDirMSE   <- here("MSEs", paste(stocks[j]))
  StockDirFigs  <- here("Figures", paste(stocks[j]))
  if(!file.exists(StockDirFigs)) dir.create(StockDirFigs, recursive=TRUE)
  if(!file.exists(StockDirMSE)) stop("Stop. No MSEs found. Please run MSEs first. \n")

  OM  <- readRDS(here(StockDirOM, "OMscenarios.rda"))[[1]]
  histMSE <- histMSEs[[j]]
  stock <- stocks[j]

  # inputs for calc_B0
  # First get the stock-recruit parameters, using long-term mean and fec
  # iscam uses long-term mean and fec in calcStockRecruitment()
  inputM   <- getM(OM,"scenario 1",age=3,type="mean", quant=FALSE) # A matrix of long-term mean M Dim: nrow=nreps, ncol=nyears
  inputFec <- getFec(OM,"scenario 1",type="mean") # A matrix of annual long-term mean fecundity-at-age (same for all reps). Dim: nrow=nages, ncol=nyears
  inputSteep <- histMSE@OMPars$hs
  spawn_time_frac <- histMSE@OMPars$spawn_time_frac

  # Get alpha and beta (these are assumed fixed with changing M)
  SRpars <- matrix(nrow=nsim,ncol=4) |> as.data.frame()
  colnames(SRpars) <- c("SRalpha","SRbeta", "M", "B0")
  for(simno in 1:nsim){
      Pars <- list(
        B0=histMSE@OMPars$SSB0[simno], # A vector of B0 needed to get S-R  alpha and beta. Length=nreps
        R0=histMSE@OMPars$R0[simno],
        Steep=histMSE@OMPars$hs[simno],
        meanM=inputM[simno,nyears], # long term mean M in 2023 (final iscam year)
        meanFec=inputFec[,nyears],  # long term mean M in 2023 (final iscam year)
        spawn_frac=spawn_time_frac[simno]
      )
      Out <- calc_tv_B0(Pars)
      SRpars[simno,1] <- Out$SRalpha
      SRpars[simno,2] <- Out$SRbeta
      SRpars[simno,3] <- Pars$meanM # long-term mean M from OM
      SRpars[simno,4] <- Pars$B0 # original B0 from OM (default, calculated from hist M)
  } # end simno

  # 2. Loop over the nsim SR parameters and M to get relationships between B0 and M
  #    across the range of SR parameters
  # Randomly subset the replicates if nsim is >10
  if(nsim <= 10){
    Nsim = 1:nsim
  }else{
    Nsim = sample(2:nsim, 10 ,replace=FALSE)
    Nsim[1] <- 1 # Always take the first sample so loop works below
  }

  inputM   <- seq(0.2,1.,by=0.025)
  nM <- length(inputM)
  for(simno in Nsim){
    # Now we have the alpha and beta parameters, we can plot relationship between
    #  M and B0 over a sequence of M (with a fixed set of SR parameters) for each replicate
    #  Use a version of calc_B0 which has leading SR pars (not R0 and Steepness)
    inputSRalpha <- SRpars[simno,1]
    inputSRbeta  <- SRpars[simno,2]

    Outpars_tmp <- matrix(nrow=nM,ncol=7) |> as.data.frame()
    colnames(Outpars_tmp) <- c("M","SRalpha", "SRbeta","B0", "R0", "phie0", "Sim")
    for(mval in 1:nM){
        Pars <- list(
          SRalpha=inputSRalpha,
          SRbeta=inputSRbeta,
          M=inputM[mval],
          Fec=inputFec[,2], # Trying to reproduce what MSEtool is doing - this is the mean of first 2 years
          spawn_frac=1
        )
        Out <- calc_tv_B0_alphabeta(Pars) # this version uses inputs of alpha, beta, M and fecundity
        Outpars_tmp[mval,1] <- Pars$M # The sequence of input values
        Outpars_tmp[mval,2] <- Pars$SRalpha # Input SRalpha
        Outpars_tmp[mval,3] <- Pars$SRbeta # Input SRbeta
        Outpars_tmp[mval,4] <- Out$B0_new # Implied B0 from M and SR pars
        Outpars_tmp[mval,5] <- Out$R0_new # Implied R0 from M and SR pars
        Outpars_tmp[mval,6] <- Out$phie0_new # Implied Phie from M and fecundity
        Outpars_tmp[mval,7] <- paste("Sim",simno)
    } # end mval
    if(simno==1){
      Outpars <- Outpars_tmp
    }else{
      Outpars <- rbind(Outpars,Outpars_tmp)
    }
  } # end simno

  # Look at relationship between M and B0 within OM replicate (each point has different SR pars)
  g1 <- Outpars |>
    ggplot(aes(x=M, y=B0, colour=Sim))+
    geom_point(size=3)+
    scale_colour_viridis_d()+
    mytheme
  g1
  ggsave(file.path(StockDirFigs, paste0("FIG3_M_B0_v1_",stocks[j],".png")),
         width = 8, height = 5)

  # Look at ribbon plot version (use all sims)
  inputM   <- seq(0.2,1.,by=0.025)
  nM <- length(inputM)
  for(simno in 1:nsim){
    # Now we have the alpha and beta parameters, we can plot relationship between
    #  M and B0 over a sequence of M (with a fixed set of SR parameters) for each replicate
    #  Use a version of calc_B0 which has leading SR pars (not R0 and Steepness)
    inputSRalpha <- SRpars[simno,1]
    inputSRbeta  <- SRpars[simno,2]

    Outpars_tmp <- matrix(nrow=nM,ncol=8) |> as.data.frame()
    colnames(Outpars_tmp) <- c("M", "SRalpha", "SRbeta","B0", "R0", "phie0", "JuvSurvival", "Sim")
    for(mval in 1:nM){
      Pars <- list(
        SRalpha=inputSRalpha,
        SRbeta=inputSRbeta,
        M=inputM[mval],
        Fec=inputFec[,2], # Trying to reproduce what MSEtool is doing - this is the mean of first 2 years
        spawn_frac=1
      )
      Out <- calc_tv_B0_alphabeta(Pars) # this version uses inputs of alpha, beta, M and fecundity
      Outpars_tmp[mval,1] <- Pars$M # The sequence of input values
      Outpars_tmp[mval,2] <- Pars$SRalpha # Input SRalpha
      Outpars_tmp[mval,3] <- Pars$SRbeta # Input SRbeta
      Outpars_tmp[mval,4] <- Out$B0_new # Implied B0 from M and SR pars
      Outpars_tmp[mval,5] <- Out$R0_new # Implied R0 from M and SR pars
      Outpars_tmp[mval,6] <- Out$phie0_new # Implied Phie from M and fecundity
      Outpars_tmp[mval,7] <- 1/Out$phie0_new # Implied Juv Survival from M and fecundity
      Outpars_tmp[mval,8] <- paste("Sim",simno)
    } # end mval
    if(simno==1){
      Outpars <- Outpars_tmp
    }else{
      Outpars <- rbind(Outpars,Outpars_tmp)
    }
  } # end simno

  Outpars_range <- Outpars |>
    select(Sim, M, B0) |>
    group_by(M) |>
    summarise("Lwr"=quantile(B0, probs=0.025), "Med"=median(B0), "Upr"=quantile(B0, probs=0.975))

  g2 <-  ggplot(Outpars_range)+
    geom_ribbon(aes(x=M,ymin=Lwr,ymax=Upr), alpha=0.3, colour="purple", fill="purple")+
    geom_line(aes(x=M,y=Med), alpha=0.3, colour="purple", lwd=2)+
    ylab("B0")+
    mytheme
  g2
  ggsave(file.path(StockDirFigs, paste0("FIG3_M_B0_v2_",stocks[j],".png")),
         width = 8, height = 5)

# FIGURE 4. STOCK-RECRUIT CURVE WITH ALTERNATIVE B0, R0 AND REPLACEMENT LINES
# Now take a random replicate from Outpars and make the S-R curve
cat("~~~ Plotting Fig 4 for", paste(stocks[j]), "~~~\n")

Reps <- unique(Outpars$Sim)
randrep <- sample(Reps,1,replace=F)

Outpars_rep <- Outpars |>
  filter(Sim %in% randrep, M %in% seq(0.2,1,by=0.1))

# Plot SR curve
B <- seq(0, 1.25*max(Outpars_rep$B0), 0.01)
R <- Outpars_rep$SRalpha[1] * B / (1 + Outpars_rep$SRbeta[1] * B)
SR_xy <- cbind(B,R) |> as.data.frame()

g3 <- Outpars_rep |>
  mutate(x1=0,xend1=B0,y1=0,yend1=R0) |>
  ggplot()+
  geom_segment(aes(x=x1,y=y1,xend=xend1,yend=yend1, colour=M),linewidth=0.5,
               inherit.aes = FALSE)+
  geom_line(data=SR_xy, aes(x=B,y=R),linewidth=1)+
  geom_point(aes(x=xend1,y=yend1, colour=M), size=3)+
  scale_colour_viridis()+
  mytheme+
  xlab("SSB")+ylab("Recruits")
g3
ggsave(file.path(StockDirFigs, paste0("FIG4_Stock-Recruit_",stocks[j],".png")),
       width = 8, height = 5)

# Add the figures to lists
fig3[[j]] <- g2
fig3_alternative[[j]] <- g1
fig4[[j]] <- g3

# Write out table of values for a single replicate
write_csv(Outpars_rep, file=file.path(StockDirFigs, paste0("TABLE1_Stock-Recruit_fromFig3",stocks[j],".csv")))

# Write all reps out for supp
write_csv(Outpars, file=file.path(StockDirFigs, paste0("TABLE1_Stock-Recruit_allreps_for_supp",stocks[j],".csv")))

} #end for j

