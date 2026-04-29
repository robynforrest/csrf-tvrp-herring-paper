# Look at steepness estimated by OM compared to iscam values

# April 27, 2026
# Robyn Forrest

# QUANG:
# openMSE parameterizes the SRR from R0 and steepness
# but uses a hard-coded definiton of phi_0 (similar to what's done for Historical B_0).
# The alpha and beta parameters between openMSE and iSCAM are compared to validate the SRR

# Compare OM to iscam R0, Steepness, baseline M, SBt, Rt,RecDevs, Mt
library(here)
library(tidyverse)
library(reshape2)
library(ggsci)
library(viridis)
library(RColorBrewer)
source(here("R","99_get_objects.R"))
source(here("R","0_settings.R"))

# Read in OMs
histMSEs <- readRDS(here("MSEs/hist_hMSEs.rda"))
stocks <- names(histMSEs)
nstocks <- length(stocks)
nsim <- histMSEs[[1]]@OM@nsim

# Read in iscam outputs
iscamlocs <- paste(here::here(),list.files("Data/Herring_iscam",full.names=T,include.dirs=T),sep="/")
syr <- 1951
nyr <- 2023
nyrs <- length(syr:nyr)
conflo <- 0.025
confhi <- 0.975

for(j in 1:nstocks){

  # ISCAM MCMC OUTPUTS
  iscamloc <- iscamlocs[j]
  mcmcout <- read.csv(file.path(iscamloc,"iscam_mcmc.csv"), header=T)
  sbt <- read.csv(file.path(iscamloc,"iscam_sbt_mcmc.csv"), header=T)
  rt <- read.csv(file.path(iscamloc,"iscam_rt_mcmc.csv"), header=T)
  rdev <- read.csv(file.path(iscamloc,"iscam_rdev_mcmc.csv"), header=T)
  mt <- read.csv(file.path(iscamloc,"iscam_m_mcmc.csv"), header=T)
  colnames(sbt) <- syr:(nyr+1)
  colnames(rt) <- colnames(rdev) <- (syr+2):(nyr)
  colnames(mt) <- syr:nyr

  # Make iscam dataframes
  # Parameters
  mcmcout_df <- mcmcout |>
    select(ro_gr1,h_gr1,m_gs1, sbo) |>
    as.data.frame() |>
    rename(ro=ro_gr1,h=h_gr1,m=m_gs1, sbo=sbo)

  nrep <- length(mcmcout_df$ro)

  # Read in MSE object
  StockDirMSE   <- here("MSEs", paste(stocks[j]))
  mse <- readRDS(here(StockDirMSE, "hMSEs_NF.rda"))[[1]] # just take first scenario

  # Parameters
  ompars <- getPars_om_static(mse, "ompars", species="pac-herring") |>
    filter(Variable %in% c("h", "R0","M")) |>
    pivot_wider(names_from = Variable, values_from=Value, id_cols=c(Sim,Scenario)) |>
    select(-Sim,-Scenario) |>
    rename(ro=R0,m=M) |>
    mutate(ro=1000*ro)

  # We're just comparing OM B0 with the 2023 iscam B0
  bo <- getmeanB0(mse,scen="", age=3, type="mean", quants=FALSE)[,73]
  ompars <- cbind(ompars,bo)
  rm(bo)

  # M
  mt_om <- getM(mse,"Annual", age=3, type="annual", quant=FALSE, input_type="MSE")

  ##################################################################################
  # Now get iscam alpha and beta posteriors based on steepness and ro posteriors
  # We just need to make sure the SR parameters are correct

  # ISCAM
  # Get alpha and beta
  SRpars_iscam <- matrix(nrow=nrep,ncol=4) |> as.data.frame()
  colnames(SRpars_iscam) <- c("ro","steepness","SRalpha","SRbeta")
  annualfec <- getFec(mse,"annual", type="annual", input_type="MSE") #(check - matches d3_wt_mat in iscam rep file)
  meanfec <- getFec(mse,"Long-term mean", type="mean", input_type="MSE")[,nyrs]

  for(simno in 1:nrep){
    R0orig <- mcmcout_df$ro
    Steeporig <- mcmcout_df$h
    phie0orig <- mcmcout_df$sbo[simno]/mcmcout_df$ro[simno]
    # check that phie0 matches long-term mean (this is how iscam calculates phib, i.e., phie0)
    meanM <- mean(as.numeric(mt[simno,1:nyrs]))
    # get fec from OM, it is read in perfectly
    phie0test <- calc_phie0(meanM,meanfec,spawn_frac=1)
    round(phie0orig,3) # ignore rounding error
    round(phie0test,3)

    #output df
    SRpars_iscam$ro <- R0orig
    SRpars_iscam$steepness  <- Steeporig
    SRpars_iscam$SRalpha[simno] <- MSEtool::SRalphaconv(Steeporig[simno], phie0orig, SR=1, type = 1)
    SRpars_iscam$SRbeta[simno]  <- MSEtool::SRbetaconv(Steeporig[simno], R0orig[simno], phie0orig, SR=1, type = 1)
  } # end simno

  # iscam doesn't report alpha and beta posteriors but the MPDs are close to the medians
  #  calculated here (for HG stock)
  # median(SRpars_iscam$SRalpha) # MPD (HG) = 199.3
  # median(SRpars_iscam$SRbeta)  # MPD (HG) = 0.76
  # quantile(SRpars_iscam$SRalpha)
  # quantile(SRpars_iscam$SRbeta)

  # OK. These are the iscam alpha and beta values.
  cols <- pal_lancet("lanonc")(5)[1:3]

  g1 <- SRpars_iscam |>
    ggplot()+
    geom_density(aes(x=SRalpha), fill=cols[1], alpha=0.3)+
    mytheme_paper+
    ggtitle("iscam SRalpha")

  g2 <- SRpars_iscam |>
    ggplot()+
    geom_density(aes(x=SRbeta), fill=cols[1], alpha=0.3)+
    mytheme_paper+
    ggtitle("iscam SRbeta")

  cowplot::plot_grid(g1,g2)
  ggsave(here("Figures",paste0("Testing_SRpars",stocks[j],".png")),
         width = 8, height = 5)

  # Assuming that these are the alpha and beta paramters being used in the OM
  # I should be able to back-calculate om steepness using the alpha parameter
  # and a historical phie0 value
  endhistyrM <- 9
  endhistyrFec <- 9
  SRpars_om_hist <- matrix(nrow=nsim,ncol=5) |> as.data.frame()
  colnames(SRpars_om_hist) <- c("steepness_iscam","steepness_om","SRalpha", "phie0", "steepness_back")

  for(simno in 1:nsim){
    SRalpha_iscam <- SRpars_iscam$SRalpha[simno]

    #steepness values for comparison
    Steep_iscam  <- mcmcout_df$h[simno]  # start with iscam values
    SteepOM    <- ompars$h[simno]

    # Get the inputs of phie0 for first 'endhistyr' years
    inputM     <- mean(as.numeric(mt[simno,1:endhistyrM]))
    # fecundity (get from OM it is the same as iscam rep file)
    OMwa_Annual <- mse@Hist@AtAge$Weight[1,-c(1,2),1:endhistyrFec] # weight at age, cut off age 0 and 1
    OMma_Annual <- mse@Hist@AtAge$Maturity[1,-c(1,2),1:endhistyrFec] # maturity at age, cut off age 0 and 1
    OMfa_Annual <- OMwa_Annual*OMma_Annual # ages 2:10, dim: nrow=nages ncol=nyears
    inputFec <- apply(OMfa_Annual,1,mean) # historical mean fec at age
    spawn_time_frac <- 1

    # Make a hard wired historical phie0
    phie0hist  <- calc_phie0(M=inputM,fec=inputFec,spawn_frac=spawn_time_frac)

    #output df
    SRpars_om_hist$steepness_iscam[simno]  <- Steep_iscam # iscam value
    SRpars_om_hist$steepness_om[simno]  <- SteepOM # iscam value
    SRpars_om_hist$SRalpha[simno] <- SRalpha_iscam
    SRpars_om_hist$phie0[simno]   <- phie0hist
    # back-calculate steepness - should be same as OM
    SRpars_om_hist$steepness_back[simno] <- MSEtool::hconv(SRalpha_iscam,phie0hist, SR=1, type = 1)
  } # end simno

  g3 <- SRpars_om_hist |>
    ggplot()+
    geom_density(aes(x=steepness_iscam, fill="iscam"), alpha=0.3)+
    geom_density(aes(x=steepness_om, fill="OM_orig"), alpha=0.3)+
    geom_density(aes(x=steepness_back, fill="OM_back_calc"),lty=2, alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("iscam"=cols[1], "OM_orig"=cols[2], "OM_back_calc"=cols[3]))+
    ggtitle("Steepness - back-calculated from phie0 with hist M and fec")
  g3
  ggsave(here("Figures",paste0("Testing_Steep_histphie0",stocks[j],".png")),
         width = 8, height = 5)

  # Now use long-term average M and fec to calculate phie0
  # Should match iscam steepness
  SRpars_om_mean <- matrix(nrow=nsim,ncol=5) |> as.data.frame()
  colnames(SRpars_om_mean) <- c("ro","steepness_orig","SRalpha","SRbeta", "steepness_longterm")
  meanM <- getM(mse,"Long-term mean", age=3, type="mean", quant=FALSE, input_type="MSE")
  meanFeca <- getFec(mse,"Long-term mean", type="mean", input_type="MSE")

  for(simno in 1:nsim){
    Steep_iscam  <- mcmcout_df$h[simno]  # start with iscam values
    SRalpha_iscam <- SRpars_iscam$SRalpha[simno]
    SteepOM    <- ompars$h[simno]

    # We just want the long term mean in 2023
    inputM <- meanM[simno,mse@nyears]
    # fecundity
    inputFec <- meanFeca[,mse@nyears]
    spawn_time_frac <- 1

    # Make mean phie0 to match iscam
    phie0mean  <- calc_phie0(M=inputM,fec=inputFec,spawn_frac=spawn_time_frac)

    #output df
    SRpars_om_mean$steepness_iscam[simno]  <- Steep_iscam # iscam value
    SRpars_om_mean$steepness_om[simno]  <- SteepOM # iscam value
    SRpars_om_mean$SRalpha[simno] <- SRalpha_iscam
    SRpars_om_mean$phie0[simno]   <- phie0hist
    # back-calculate steepness - should be exactly the same as iscam
    SRpars_om_mean$steepness_back[simno] <- MSEtool::hconv(SRalpha_iscam,phie0mean, SR=1, type = 1)
  } # end simno

  g4 <- SRpars_om_mean |>
    ggplot()+
    geom_density(aes(x=steepness_iscam, fill="iscam"), alpha=0.3)+
    geom_density(aes(x=steepness_om, fill="OM_orig"), alpha=0.3)+
    geom_density(aes(x=steepness_back, fill="OM_back_calc"),lty=2, alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("iscam"=cols[1], "OM_orig"=cols[2], "OM_back_calc"=cols[3]))+
    ggtitle("Steepness - back-calculated from phie0 with long-term mean M and fec")
  g4
  ggsave(here("Figures",paste0("Testing_Steep_meanphie0",stocks[j],".png")),
         width = 8, height = 5)
}

