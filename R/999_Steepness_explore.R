# Look at steepness estimated by OM compared to iscam values

# April 27, 2026
# Robyn Forrest

# QUANG:
# openMSE parameterizes the SRR from R0 and steepness
# but uses a hard-coded defitiniton of phi_0 (similar to what's done for Historical B_0).
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

   # Make iscam dataframes for plotting
  # Parameters
  mcmcout_df <- mcmcout |>
    select(ro_gr1,h_gr1,m_gs1, sbo) |>
    as.data.frame() |>
    rename(ro=ro_gr1,h=h_gr1,m=m_gs1, bo=sbo)

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
  mt_om <- getM(mse,"Historical", age=3, type="annual", quant=FALSE, input_type="MSE")

  ##################################################################################
  # Now get iscam alpha and beta posteriors based on steepness and ro posteriors
  # We just need to make sure the SR parameters are correct

  # ISCAM
  # Get alpha and beta
  SRpars_iscam <- matrix(nrow=nrep,ncol=4) |> as.data.frame()
  colnames(SRpars_iscam) <- c("ro","steepness","SRalpha","SRbeta")
  for(simno in 1:nrep){
    R0orig <- mcmcout_df$ro
    Steeporig <- mcmcout_df$h
    phie0orig <- mcmcout_df$bo/mcmcout_df$ro
    #M_histmean <- mean(as.numeric(mt[simno,1:5]))
    # phie0hist <-

    #output df
    SRpars_iscam$ro <- R0orig
    SRpars_iscam$steepness  <- Steeporig
    SRpars_iscam$SRalpha[simno] <- MSEtool::SRalphaconv(Steeporig[simno], phie0orig[simno], SR=1, type = 1)
    SRpars_iscam$SRbeta[simno]  <- MSEtool::SRbetaconv(Steeporig[simno], phie0orig[simno], R0orig[simno], SR=1, type = 1)
  } # end simno

  endhistyr <-10

  # OM
  # Get alpha and beta using phie0 from early hist M
  SRpars_om <- matrix(nrow=nsim,ncol=4) |> as.data.frame()
  colnames(SRpars_om) <- c("ro","steepness","SRalpha","SRbeta")
  for(simno in 1:nsim){
    R0orig     <- ompars$ro
    Steeporig  <- ompars$h
    # We just want the historical average of the first five years
    inputM     <- mean(mt_om[simno,1:endhistyr])
    # fecundity
    OMwa_Annual <- mse@Hist@AtAge$Weight[1,-c(1,2),1:endhistyr] # weight at age, cut off age 0 and 1
    OMma_Annual <- mse@Hist@AtAge$Maturity[1,-c(1,2),1:endhistyr] # maturity at age, cut off age 0 and 1
    OMfa_Annual <- OMwa_Annual*OMma_Annual # ages 2:10, dim: nrow=nages ncol=nyears
    OMfa_mean <- apply(OMfa_Annual,1,mean) # historical mean fec at age
    spawn_time_frac <- 1

    phie0hist  <- calc_phie0(M=inputM,fec=OMfa_mean,spawn_frac=spawn_time_frac)

    #output df
    SRpars_om$ro <- R0orig
    SRpars_om$steepness  <- Steeporig
    SRpars_om$SRalpha[simno] <- MSEtool::SRalphaconv(Steeporig[simno], phie0hist, SR=1, type = 1)
    SRpars_om$SRbeta[simno]  <- MSEtool::SRbetaconv(Steeporig[simno], phie0hist, R0orig[simno], SR=1, type = 1)
  } # end simno

  # Plot parameters
  cols <- pal_lancet("lanonc")(5)[1:3]

 g1 <- SRpars_iscam |>
    ggplot()+
    geom_density(aes(x=steepness, fill="iscam"), alpha=0.3)+
    geom_density(data=SRpars_om,aes(x=steepness, fill="OM"), alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("iscam"=cols[1], "OM"=cols[2]))+
    ggtitle("Steepness")
  g1

  g2 <- SRpars_iscam |>
    ggplot()+
    geom_density(aes(x=SRalpha, fill="iscam"), alpha=0.3)+
    geom_density(data=SRpars_om,aes(x=SRalpha, fill="OM"), alpha=0.3)+
    mytheme_paper+

    scale_fill_manual(name="Model",values=c("iscam"=cols[1], "OM"=cols[2]))+
    ggtitle("SRalpha")
  g2

  g3 <- SRpars_iscam |>
    ggplot()+
    geom_density(aes(x=SRbeta, fill="iscam"), alpha=0.3)+
    geom_density(data=SRpars_om,aes(x=SRbeta, fill="OM"), alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("iscam"=cols[1], "OM"=cols[2]))+
    ggtitle("SRbeta")
  g3




}





  # back-calculate om steepness from sr parameters and long-term mean M

  g5 <- mcmcout_df |>
    ggplot()+
    geom_density(aes(x=h, fill="iscam"), alpha=0.3)+
    geom_density(data=ompars,aes(x=h, fill="OM"), alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("iscam"=cols[1], "OM"=cols[2]))+
    ggtitle("Steepness")
  g5
  ggsave(here("Figures",paste0("Supp_compare_Steepness",stocks[j],".png")),
         width = 8, height = 5)

  g6 <- mcmcout_df |>
    ggplot()+
    geom_density(aes(x=m, fill="iscam"), alpha=0.3)+
    geom_density(data=ompars,aes(x=m, fill="OM"), alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("iscam"=cols[1], "OM"=cols[2]))+
    ggtitle("Baseline M")
  g6
  ggsave(here("Figures",paste0("Supp_compare_BaseM",stocks[j],".png")),
         width = 8, height = 5)

  g7 <- mcmcout_df |>
    ggplot()+
    geom_density(aes(x=bo, fill="iscam"), alpha=0.3)+
    geom_density(data=ompars,aes(x=bo, fill="OM"), alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("iscam"=cols[1], "OM"=cols[2]))+
    ggtitle("B0")
  g7
  ggsave(here("Figures",paste0("Supp_compare_B0",stocks[j],".png")),
         width = 8, height = 5)


  # Investigate what MSEtool is doing for its BaseM?
  mt_iscam_2023 <- mt[,73]
  mt_iscam_1951 <- mt[,1]

  mt_om_2023 <- getM(mse,"Historical", age=3, type="annual",
                     quant=FALSE, input_type="MSE")[,73]
  mt_om_1951 <- getM(mse,"Historical", age=3, type="annual",
                     quant=FALSE, input_type="MSE")[,1]

  mt_iscam_2023_df  <- mt_iscam_2023 |>
    as.data.frame() |>
    rename(m=mt_iscam_2023)
  mt_iscam_1951_df  <- mt_iscam_1951 |>
    as.data.frame() |>
    rename(m=mt_iscam_1951)

  mt_om_1951_df  <- mt_om_1951 |>
    as.data.frame() |>
    rename(m=mt_om_1951)
  mt_om_2023_df  <- mt_om_2023 |>
    as.data.frame() |>
    rename(m=mt_om_2023)

  g7 <- mcmcout_df |>
    ggplot()+
    geom_density(aes(x=m, fill="iscambase"), alpha=0.3)+
    geom_density(data=mt_iscam_1951_df,aes(x=m, fill="iscamM1"), alpha=0.3)+
    geom_density(data=mt_iscam_2023_df,aes(x=m, fill="iscamM73"), alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("iscambase"=cols[1], "iscamM1"=cols[2], "iscamM73"=cols[3]))+
    ggtitle("Baseline M")+
    xlim(0,2)+ylim(0,4)
  g7
  ggsave(here("Figures",paste0("Extra_compare_BaseM_iscam",stocks[j],".png")),
         width = 8, height = 5)

  g8 <- mcmcout_df |>
    ggplot()+
    geom_density(aes(x=m, fill="iscambase"), alpha=0.3)+
    geom_density(data=mt_iscam_2023_df,aes(x=m, fill="iscamM73"), alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("iscambase"=cols[1], "iscamM73"=cols[3]))+
    ggtitle("Baseline M")+
    xlim(0,2)+ylim(0,4)
  g8
  ggsave(here("Figures",paste0("Extra_compare_BaseM_iscam2023",stocks[j],".png")),
         width = 8, height = 5)

  g9 <- mcmcout_df |>
    ggplot()+
    geom_density(aes(x=m, fill="iscambase"), alpha=0.3)+
    geom_density(data=mt_iscam_1951_df,aes(x=m, fill="iscamM1"), alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("iscambase"=cols[1], "iscamM1"=cols[2]))+
    ggtitle("Baseline M")+
    xlim(0,2)+ylim(0,4)
  g9
  ggsave(here("Figures",paste0("Extra_compare_BaseM_iscam1951",stocks[j],".png")),
         width = 8, height = 5)

  # So iscam base m = iscam Mt in year 1
  g10 <- ompars |>
    ggplot()+
    geom_density(aes(x=m, fill="OMbase"), alpha=0.3)+
    geom_density(data=mt_om_1951_df,aes(x=m, fill="OM1"), alpha=0.3)+
    geom_density(data=mt_om_2023_df,aes(x=m, fill="OM73"), alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("OMbase"=cols[1], "OM1"=cols[2], "OM73"=cols[3]))+
    ggtitle("Baseline M")+
    xlim(0,2)+ylim(0,10)
  g10
  ggsave(here("Figures",paste0("Extra_compare_BaseM_OM",stocks[j],".png")),
         width = 8, height = 5)

  # No idea where OM is getting its baseline M from - ignore it and use Mt in
  # year 1 for the stock recruit pars
}
