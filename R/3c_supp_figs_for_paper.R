# Make supplemental figures for the paper
# January 11, 2026
# Robyn Forrest

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

  # Read in MSE object
  StockDirMSE   <- here("MSEs", paste(stocks[j]))
  mse <- readRDS(here(StockDirMSE, "hMSEs_NF.rda"))[[1]] # just take first scenario

  # Make iscam dataframes for plotting
  # Parameters
  mcmcout_df <- mcmcout |>
    select(ro_gr1,h_gr1,m_gs1, sbo) |>
    as.data.frame() |>
    rename(ro=ro_gr1,h=h_gr1,m=m_gs1, bo=sbo)

  nrep <- length(ro)

  #Put time series in long format for ggplot
  sbt_df <- sbt |>
    apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
    as.data.frame() |>
    mutate(year=syr:(nyr+1), Model="iscam") |>
    filter(year!=nyr+1) |>
    dplyr::rename(lwr=1, med=`50%`, upr=3) |>
    as.data.frame() |>
    select(year,lwr,upr,med,Model)

  rt_df <- rt |>
    apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
    as.data.frame() |>
    mutate(year=(syr+2):nyr, Model="iscam") |>
    filter(year!=nyr+1) |>
    dplyr::rename(lwr=1, med=`50%`, upr=3) |>
    as.data.frame()|>
    select(year,lwr,upr,med,Model)

  mt_df <- mt |>
    apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
    as.data.frame() |>
    mutate(year=syr:nyr, Model="iscam") |>
    filter(year!=nyr+1) |>
    dplyr::rename(lwr=1, med=`50%`, upr=3) |>
    as.data.frame()|>
    select(year,lwr,upr,med,Model)

  # Make OM dataframes for plotting
  sbt_om_df <- getSSB(mse,"Historical", mp=1) |>
    filter(year<=nyr) |>
    mutate(Model="OM")|>
    select(year,lwr,upr,med,Model)

  rt_om_df <- getRec(mse,"Historical", mp=1) |>
    filter(year>(syr+1))|>
    filter(year<=nyr)|>
    mutate(Model="OM")|>
    select(year,lwr,upr,med,Model)

  mt_om_df <- getM(mse,"Historical", age=3, type="annual", quant=TRUE, input_type="MSE") |>
    filter(year<=nyr) |>
    select(-scenario,-Mtype)|>
    mutate(Model="OM")|>
    select(year,lwr,upr,med,Model)

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

  ##################################################################################
  # Plot time series
  allsbt <- rbind(sbt_df,sbt_om_df)
  allrt <- rbind(rt_df,rt_om_df)
  allmt <- rbind(mt_df,mt_om_df)

  g1 <- allsbt |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=Model), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=Model, lty=Model),lwd=1.25)+
    #facet_wrap(vars(Model), nrow=1)+
    scale_x_continuous(breaks=seq(syr,nyr,5))+
    labs(x = "Year", y = "SSB", title= "")+
    scale_color_lancet()+
    scale_fill_lancet()+
    mytheme_paper+
    theme(legend.position = "right")
  g1
  ggsave(here("Figures",paste0("Supp_compare_SSB",stocks[j],".png")),
         width = 8, height = 5)

  g2 <- allrt |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=Model), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=Model, lty=Model),lwd=1.25)+
    scale_x_continuous(breaks=seq((syr+2),nyr,5))+
    labs(x = "Year", y = "Recruits", title= "")+
    scale_color_lancet()+
    scale_fill_lancet()+
    mytheme_paper+
    theme(legend.position = "right")
  g2
  ggsave(here("Figures",paste0("Supp_compare_Rec",stocks[j],".png")),
         width = 8, height = 5)

  g3 <- allmt |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=Model), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=Model, lty=Model),lwd=1.25)+
    scale_x_continuous(breaks=seq(syr,nyr,5))+
    labs(x = "Year", y = "M", title= "")+
    scale_color_lancet()+
    scale_fill_lancet()+
    mytheme_paper+
    theme(legend.position = "right")
  g3
  ggsave(here("Figures",paste0("Supp_compare_Mt",stocks[j],".png")),
         width = 8, height = 5)

  ##################################################################################
  # Plot parameters
  cols <- pal_lancet("lanonc")(5)[1:3]

  g4 <- mcmcout_df |>
    ggplot()+
    geom_density(aes(x=ro, fill="iscam"), alpha=0.3)+
    geom_density(data=ompars,aes(x=ro, fill="OM"), alpha=0.3)+
    mytheme_paper+
    scale_fill_manual(name="Model",values=c("iscam"=cols[1], "OM"=cols[2]))+
    ggtitle("R0")
  g4
  ggsave(here("Figures",paste0("Supp_compare_R0",stocks[j],".png")),
         width = 8, height = 5)

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
