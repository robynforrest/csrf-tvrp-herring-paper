# Make the figures for the paper (figs 5 and 7)
# January 3, 2026

# FIGURE 5. TIME SERIES OF M AND LRP UNDER ALTERNATIVE AVERAGING SCENARIOS
# This is a subset of the plots made in my csrf-tvrp-herring-pac repo in the file
#   2b_plot-pac-herring-mse-nf.R
# --- Load Base Herring OMs just to get the number and names of stocks and nsim --------------------
# In normal running, a lot of these objects will already exist
# because 2a_run-pac-herring-mse.R, 2b_plot-pac-herring-mse-nf.R
# and 2c_plot-pac-herring-mse.R are called together.
#  But these objects are needed here for dev.
hOMs <- readRDS(here("OMs/hOMs.rda"))
stocks <- names(hOMs)
nstocks <- length(stocks)
nsim <- hOMs[[1]]@nsim
histMSEs <- readRDS(here("MSEs/hist_hMSEs.rda"))
scenameHuman <- readRDS(here(SpDirOM, "ScenarioNamesHuman.rda"))

for(j in 1:nstocks){
  cat("~~~ Plotting Fig 4 for", paste(stocks[j]), "~~~\n")

  StockDirOM    <- here(SpDirOM, paste(stocks[j]))
  StockDirMSE   <- here(SpDirMSE, paste(stocks[j]))
  StockDirFigs  <- here(SpDirFigs, paste(stocks[j]))
  if(!file.exists(StockDirFigs)) dir.create(StockDirFigs, recursive=TRUE)
  StockDirFigs_NF <- here(StockDirFigs, "NF")
  if(!file.exists(StockDirMSE)) stop("Stop. No MSEs found. Please run 2_run-mse.R first. \n")
  if(!file.exists(StockDirFigs_NF)) dir.create(StockDirFigs_NF, recursive=TRUE)

  OMscenarios  <- readRDS(here(StockDirOM, "OMscenarios.rda"))
  MSEscenarios <- readRDS(here(StockDirMSE, "hMSEs_NF.rda"))
  histMSE <- histMSEs[j][[1]]
  nScenarios <- length(OMscenarios)
  cyr <- MSEscenarios[[1]]@OM$CurrentYr[1] # current year (2023)
  syr <- cyr-MSEscenarios[[1]]@nyears+1
  pyr <- cyr + 1 # get the first of the projection years (currently 2024)
  fyr <- cyr + pro_years #final year of projections
  stock <- stocks[j]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot time series of Spawning Biomass - only annual M applies here, this is the true stock size
  # 1. Plot SSB
  dat <- purrr::map2_df(MSEscenarios,scenameHuman, getSSB, mp=1) |>
    as.data.frame() |>
    mutate(group=factor(scenario, levels=scenameHuman))

  g <- ggplot(dat) +
    geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), fill=ssbcol, alpha = 0.1) +
    geom_line(aes(x=year,y=med), color=ssbcol, lwd=1.5) +
    facet_wrap(vars(group), nrow=1)+
    theme(legend.position = "none") +
    labs(x = "Year", y = "Spawning biomass", title= "")+
    geom_vline(xintercept=cyr, lty=3)+
    mytheme_lg #+
    #annotate("text", x=1953, y=maxY, label= "(a)")

  ggsave(file.path(StockDirFigs_NF, paste0("MSE-Biomass_allScen_allYear_NF.png")),
         width = 8, height = 5)
  ggsave(file.path(StockDirFigs, paste0("FIGURE4_MSE-SSB_allScen_allYear_NF.png")),
         width = 16, height = 10)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot time series of all alternative Ms
  M_levels <- c("hist", "mean", "recent","annual")
  histM <- purrr::map2_df(MSEscenarios,scenameHuman, getM, age=Mage, type="hist", quant=TRUE, input_type="MSE")
  meanM <- purrr::map2_df(MSEscenarios,scenameHuman, getM, age=Mage, type="mean", quant=TRUE, input_type="MSE")
  recentM <- purrr::map2_df(MSEscenarios,scenameHuman, getM,age=Mage, type="recent", quant=TRUE, input_type="MSE")
  annualM <- purrr::map2_df(MSEscenarios,scenameHuman, getM, age=Mage, type="annual", quant=TRUE, input_type="MSE")

  allM <- rbind(histM,meanM,recentM, annualM) |>
    mutate(`M type`=factor(Mtype, levels=M_levels)) |>
    select(-Mtype)

  write_csv(allM, file.path(StockDirFigs_NF, paste0("MSE-allM_allScen_allYear.csv")))

  # all years with uncertainty
  g <- allM |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`M type`), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=`M type`, lty=`M type`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=3)+
    scale_color_manual(values=manualcolors[2:5])+
    scale_fill_manual(values=manualcolors[2:5])+
    scale_linetype_manual(values = c("hist" = 2,
                                     "mean" = 4,
                                     "recent" =5,
                                     "annual" = 3))+
    facet_wrap(vars(group), nrow=1)+
    scale_x_continuous(breaks=seq(syr,fyr,16))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "M", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allM_allScen_allYear_NF.png")),
         width = 16, height = 10)
  ggsave(file.path(StockDirFigs, paste0("FIGURE4_MSE-allM_allScen_allYear_NF.png")),
         width = 16, height = 10)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot time series of all alternative B0-based reference points
  refpt_levels <- c("SSB","hist", "mean", "recent", "dyn")
  histB0 <- purrr::map2_df(MSEscenarios,scenameHuman, getmeanB0, age=Mage, type="hist", quants=TRUE)
  meanB0 <- purrr::map2_df(MSEscenarios,scenameHuman, getmeanB0, age=Mage, type="mean", quants=TRUE)
  recentB0 <- purrr::map2_df(MSEscenarios,scenameHuman, getmeanB0,age=Mage, type="recent", quants=TRUE)
  dynB0 <- purrr::map2_df(MSEscenarios,scenameHuman, getdynB0,quants=TRUE)
  SSBnf <- purrr::map2_df(MSEscenarios,scenameHuman, getSSB, mp=1) |>
    mutate(RefPtName="SSB") # dynamic B0 (from MSE object)# dynamic B0 for NF mp (from MSE object)
  allB0 <- rbind(histB0,meanB0,recentB0,dynB0,SSBnf) |>
    mutate(`B0 type`=factor(RefPtName, levels=refpt_levels)) |>
    select(-RefPtName)

  write_csv(allB0, file.path(StockDirFigs_NF, paste0("MSE-allB0_allScen_allYear_NF.csv")))

  # all years with uncertainty
  g <- allB0 |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`B0 type`), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=`B0 type`, lty=`B0 type`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=3)+
    scale_color_manual(values=manualcolors)+
    scale_fill_manual(values=manualcolors)+
    facet_wrap(vars(group), nrow=1)+
    scale_linetype_manual(values = c("SSB"=1,
                                     "hist" = 2,
                                     "mean" = 4,
                                     "recent" =5,
                                     "dyn" = 3))+
    scale_x_continuous(breaks=seq(syr,fyr,16))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "SSB or B0", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allB0_allScen_allYear_NF.png")),
         width = 16, height = 10)
  ggsave(file.path(StockDirFigs, paste0("FIGURE4_MSE-allB0_allScen_allYear_NF.png")),
         width = 16, height = 10)

  # FIGURE 5. NO FISHING SCENARIO WITH ALTERNATIVE LRPS - BIOMASS AND PROBABILITIES
  #   #########################################################################################################
  cat("~~~ Plotting Fig 5 for", paste(stocks[j]), "~~~\n")
  # Now plot all years again as LRP (0.3B0)
  # multiply all ref points by 0.3
  LRP <- allB0 |>
    filter(!`B0 type` %in% "SSB") |>
    mutate(lwr=0.3*lwr, med=0.3*med, upr=0.3*upr)

  # add back to df with SSB
  LRP <- allB0 |>
    filter(`B0 type` %in% "SSB") |>
    rbind(LRP)

  LRP2023 <- LRP |>
    filter(year==cyr, `B0 type`=="mean")

  write_csv(allLRP_quants, file.path(StockDirFigs_NF, paste0("MSE-allLRP_allScen_allYear_NF.csv")))

  # Pro years only
  g <- LRP |>
    filter(year>cyr) |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`B0 type`), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=`B0 type`, lty=`B0 type`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=2)+
    geom_pointrange(data=LRP2023,aes(x=cyr ,y=med, ymin=lwr , ymax=upr), colour=meancol, lwd=2, size=3)+
    scale_color_manual(values=manualcolors)+
    scale_fill_manual(values=manualcolors)+
    scale_linetype_manual(values = c("SSB"=1,
                                     "hist" = 2,
                                     "mean" = 4,
                                     "recent" =5,
                                     "dyn" = 3))+
    facet_wrap(vars(group), nrow=1)+
    scale_x_continuous(breaks=seq(cyr+1,fyr,6))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg+
    theme(legend.position = "right")
  g
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allLRP_allScen_proYear_NF.png")),
         width = 16, height = 10)
  ggsave(file.path(StockDirFigs, paste0("FIGURE5_MSE-allLRP_proYear_NF.png")),
         width = 16, height = 10)

  # # Plot P(B > 0.3B0)
  # # Make 5 panel plot with probs
  # # Show conservation performance metrics
  PLRPhistB0   <- purrr::map2_df(MSEscenarios,scenameHuman, getPLRP_B0, age=Mage,type="hist", mp=1) |>
    mutate(MP="NFref")
  PLRPmeanB0   <- purrr::map2_df(MSEscenarios,scenameHuman, getPLRP_B0, age=Mage,type="mean", mp=1)|>
    mutate(MP="NFref")
  PLRPrecentB0 <- purrr::map2_df( MSEscenarios,scenameHuman, getPLRP_B0, age=Mage,type="recent", mp=1)|>
    mutate(MP="NFref")
  PLRPdynB0    <- purrr::map2_df(MSEscenarios,scenameHuman, getPLRP_dynB0, mp=1)|>
    mutate(MP="NFref")

  PLRPB0_NF <- rbind(PLRPhistB0, PLRPmeanB0, PLRPrecentB0, PLRPdynB0) |>
    mutate(`B0 type`=factor(b0type, levels=c("hist", "mean", "recent", "dyn"))) |>
    select(-b0type)

  write_csv(PLRPB0_NF,file.path(StockDirFigs_NF, "PLRP_Metrics_B0_NF.csv"))

  # Plot all B0 performance metrics on one plot
  g <- plotPLRP(PLRPB0_NF,
                scentext=TRUE,
                panel=FALSE)
  g
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-PLRP_B0_allScen_NF.png")),
         width = 16, height = 10)
  ggsave(file.path(StockDirFigs, paste0("FIGURE5_MSE-PLRP_B0_allScen_NF.png")),
         width = 16, height = 10)

  # ##############################################################
  # Plot relative SSB and dynamicB0 (Like Berger 2019 Fig)  FOR SUPP MAYBE
  cat("~~~ Plotting supplemental dynamic B0 fig for", paste(stocks[j]), "~~~\n")
  dynB0_rel <- purrr::map2_df(MSEscenarios,scenameHuman, getdynB0rel) |>
    dplyr::select(year, scenario, med) |>
    rename(dynB0=med)
  SSB_rel <- purrr::map2_df(MSEscenarios,scenameHuman, getSSBrel, mp=1) |>
    dplyr::select(year, scenario, med) |>
    rename(SSB=med)
  relSSBdynB0 <- left_join(SSB_rel, dynB0_rel) |>
    melt(id.vars= c("year", "scenario")) |>
    rename(refpt=variable) |>
    mutate(group=factor(scenario, levels=scenameHuman))

  Ymax <- max(relSSBdynB0$value)
  Ymaxpro <- max(relSSBdynB0[which(relSSBdynB0$year>cyr),]$value)

  # all years
  g <- relSSBdynB0 |>
    ggplot() +
    geom_line(aes(x=year,y=value, colour=refpt, lty=refpt), lwd=2) +
    geom_vline(xintercept = cyr, lty=3) +
    facet_wrap(~group, ncol=2)+
    scale_color_manual(values=c(ssbcol, dyncol)) +
    labs(x = "Year", y = "", title="")+
    mytheme+
    ylim(0,Ymax)
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-DynB0_v_SSB_allScen_allYear_NF.png")),
         width = 8, height = 5)
  ggsave(file.path(StockDirFigs, paste0("SUPPFIG_MSE-DynB0_v_SSB_allScen_allYear_NF.png")),
         width = 8, height = 5)




} # end for j in stocks

# ====================================================================================================================
# ======= END OF SCRIPT ==============================================================================================
# ====================================================================================================================






