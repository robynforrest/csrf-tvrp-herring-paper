# ===================================================================================================================================================
# === Pacific Herring: 3a Plot the MSE results for the No Fishing MP  =============================================================================================
# === Sourced by 2_run-mse.R  ===========================================================================
# ===================================================================================================================================================

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

#1:nstocks
for(j in 3){
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
  cyr <- MSEscenarios[[1]]@OM$CurrentYr[1] # current year (2019)
  syr <- cyr-MSEscenarios[[1]]@nyears+1
  pyr <- cyr + 1 # get the first of the projection years (currently 2020)
  fyr <- cyr + pro_years #final year of projections
  stock <- stocks[j]

#=================================================================================================================
#========= ~ MAKE PLOTS FOR NO FISHING MP ~ ======================================================================
#=================================================================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Plot SSB
  g <- purrr::map2_df(MSEscenarios,scenameHuman, getSSB, mp=1) |>
    as.data.frame() |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot() +
    geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), fill=ssbcol, alpha = 0.1) +
    geom_line(aes(x=year,y=med), color=ssbcol, lwd=1) +
    facet_wrap(vars(group), nrow=2)+
    theme(legend.position = "none") +
    labs(x = "Year", y = "Spawning biomass", title= "SSB (MP = NF)")+
    geom_vline(xintercept=cyr, lty=3)+
    mytheme_lg
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-Biomass_allScen_allYear_NF.png")),
         width = 8, height = 5)

  # historical only
  g <- purrr::map2_df(MSEscenarios,scenameHuman, getSSB, mp=1) |>
    filter(year < pyr, scenario %in% scenameHuman[1]) |>
    as.data.frame() |>
    ggplot() +
    geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), fill=ssbcol, alpha = 0.1) +
    geom_line(aes(x=year,y=med), color=ssbcol, lwd=1) +
    mytheme_lg+
    theme(legend.position = "none") +
    labs(x = "Year", y = "Spawning biomass", title= "")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-Biomass_allScen_histYear.png")),
         width = 8, height = 5)

  # 2. Plot M and SSB - colour these by Scenario
  g1 <- purrr::map2_df(OMscenarios,scenameHuman, getM, age=Mage, type="annual", quant=TRUE, input_type="OM") |>
    as.data.frame() |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot() +
    geom_ribbon(aes(x=year, ymin=lwr, ymax=upr),fill=mortcol, alpha = 0.1) +
    geom_line(aes(x=year,y=med), color=mortcol, lwd=1) +
    facet_grid(vars(group))+
    mytheme_lg+
    theme(strip.text.y = element_text(size=12))+
    theme(legend.position = "none") +
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "",  title= "M")

  g2 <- purrr::map2_df(MSEscenarios,scenameHuman, getSSB, mp=1) |>
    as.data.frame() |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot() +
    geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), fill=ssbcol, alpha = 0.1) +
    geom_line(aes(x=year,y=med), color=ssbcol, lwd=1) +
    facet_grid(vars(group))+
    mytheme_lg+
    theme(strip.text.y = element_text(size=12))+
    theme(legend.position = "none") +
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "", title= "SSB (MP = NF)")

  cowplot::plot_grid(g1,g2, ncol=2)
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-Biomass_M_allScen_allYear_NF.png")),
         width = 8, height = 5)

  # proyears only
  g1 <- purrr::map2_df(OMscenarios,scenameHuman, getM, age=Mage, type="annual",quant=TRUE, input_type="OM") |>
    filter(year>cyr) |>
    as.data.frame() |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot() +
    geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), fill=mortcol, alpha = 0.1) +
    geom_line(aes(x=year,y=med), color=mortcol, lwd=1) +
    facet_grid(vars(group))+
    mytheme_lg+
    theme(strip.text.y = element_text(size=12))+
    theme(axis.text.x = element_text(size=10))+
    theme(legend.position = "none") +
    scale_x_continuous(breaks=seq(cyr+1,fyr,4))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "",  title= "M")

  g2 <- purrr::map2_df(MSEscenarios,scenameHuman, getSSB, mp=1) |>
    filter(year>cyr) |>
    as.data.frame() |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot() +
    geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), fill=ssbcol, alpha = 0.1) +
    geom_line(aes(x=year,y=med), color=ssbcol, lwd=1) +
    facet_grid(vars(group))+
    mytheme_lg+
    theme(strip.text.y = element_text(size=12))+
    theme(axis.text.x = element_text(size=10))+
    theme(legend.position = "none") +
    scale_x_continuous(breaks=seq(cyr+1,fyr,4))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "", title= "SSB (MP = NF)")

  cowplot::plot_grid(g1,g2, ncol=2)
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-Biomass_M_allScen_proYear_NF.png")),
         width = 8, height = 5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Plot time series of all alternative B0-based reference points
  refpt_levels <- c("SSB","hist", "mean", "recent", "dyn")
  histB0 <- purrr::map2_df(MSEscenarios,scenameHuman, getmeanB0, age=Mage, type="hist", quants=TRUE)
  meanB0 <- purrr::map2_df(MSEscenarios,scenameHuman, getmeanB0, age=Mage, type="mean", quants=TRUE)
  recentB0 <- purrr::map2_df(MSEscenarios,scenameHuman, getmeanB0,age=Mage, type="recent", quants=TRUE)
  dynB0 <- purrr::map2_df(MSEscenarios,scenameHuman, getdynB0,quants=TRUE)
  SSBnf <- purrr::map2_df(MSEscenarios,scenameHuman, getSSB, mp=1) |>
    mutate(RefPtName="SSB") # dynamic B0 (from MSE object)# dynamic B0 for NF mp (from MSE object)
  allB0 <- rbind(histB0,meanB0,recentB0,dynB0,SSBnf) |>
    mutate(`Ref. Pt`=factor(RefPtName, levels=refpt_levels)) |>
    select(-RefPtName)

  write_csv(allB0, file.path(StockDirFigs_NF, paste0("MSE-allB0_allScen_allYear_NF.csv")))

  # all years with uncertainty
  g <- allB0 |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`Ref. Pt`), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=3)+
    scale_color_manual(values=manualcolors)+
    scale_fill_manual(values=manualcolors)+
    facet_wrap(vars(group), nrow=1)+
    scale_x_continuous(breaks=seq(syr,fyr,16))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allB0_allScen_allYear_NF.png")),
         width = 16, height = 10)

  # pro years with uncertainty
  g <- allB0 |>
    filter(year>cyr) |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`Ref. Pt`), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=2)+
    scale_color_manual(values=manualcolors)+
    scale_fill_manual(values=manualcolors)+
    facet_wrap(vars(group), nrow=1)+
    scale_x_continuous(breaks=seq(cyr+1,fyr,6))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allB0_allScen_proYear_NF.png")),
         width = 16, height = 10)

  # Historical only
  g <- allB0  |>
    filter(year<=cyr,
           scenario==scenameHuman[1]) |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`Ref. Pt`), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=3)+
    scale_color_manual(values=manualcolors)+
    scale_fill_manual(values=manualcolors)+
    scale_x_continuous(breaks=seq(syr,cyr,10))+
    labs(x = "Year", y = "Reference point", title= "")+
    mytheme_lg+
    theme(legend.position = "bottom")
   ggsave(file.path(StockDirFigs_NF, paste0("MSE-allB0_Hist.png")),
         width = 16, height = 10)

   # Aug 2025 For comparison with real assessments, plot 2017 to 2023 (median only)
   repfile_years <- 2017:2023
   g <- allB0  |>
     filter(year %in% repfile_years,
            scenario==scenameHuman[1]) |>
     ggplot()+
     geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`Ref. Pt`), alpha = 0.1)+
     geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
     geom_vline(xintercept=cyr, lty=3)+
     scale_color_manual(values=manualcolors)+
     scale_fill_manual(values=manualcolors)+
     scale_x_continuous(breaks=repfile_years)+
     labs(x = "Year", y = "Reference point", title= "")+
     mytheme_lg+
     theme(legend.position = "bottom")
   ggsave(file.path(StockDirFigs_NF, paste0("MSE-allB0_Hist_2017-2023.png")),
          width = 16, height = 10)

 # Now plot all years again as LRP (0.3B0)
  # multiply all ref points by 0.3
  LRP <- allB0 |>
    filter(!`Ref. Pt` %in% "SSB") |>
    mutate(lwr=0.3*lwr, med=0.3*med, upr=0.3*upr)

  # add back to df with SSB
  LRP <- allB0 |>
    filter(`Ref. Pt` %in% "SSB") |>
    rbind(LRP)

  # All years
  g <- LRP |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`Ref. Pt`), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=2)+
    scale_color_manual(values=manualcolors)+
    scale_fill_manual(values=manualcolors)+
    facet_wrap(vars(group), nrow=1)+
    scale_x_continuous(breaks=seq(syr,fyr,20))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allLRP_allScen_allYears_NF.png")),
         width = 16, height = 10)

  # Pro years only
  g <- LRP |>
    filter(year>cyr) |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`Ref. Pt`), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=2)+
    scale_color_manual(values=manualcolors)+
    scale_fill_manual(values=manualcolors)+
    facet_wrap(vars(group), nrow=1)+
    scale_x_continuous(breaks=seq(cyr+1,fyr,6))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allLRP_allScen_proYear_NF.png")),
         width = 16, height = 10)

  # Historical years only
  g <- LRP |>
    filter(year<=cyr,
           scenario==scenameHuman[1]) |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`Ref. Pt`), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    scale_color_manual(values=manualcolors)+
    scale_fill_manual(values=manualcolors)+
    scale_x_continuous(breaks=seq(syr,cyr,11))+
    labs(x = "Year", y = "Reference point", title= "")+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allLRP_Hist.png")),
         width = 16, height = 10)

  # zoom in on iscam report files years
  g <- LRP |>
    filter(year %in% repfile_years,
           scenario==scenameHuman[1]) |>
    ggplot()+
    geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`Ref. Pt`), alpha = 0.1)+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=2)+
    scale_color_manual(values=manualcolors)+
    scale_fill_manual(values=manualcolors)+
    scale_x_continuous(breaks=repfile_years)+
    labs(x = "Year", y = "Reference point", title= "")+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allLRP_Hist_2017-2023.png")),
         width = 16, height = 10)

    # REPEAT BUT WITH MEDIAN ONLY
  # all years med
  g <- allB0 |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot()+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=3)+
    scale_color_manual(values=manualcolors)+
    facet_wrap(vars(group), nrow=1)+
    scale_x_continuous(breaks=seq(syr,fyr,16))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Median reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allB0_allScen_allYear_NF-med.png")),
         width = 16, height = 10)

  # pro years med
  g <- allB0 |>
    filter(year>cyr) |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot()+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=2)+
    scale_color_manual(values=manualcolors)+
    facet_wrap(vars(group), nrow=1)+
    scale_x_continuous(breaks=seq(cyr+1,fyr,6))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Median reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allB0_allScen_proYear_NF-med.png")),
         width = 16, height = 10)

  # Historical only
  g <- allB0  |>
    filter(year <= cyr,
           scenario==scenameHuman[1]) |>
    ggplot()+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=3)+
    scale_color_manual(values=manualcolors)+
    scale_x_continuous(breaks=seq(syr,cyr,10))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Median reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allB0_Hist-med.png")),
         width = 16, height = 10)

  # repfile years (available iscam report files)
  g <- allB0 |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    filter(year %in% repfile_years,
           scenario==scenameHuman[1]) |>
    ggplot()+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=3)+
    scale_color_manual(values=manualcolors)+
    scale_x_continuous(breaks=seq(min(repfile_years), max(repfile_years),1))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Median reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allB0_Hist_2017-2023-med.png")),
         width = 16, height = 10)

  # Now LRP medians
  # Aug 2025 For comparison with real assessments, plot 2017 to 2023 (median only)
  # Also add plot for just historical years

  # All years
  g <- LRP |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot()+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=2)+
    scale_color_manual(values=manualcolors)+
    facet_wrap(vars(group), nrow=1)+
    scale_x_continuous(breaks=seq(syr,fyr,20))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Median reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allLRP_allScen_allYear_NF-med.png")),
         width = 16, height = 10)

  # pro years
  g <- LRP |>
    filter(year>cyr) |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    ggplot()+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=2)+
    scale_color_manual(values=manualcolors)+
    facet_wrap(vars(group), nrow=1)+
    scale_x_continuous(breaks=seq(cyr+1,fyr,6))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Median reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allLRP_allScen_proYear_NF-med.png")),
         width = 16, height = 10)

  # Historical only
  g <- LRP  |>
    filter(year <= cyr,
           scenario==scenameHuman[1]) |>
  ggplot()+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=3)+
    scale_color_manual(values=manualcolors)+
    scale_x_continuous(breaks=seq(syr,cyr,10))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Median reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allLRP_Hist-med.png")),
         width = 16, height = 10)

  # repfile years (available iscam report files)
    g <- LRP |>
    mutate(group=factor(scenario, levels=scenameHuman)) |>
    filter(year %in% repfile_years,
           scenario==scenameHuman[1]) |>
    ggplot()+
    geom_line(aes(x=year, y=med, col=`Ref. Pt`, lty=`Ref. Pt`),lwd=1.5)+
    geom_vline(xintercept=cyr, lty=3)+
    scale_color_manual(values=manualcolors)+
    scale_x_continuous(breaks=seq(min(repfile_years), max(repfile_years),1))+
    geom_vline(xintercept=cyr, lty=3)+
    labs(x = "Year", y = "Median reference point", title= "")+
    theme(panel.spacing = unit(1, "lines"))+
    mytheme_lg+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-allLRP_Hist_2017-2023-med.png")),
         width = 16, height = 10)

# ##############################################################
# 4. Plot relative SSB and dynamicB0 (Like Berger 2019 Fig)
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

  # proj years
  g <- relSSBdynB0 |>
    dplyr::filter(year >= pyr) |>
    ggplot() +
    geom_line(aes(x=year,y=value, colour=refpt, lty=refpt), lwd=2) +
    geom_vline(xintercept = cyr, lty=3) +
    facet_wrap(~group, ncol=2)+
    scale_color_manual(values=c(ssbcol, dyncol)) +
    labs(x = "Year", y = "", title="")+
    mytheme+
    ylim(0,Ymaxpro)
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-DynB0_v_SSB_allScen_proYear_NF.png")),
         width = 8, height = 5)

 #########################################################################################################
#5.  Plot asymptotic reference points vs M
  # Need to pick an age to plot (last argument)
  # For PAC stocks as M is constant across ages > 1 (age index > 2)
   for(k in 1:nScenarios){
    scename <- OMscenarios[[k]]@Name

    g <- plotasymRefPts_allsims(OMscenarios[[k]],
                         MSEscenarios[[k]],
                         scename[k],
                         scenameHuman[k],
                         age=Mage)
    g
    ggsave(file.path(StockDirFigs_NF, paste0("MSE-MvRefpts_All_Sims_", scename,"_NF.png")),
           width = 16, height = 10)

    g <- plotasymRefPts_onesim(OMscenarios[[k]],
                                MSEscenarios[[k]],
                                scename[k],
                                scenameHuman[k],
                                age=Mage,
                                sim=2)
    g
    ggsave(file.path(StockDirFigs_NF, paste0("MSE-MvRefpts_One_Sim_", scename,"_NF.png")),
           width = 16, height = 10)
  }# end Scenarios loop

#########################################################################################################
# 5. Plot P(B > 0.3B0)
  # Make 5 panel plot with probs
  # Show conservation performance metrics
  PLRPhistB0   <- purrr::map2_df(MSEscenarios,scenameHuman, getPLRP_B0, age=Mage,type="hist", mp=1) |>
    mutate(MP=MPs[1])
  PLRPmeanB0   <- purrr::map2_df(MSEscenarios,scenameHuman, getPLRP_B0, age=Mage,type="mean", mp=1)|>
    mutate(MP=MPs[1])
  PLRPrecentB0 <- purrr::map2_df( MSEscenarios,scenameHuman, getPLRP_B0, age=Mage,type="recent", mp=1)|>
    mutate(MP=MPs[1])
  PLRPdynB0    <- purrr::map2_df(MSEscenarios,scenameHuman, getPLRP_dynB0, mp=1)|>
    mutate(MP=MPs[1])

  PLRPB0_NF <- rbind(PLRPhistB0, PLRPmeanB0, PLRPrecentB0, PLRPdynB0)

  write_csv(PLRPB0_NF,file.path(StockDirFigs_NF, "PLRP_Metrics_B0_NF.csv"))

  # Plot all B0 performance metrics on one plot
  g <- plotPLRP(PLRPB0_NF,
                scentext=TRUE,
                panel=FALSE)
  ggsave(file.path(StockDirFigs_NF, paste0("MSE-PLRP_B0_allScen_NF.png")),
            width = 16, height = 10)

  # Repeat with each metric on its own panel
  g <- plotPLRP(PLRPB0_NF,
                scentext=TRUE,
                panel=TRUE)
    ggsave(file.path(StockDirFigs_NF, paste0("MSE-PLRP_B0_allScen_panel_NF.png")),
             width = 16, height = 10)

} # end for j in stocks

# ====================================================================================================================
# ======= END OF SCRIPT ==============================================================================================
# ====================================================================================================================

