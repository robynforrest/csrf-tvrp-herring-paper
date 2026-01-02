# Make the figures for the paper
# January 2, 2026
# Reload the historical MSEs.
# The OM scenarios will be loaded in the loop
hist_MSEs <- readRDS(here(SpDirMSE, "hist_hMSEs.rda"))
ScenarioNamesHuman <- readRDS(here(SpDirOM, "ScenarioNamesHuman.rda"))
stocks <- names(hist_MSEs)
nstocks <- length(stocks)

# FIGURE 2. TIME SERIES OF HISTORICAL ANC PROJECTED SSB AND M (NO FISHING MP)
for(j in 1:nstocks){
  cat("~~~ Plotting OM figs for", paste(stocks[j]), "~~~\n")

  nsim<-hist_MSEs[[j]]@OM@nsim
  yind<-hist_MSEs[[j]]@OM@nyears+(1:hist_MSEs[[j]]@OM@proyears)

  # Get the OMs with alternative M scenarios
  OMscenarios <- readRDS(here(SpDirOM, paste(stocks[j]),"OMScenarios.rda"))
  pyr1 <- OMscenarios[[1]]@CurrentYr + 1 # get the first of the projection years (currently 2020)
  cyr <- OMscenarios[[1]]@CurrentYr
  syr <- cyr-OMscenarios[[1]]@nyears+1
  ScenarioNames <- names(OMscenarios)
  nsc <- length(ScenarioNames)

  # Make a directory for the figures
  StockDirFigs <- here::here(SpDirFigs, paste(stocks[j]))
  if(!file.exists(StockDirFigs)) dir.create(StockDirFigs, recursive=TRUE)

  #~~~~~~~~~~~PLOT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot the M time series
  Mtout <- purrr::map2_df(OMscenarios, ScenarioNamesHuman, getM, age=Mage, type="annual", quant=TRUE, input_type="OM") %>%
    mutate(Scenario = factor(scenario, levels = ScenarioNamesHuman)) %>%
    as.data.frame()
  write_csv(Mtout, file=file.path(StockDirFigs, paste0("OM-M_All_M_scenarios_",stocks[j],".csv")))


  traces <- Mscenario@cpars$M_ageArray[traceSample,3,] %>% t() %>%
    as.data.frame() %>%
    mutate(years=all_years) %>%
    rename(trace1=V1, trace2=V2,trace3=V3)

  g <- purrr::map2_df(OMscenarios, ScenarioNamesHuman, getM, age=Mage, type="annual",quant=TRUE, input_type="OM") %>%
    mutate(scenario = factor(scenario, levels = ScenarioNamesHuman)) %>%
    as.data.frame() %>%
    ggplot() +
    geom_ribbon(aes(x=year, ymin=lwr, ymax=upr, fill=scenario), alpha = 0.3) +
    geom_line(aes(x=year,y=med, color=scenario), lwd=1) +
    geom_vline(xintercept=pyr1, lty=3)+
    scale_fill_startrek()+  # ggsci package
    scale_color_startrek()+
    #ylim(0,3.)+
    gfplot::theme_pbs() +
    labs(x = "Year", y = "M")+
    mytheme+
    theme(legend.position = "bottom")
  ggsave(file.path(StockDirFigs, paste0("OM-M_All_M_scenarios_",stocks[j],".png")),
         width = 8, height = 5)

}

# FIGURE 3.ANALYTICAL RELATIONSHIP BETWEEN M AND B0


# FIGURE 4. STOCK-RECRUIT CURVE WITH ALTERNATIVE B0, R0 AND REPLACEMENT LINES


# FIGURE 5. TIME SERIES OF M AND LRP UNDER ALTERNATIVE AVERAGING SCENARIOS


# FIGURE 6. SURPLUS PRODUCTION VS SSB


# FIGURE 7. NO FISHING SCENARIO WITH ALTERNATIVE LRPS - BIOMASS AND PROBABILITIES


# SUPP:
# APPENDIX A. OM SETTINGS
# APPENDIX B. FITS TO AGE COMPS, COMPARISON OF M, RECRUITS AND BIOMASS (ISCAM VS OPENMSE)
