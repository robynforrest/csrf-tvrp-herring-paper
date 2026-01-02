# ===================================================================================================================================================
# === 1c Plot the historical time series and M scenarios  =============================================================================================
# === Sourced by 1_make-pac-herring-oms.R  ===========================================================================
# ===================================================================================================================================================

# Reload the historical MSEs.
# The OM scenarios will be loaded in the loop since
#  there is a list of scenarios for each stock
hist_MSEs <- readRDS(here(SpDirMSE, "hist_hMSEs.rda"))
ScenarioNamesHuman <- readRDS(here(SpDirOM, "ScenarioNamesHuman.rda"))

stocks <- names(hist_MSEs)
nstocks <- length(stocks)

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
    # All scenarios
    Mtout <- purrr::map2_df(OMscenarios, ScenarioNamesHuman, getM, age=Mage, type="annual", quant=TRUE, input_type="OM") %>%
      mutate(Scenario = factor(scenario, levels = ScenarioNamesHuman)) %>%
      as.data.frame()
    write_csv(Mtout, file=file.path(StockDirFigs, paste0("OM-M_All_M_scenarios_",stocks[j],".csv")))

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


    # Facet
    g <- purrr::map2_df(OMscenarios, ScenarioNamesHuman, getM, age=Mage, type="annual",quant=TRUE, input_type="OM") %>%
      mutate(scenario = factor(scenario, levels = ScenarioNamesHuman)) %>%
      as.data.frame() %>%
      ggplot() +
      geom_ribbon(aes(x=year, ymin=lwr, ymax=upr, fill=scenario), alpha = 0.3) +
      #ylim(0,3.)+
      geom_line(aes(x=year,y=med, color=scenario), lwd=1) +
      geom_vline(xintercept=pyr1, lty=3)+
      scale_fill_startrek()+  # ggsci package
      scale_color_startrek()+
      facet_wrap(vars(scenario))+
      gfplot::theme_pbs() +
      labs(x = "Year", y = "M")+
      mytheme+
      theme(legend.position = "bottom")
      ggsave(file.path(StockDirFigs, paste0("OM-M_All_M_scenarios_",stocks[j],"_Facet.png")))

    # 3. Rec deviations
    g <- purrr::map2_df(OMscenarios,ScenarioNamesHuman, getperry) %>%
      as.data.frame() %>%
      mutate(group=factor(scenario, levels=ScenarioNamesHuman)) %>%
      ggplot() +
      geom_pointrange(aes(x=year, y=log(med),ymin=log(lwr), ymax=log(upr), color=scenario)) +
      geom_line(aes(x=year,y=log(med), color=scenario), lwd=0.25, lty=1) +
      geom_hline(yintercept=0, linetype="dashed", color = 1, linewidth=1)+
      geom_vline(xintercept=cyr, linetype=3, color = 1, linewidth=1)+
      facet_grid(group~.)+
      gfplot::theme_pbs() +
      scale_fill_startrek()+  # ggsci package
      scale_color_startrek()+
      labs(x = "Year", y = "", title="Log recruitment deviations")+
      theme(plot.title = element_text(face="bold", size=20),
          axis.title.x = element_text(size=16,face="bold"),
          axis.text.y = element_text(size=12,face="bold"),
          axis.text.x = element_text(size=12,face="bold"),
          strip.text.y = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"))
    ggsave(file.path(StockDirFigs, paste0("OM-Recdevs.png")),
       width = 12, height = 7.5)

    g <- purrr::map2_df(OMscenarios,ScenarioNamesHuman, getperry) %>%
      as.data.frame() %>%
      filter(year>=pyr1) %>%
      mutate(group=factor(scenario, levels=ScenarioNamesHuman)) %>%
      ggplot() +
      geom_pointrange(aes(x=year, y=log(med),ymin=log(lwr), ymax=log(upr), color=scenario)) +
      geom_line(aes(x=year,y=log(med), color=scenario), lwd=0.5, lty=1) +
      geom_vline(xintercept = pyr1, lty=3) +
      geom_hline(yintercept=0, linetype="dashed", color = 1, linewidth=1)+
      facet_grid(group~.)+
      gfplot::theme_pbs() +
      scale_fill_startrek()+  # ggsci package
      scale_color_startrek()+
      labs(x = "Year", y = "", title="Projected log recruitment deviations")+
      theme(plot.title = element_text(face="bold", size=20),
          axis.title.x = element_text(size=16,face="bold"),
          axis.text.y = element_text(size=12,face="bold"),
          axis.text.x = element_text(size=12,face="bold"),
          strip.text.y = element_blank(),
          legend.text = element_text(size=12),
          legend.title = element_text(size=12, face="bold"))
    ggsave(file.path(StockDirFigs, paste0("OM-Recdevs_pro.png")),
       width = 16, height = 10)
} #end j
