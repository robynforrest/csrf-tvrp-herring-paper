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
    proc_err <- OMscenarios[[1]]@cpars$Perr

    # Make a directory for the figures
    StockDirFigs <- here::here(SpDirFigs, paste(stocks[j]))
    if(!file.exists(StockDirFigs)) dir.create(StockDirFigs, recursive=TRUE)

    #~~~~~~~~~~~PLOT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Plot the M time series
    Mtout <- purrr::map2_df(OMscenarios, ScenarioNamesHuman, getM, age=Mage, type="annual", quant=TRUE, input_type="OM")  |>
      mutate(Scenario = factor(scenario, levels = ScenarioNamesHuman)) |>
      as.data.frame()
    write_csv(Mtout, file=file.path(StockDirFigs, paste0("OM-M_All_M_scenarios_",stocks[j],".csv")))

    g <- purrr::map2_df(OMscenarios, ScenarioNamesHuman, getM, age=Mage, type="annual",quant=TRUE, input_type="OM") |>
      mutate(scenario = factor(scenario, levels = ScenarioNamesHuman)) |>
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

    # 2. Log rec devs
    meanlogdev <- purrr::map2_df(OMscenarios,ScenarioNamesHuman, getlogperry_mean, proc_err)|>
      as.data.frame()
    quantlogdev <- purrr::map2_df(OMscenarios,ScenarioNamesHuman, getlogperry, proc_err) |>
      as.data.frame()
    g <- left_join(quantlogdev,meanlogdev) |>
      mutate(group=factor(scenario, levels=ScenarioNamesHuman)) |>
      filter(scenario==ScenarioNamesHuman[1], year>=syr) |>
      ggplot() +
      geom_pointrange(aes(x=year, y=med,ymin=lwr, ymax=upr), color=2) +
      #geom_point(aes(x=year, y=mean), color=1, shape=15) +
      geom_line(aes(x=year,y=med), color=2, lwd=0.25, lty=1) +
      geom_hline(yintercept=0, linetype="dashed", color = 1, linewidth=1)+
      geom_vline(xintercept=cyr, linetype=3, color = 1, linewidth=0.5)+
      gfplot::theme_pbs() +
      scale_fill_startrek()+  # ggsci package
      scale_color_startrek()+
      labs(x = "Year", y = "Log_recruitment deviations", title="")+
      theme(plot.title = element_text(face="bold", size=20),
            axis.title.x = element_text(size=16,face="bold"),
            axis.title.y = element_text(size=16,face="bold"),
            axis.text.y = element_text(size=12,face="bold"),
            axis.text.x = element_text(size=12,face="bold"),
            strip.text.y = element_blank(),
            legend.text = element_text(size=12),
            legend.title = element_text(size=12, face="bold"))
    g
    ggsave(file.path(StockDirFigs, paste0("Supp_OM-LogRecdevs",stocks[j],".png")),
           width = 12, height = 7.5)

    # Pro years only
    # 2. Log rec devs
    g <- left_join(quantlogdev,meanlogdev) |>
      mutate(group=factor(scenario, levels=ScenarioNamesHuman)) |>
      filter(scenario==ScenarioNamesHuman[1], year>=pyr1) |>
      mutate(group=factor(scenario, levels=ScenarioNamesHuman)) |>
      ggplot() +
      geom_pointrange(aes(x=year, y=med,ymin=lwr, ymax=upr), color=2) +
      #geom_point(aes(x=year, y=mean), color=1, shape=15) +
      geom_line(aes(x=year,y=med), color=2, lwd=0.25, lty=1) +
      geom_hline(yintercept=0, linetype="dashed", color = 1, linewidth=1)+
      geom_vline(xintercept=cyr, linetype=3, color = 1, linewidth=0.5)+
      gfplot::theme_pbs() +
      scale_fill_startrek()+  # ggsci package
      scale_color_startrek()+
      labs(x = "Year", y = "Log recruitment deviations", title="")+
      theme(plot.title = element_text(face="bold", size=20),
            axis.title.x = element_text(size=16,face="bold"),
            axis.title.y = element_text(size=16,face="bold"),
            axis.text.y = element_text(size=12,face="bold"),
            axis.text.x = element_text(size=12,face="bold"),
            strip.text.y = element_blank(),
            legend.text = element_text(size=12),
            legend.title = element_text(size=12, face="bold"))
    ggsave(file.path(StockDirFigs, paste0("Supp_OM-LogRecdevs_pro",stocks[j],".png")),
       width = 16, height = 10)

} #end j
