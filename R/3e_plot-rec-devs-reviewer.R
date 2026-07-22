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
ymax <- c(80,200,220) #biomass ylims for each stock

for(j in 1:nstocks){
  cat("~~~ Plotting Sensitivity figs for", paste(stocks[j]), "~~~\n")

   nsim<-hist_MSEs[[j]]@OM@nsim
   yind<-hist_MSEs[[j]]@OM@nyears+(1:hist_MSEs[[j]]@OM@proyears)

    # Get the OMs with alternative M scenarios - just 250 reps
   OMscenarios <- readRDS(here("Sensitivity_results","BaseCase", "OMs",
                                   paste(stocks[j]),"OMScenarios.rda"))
   MSEscenarios <- readRDS(here("Sensitivity_results","BaseCase", "MSEs",
                                    paste(stocks[j]),"hMSEs_NF.rda"))
   # Alternative with higher rec devs
   OMscenarios_alt <- readRDS(here("Sensitivity_results","IncreaseRecDevs_reviewer_only", "OMs",
                                    paste(stocks[j]),"OMScenarios.rda"))
   MSEscenarios_alt <- readRDS(here("Sensitivity_results","IncreaseRecDevs_reviewer_only", "MSEs",
                                     paste(stocks[j]),"hMSEs_NF.rda"))



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
    # 2. Log rec devs - baseline
    meanlogdev <- purrr::map2_df(OMscenarios,ScenarioNamesHuman, getlogperry_mean, proc_err)|>
      as.data.frame()
    quantlogdev <- purrr::map2_df(OMscenarios,ScenarioNamesHuman, getlogperry, proc_err) |>
      as.data.frame()
    g1 <- left_join(quantlogdev,meanlogdev) |>
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
      labs(x = "Year", y = "Log recruitment deviations", title="Mean (projection years) = 0")+
      theme(plot.title = element_text(face="bold", size=16),
            axis.title.x = element_text(size=16,face="bold"),
            axis.title.y = element_text(size=16,face="bold"),
            axis.text.y = element_text(size=12),
            axis.text.x = element_text(size=12),
            strip.text.y = element_blank(),
            legend.text = element_text(size=12),
            legend.title = element_text(size=12, face="bold"))
    g1

    meanlogdev <- purrr::map2_df(OMscenarios_alt,ScenarioNamesHuman, getlogperry_mean, proc_err)|>
      as.data.frame()
    quantlogdev <- purrr::map2_df(OMscenarios_alt,ScenarioNamesHuman, getlogperry, proc_err) |>
      as.data.frame()
    g2 <- left_join(quantlogdev,meanlogdev) |>
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
      labs(x = "Year", y = "Log recruitment deviations", title="Mean (projection years) = mean of last five historical years")+
      theme(plot.title = element_text(face="bold", size=16),
            axis.title.x = element_text(size=16,face="bold"),
            axis.title.y = element_text(size=16,face="bold"),
            axis.text.y = element_text(size=12),
            axis.text.x = element_text(size=12),
            strip.text.y = element_blank(),
            legend.text = element_text(size=12),
            legend.title = element_text(size=12, face="bold"))
    g2

    cowplot::plot_grid(g1,g2,ncol=1)

    ggsave(file.path(StockDirFigs, paste0("Sensitivity_OM-IncreasedLogRecdevs_250_",stocks[j],".png")),
           width = 12, height = 7.5)

    # Now show biomass
    dat <- purrr::map2_df(MSEscenarios,ScenarioNamesHuman, getSSB, mp=1) |>
      as.data.frame() |>
      mutate(group=factor(scenario, levels=ScenarioNamesHuman))

    g3 <- ggplot(dat) +
      geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), fill=ssbcol, alpha = 0.1) +
      geom_line(aes(x=year,y=med), color=ssbcol, lwd=1.25) +
      facet_wrap(vars(group), nrow=1)+
      theme(legend.position = "none")+
      labs(x = "Year", y = "SB", title= "")+
      ylim(0,ymax[j])+
      geom_vline(xintercept=cyr, lty=3)+
      mytheme_sens_plots+
      theme(axis.title.x = element_text(size=16,face="bold"),
            axis.title.y = element_text(size=16,face="bold"))
    g3

    dat <- purrr::map2_df(MSEscenarios_alt,ScenarioNamesHuman, getSSB, mp=1) |>
      as.data.frame() |>
      mutate(group=factor(scenario, levels=ScenarioNamesHuman))

    g4 <- ggplot(dat) +
      geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), fill=ssbcol, alpha = 0.1) +
      geom_line(aes(x=year,y=med), color=ssbcol, lwd=1.25) +
      facet_wrap(vars(group), nrow=1)+
      theme(legend.position = "none")+
      labs(x = "Year", y = "SB", title= "")+
      ylim(0,ymax[j])+
      geom_vline(xintercept=cyr, lty=3)+
      mytheme_sens_plots+
      theme(axis.title.x = element_text(size=16,face="bold"),
            axis.title.y = element_text(size=16,face="bold"))
    g4

    constM <- cowplot::plot_grid(g1,g3,ncol=1, align="v")
    ggsave(file.path(StockDirFigs, paste0("Sensitivity_MSE_SSB_BaseLogRecdevs_250_",stocks[j],".png")),
           width = 16, height = 10)
    IncreasM <- cowplot::plot_grid(g2,g4,ncol=1, align="v")
    ggsave(file.path(StockDirFigs, paste0("Sensitivity_MSE_SSB_IncreasedLogRecdevs_250_",stocks[j],".png")),
           width = 16, height = 10)

    cowplot::plot_grid(constM,IncreasM,ncol=2, align="h")
    ggsave(file.path(here("Figures"), paste0("/Sensitivity_MSE_LogRecdevs_250_",stocks[j],".png")),
           width = 16, height = 10)

} #end j
