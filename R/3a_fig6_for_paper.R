# Make the figures for the paper
# January 2, 2026
# Robyn Forrest

# Load the historical MSEs.
# The OM scenarios will be loaded in the loop
hOMs <- readRDS(here("OMs/hOMs.rda"))
stocks <- names(hOMs)
nstocks <- length(stocks)
nsim <- hOMs[[1]]@nsim
histMSEs <- readRDS(here("MSEs/hist_hMSEs.rda"))
scenameHuman <- readRDS(here(SpDirOM, "ScenarioNamesHuman.rda"))

# # FIGURE 6. HARVEST CONTROL RULES
for(j in 1:nstocks){
  cat("~~~ Plotting Fig 6 for", paste(stocks[j]), "~~~\n")

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

  # Show LRP changing through time in perfectly estimated HCR (OM)
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

  # OM HCR - no phase plot - this will save the plots into dir
  # This function is in 99_plot_funcs.R
  g1 <- plotTVHCRom_oneplot(allB0,
                    b0type="hist",
                    scenario=ScenarioNamesHuman[2],
                    pyr=cyr+1,
                    lrp = 0.3,
                    usr = 0.6,
                    maxf=0.2,
                    om=TRUE)
  g2 <- plotTVHCRom_oneplot(allB0,
                            b0type="mean",
                            scenario=ScenarioNamesHuman[2],
                            pyr=cyr+1,
                            lrp = 0.3,
                            usr = 0.6,
                            maxf=0.2,
                            om=TRUE)
  g3 <- plotTVHCRom_oneplot(allB0,
                            b0type="recent",
                            scenario=ScenarioNamesHuman[2],
                            pyr=cyr+1,
                            lrp = 0.3,
                            usr = 0.6,
                            maxf=0.2,
                            om=TRUE)
  g4 <- plotTVHCRom_oneplot(allB0,
                            b0type="dyn",
                            scenario=ScenarioNamesHuman[2],
                            pyr=cyr+1,
                            lrp = 0.3,
                            usr = 0.6,
                            maxf=0.2,
                            om=TRUE)

  g1 <- g1+
    xlab("")+
    theme(axis.text.x=element_blank())
  g2 <- g2+
    xlab("")+
    theme(axis.text.x=element_blank())
  g3 <- g3+
    xlab("")+
    theme(axis.text.x=element_blank())
  g4 <- g4

  cowplot::plot_grid(g1,g2,g3,g4, nrow=4, labels=c("(a)","(b)","(c)","(d)"),
                     hjust = 0.25)#, align="h")
  ggsave(here("Figures",paste0("Figure6_",stocks[j],".png")))


}# end j

