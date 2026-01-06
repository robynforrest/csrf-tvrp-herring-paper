# Make the figures for the paper
# January 2, 2026

# Load the historical MSEs.
# The OM scenarios will be loaded in the loop
histMSEs <- readRDS(here(SpDirMSE, "hist_hMSEs.rda"))
ScenarioNamesHuman <- readRDS(here(SpDirOM, "ScenarioNamesHuman.rda"))
stocks <- names(histMSEs)
nstocks <- length(stocks)

# Create lists for putting figures
fig2a <- list()
fig2b <- list()

# FIGURE 2. TIME SERIES OF HISTORICAL SSB AND M
for(j in 1:nstocks){
  cat("~~~ Plotting Fig 2 for", paste(stocks[j]), "~~~\n")

  # Make directories
  StockDirOM    <- here(SpDirOM, paste(stocks[j]))
  StockDirMSE   <- here(SpDirMSE, paste(stocks[j]))
  StockDirFigs  <- here(SpDirFigs, paste(stocks[j]))
  if(!file.exists(StockDirFigs)) dir.create(StockDirFigs, recursive=TRUE)
  StockDirFigs_NF <- here(StockDirFigs, "NF")
  if(!file.exists(StockDirMSE)) stop("Stop. No MSEs found. Please run 2_run-mse.R first. \n")
  if(!file.exists(StockDirFigs_NF)) dir.create(StockDirFigs_NF, recursive=TRUE)

  nsim<-histMSEs[[j]]@OM@nsim
  yind<-histMSEs[[j]]@OM@nyears+(1:histMSEs[[j]]@OM@proyears)

  OMscenarios  <- readRDS(here(StockDirOM, "OMscenarios.rda"))
  MSEscenarios <- readRDS(here(StockDirMSE, "hMSEs_NF.rda"))
  #histMSE <- histMSEs[j][[1]]
  cyr <- MSEscenarios[[1]]@OM$CurrentYr[1] # current year (2019)
  syr <- cyr-MSEscenarios[[1]]@nyears+1
  stock <- stocks[j]
  nyears <- MSEscenarios[[1]]@nyears

  #~~~~~~~~~~~PLOT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot the M time series
  Mtout <-  getM(OMscenarios[[1]], ScenarioNamesHuman[1],age=Mage,
                 type="annual", quant=TRUE, input_type="OM")|>
    filter(year<=cyr) |>
    as.data.frame()

  g1 <- Mtout |>
    as.data.frame()  |>
    ggplot() +
    geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), fill=1, alpha = 0.2) +
    geom_line(aes(x=year,y=med), color=1, lwd=2) +
    # geom_line(aes(x=year,y=Trace1), color=1, lwd=0.25) +
    # geom_line(aes(x=year,y=Trace2), color=1, lwd=0.25) +
    # geom_line(aes(x=year,y=Trace3), color=1, lwd=0.25) +
    # geom_line(aes(x=year,y=Trace4), color=1, lwd=0.25) +
    # geom_line(aes(x=year,y=Trace5), color=1, lwd=0.25) +
    scale_y_continuous(breaks = seq(0,3,by=0.25))+
    scale_x_continuous(breaks = seq(1951,2023,by=5))+
    gfplot::theme_pbs() +
    labs(x = "Year", y = "M")+
    mytheme_paper+
    theme(legend.position = "none")
g1

# Get the LRP
LRP <- getmeanB0(MSEscenarios[[1]], ScenarioNamesHuman[1], age=Mage,
                      type="mean", quants=TRUE)|>
  as.data.frame() |>
  filter(year==2023) |>
  mutate(lwr=0.3*lwr,med=0.3*med,upr=0.3*upr)

LRP_ribbon <-cbind(Mtout$year,rep(LRP$lwr,nyears),rep(LRP$med,nyears),rep(LRP$upr,nyears)) |>
  as.data.frame()
colnames(LRP_ribbon) <- c("year","lwr","med","upr")

# Plot the SSB time series
g2 <- purrr::map2_df(MSEscenarios,ScenarioNamesHuman, getSSB, mp=1) |>
  as.data.frame() |>
  filter(year<=cyr, scenario %in% ScenarioNamesHuman[1]) |>
  ggplot() +
  geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), fill=1, alpha = 0.2) +
  geom_line(aes(x=year,y=med), color=1, lwd=2) +
  geom_ribbon(data=LRP_ribbon, aes(x=year, ymin=lwr, ymax=upr), fill=2, alpha = 0.1) +
  geom_line(data=LRP_ribbon,aes(x=year,y=med), color=2, lwd=1, lty=2) +
  theme(legend.position = "none") +
  labs(x = "Year", y = "SSB")+
  scale_x_continuous(breaks = seq(1951,2023,by=5))+
  gfplot::theme_pbs() +
  mytheme_paper+
  theme(legend.position = "none")
g2

cowplot::plot_grid(g1,g2,nrow=2)
ggsave(file.path(StockDirFigs, paste0("FIG2_M_SSB_",stocks[j],".png")),
       width = 8, height = 5)

# Add the figures to lists
fig5a[[j]] <- g1
fig5b[[j]] <- g2

} #end for j

