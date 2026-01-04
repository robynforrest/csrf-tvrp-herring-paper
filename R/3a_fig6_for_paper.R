# Make the figures for the paper
# January 2, 2026

# Load the historical MSEs.
# The OM scenarios will be loaded in the loop
hOMs <- readRDS(here("OMs/hOMs.rda"))
stocks <- names(hOMs)
nstocks <- length(stocks)
nsim <- hOMs[[1]]@nsim
histMSEs <- readRDS(here("MSEs/hist_hMSEs.rda"))
scenameHuman <- readRDS(here(SpDirOM, "ScenarioNamesHuman.rda"))

# FIGURE 6. SURPLUS PRODUCTION VS SSB
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
  cyr <- MSEscenarios[[1]]@OM$CurrentYr[1] # current year (2019)
  syr <- cyr-MSEscenarios[[1]]@nyears+1
  pyr <- cyr + 1 # get the first of the projection years (currently 2020)
  fyr <- cyr + pro_years #final year of projections
  stock <- stocks[j]


}

