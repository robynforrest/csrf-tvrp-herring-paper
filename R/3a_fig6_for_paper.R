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

# # FIGURE 7. HARVEST CONTROL RULES
# This is a new plot for the paper
for(j in 1:nstocks){
  cat("~~~ Plotting Fig 6 for", paste(stocks[j]), "~~~\n")

  StockDirOM    <- here(SpDirOM, paste(stocks[j]))
  StockDirMSE   <- here(SpDirMSE, paste(stocks[j]))
  StockDirFigs  <- here(SpDirFigs, paste(stocks[j]))
  if(!file.exists(StockDirFigs)) dir.create(StockDirFigs, recursive=TRUE)
  StockDirFigs_NF <- here(StockDirFigs, "NF")
  if(!file.exists(StockDirMSE)) stop("Stop. No MSEs found. Please run 2_run-mse.R first. \n")
  if(!file.exists(StockDirFigs_NF)) dir.create(StockDirFigs_NF, recursive=TRUE)



}

