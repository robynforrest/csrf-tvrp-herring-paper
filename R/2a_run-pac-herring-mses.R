# === 2 Run the MSEs  ===========================================================================================
# This code doesn't stand alone. Run 0_run-analyses.R first to load
#  packages, MPs and some objects
# This code is called by 2_run-mse.R
# This version only runs the no fishing MP
# ===================================================================================================================================================
cores <- floor(future::availableCores()/2)
plan(multisession, workers = cores)

stocks <- pacherringstocks # this is set in 0_run-analyses.R
nstocks <- length(stocks)
ScenarioNamesHuman <- readRDS(here(SpDirOM, "ScenarioNamesHuman.rda"))

hist <- readRDS(here(SpDirMSE,"hist_hMSEs.rda"))

# --- NO FISHING MP ------------------------------
if(run_mses==TRUE){
    # Loop over stocks
    for(j in 1:nstocks){

      StockDirOM <- here(SpDirOM, paste(stocks[j]))
        if(!file.exists(StockDirOM)) message("Stop. No OMs found. Please run 1_make-oms.R first. \n")
      StockDirMSE <- here(SpDirMSE, paste(stocks[j]))
        if(!file.exists(StockDirMSE)) dir.create(StockDirMSE, recursive=TRUE)
      StockDirFigs <- here(SpDirFigs, paste(stocks[j]))
        if(!file.exists(StockDirFigs)) dir.create(StockDirFigs, recursive=TRUE)

      # Read in the OM objects
      OMscenarios <- readRDS(here(StockDirOM, "OMScenarios.rda"))
      scenames <- names(OMscenarios)
      nscenarios <- length(scenames)
      nsim <- OMscenarios[[1]]@nsim

      # Run No Fishing MSE for each scenario (this could take a while if nsim is large)
      hMSEs_NF <- purrr::map(OMscenarios, MPs="NFref",extended=T, MSEtool::runMSE)
      names(hMSEs_NF) <- scenames

      saveRDS(hMSEs_NF,here(StockDirMSE,"hMSEs_NF.rda"))

    } # end NF stocks loop

    cat("~~~ Finished No Fishing MSEs for Pacific Herring stocks ~~~\n")

    source("R/2b_plot-pac-herring-mse-nf.R")

    cat("~~~ Finished plotting No Fishing MSEs for Pacific Herring stocks ~~~\n")
  }else{
    cat("~~~ Skipping run MSEs ~~~\n")
  }
# ====================================================================================================================
# ======= END OF SCRIPT ==============================================================================================
# ====================================================================================================================
