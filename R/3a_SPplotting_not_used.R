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

  # Surplus production equation (from Forrest et al. 2023)
  # Pt = Bt+1 - Bt + Ct+1
  # We will only look at the historical period so we only need to look at one scenario
  mse <- readRDS(here(StockDirMSE, "hMSEs_NF.rda"))[[1]]
  nyrs <- mse@nyears
  cyr <- mse@OM$CurrentYr[1] # current year (2023)
  syr <- cyr-mse@nyears+1
  Year <- syr:cyr

  stock <- stocks[j]

  for(k in 1:nsim){
    SSB   <- mse@SSB_hist[k,]
    Catch <- mse@CB_hist[k,]

    Pt <- vector(length=nyrs-1)
    for(i in 1:(nyrs-1)){
      Pt[i] <- SSB[i+1]-SSB[i]+Catch[i+1]
    }

    # Get the LRP in 2023 (0.3 * meanB0 in 2023)
    meanB0 <- getmeanB0(mse,scenameHuman[1], age=Mage, type="mean", quants=FALSE)
    LRP2023 <- 0.3*meanB0[k,nyrs]

    SPdata <- cbind(Year[1:(nyrs-1)],SSB[1:(nyrs-1)],Catch[1:(nyrs-1)],
                    Pt, rep(LRP2023,(nyrs-1)),k)
    colnames(SPdata) <- c("Year", "SSB","Catch","Pt","LRP2023","Sim")

    plot(SSB[1:(nyrs-1)],Pt, type="l", main=paste(k, stock))
    abline(v = LRP2023, col = "red", lty = 1, lwd = 0.5)
    abline(h = 0, col =1, lty = 2, lwd = 0.5)
  }


}

