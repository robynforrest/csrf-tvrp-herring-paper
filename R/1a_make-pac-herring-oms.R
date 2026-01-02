# ===================================================================================================================================================
# === 1 make the OMs ======================================================================================
# This code doesn't stand alone. Run 0_run-analyses.R first to load packages
#    and some objects
# Author: Robyn Forrest. August 2023. Updated August 2025.
# ===================================================================================================================================================
cat("~~~ Running 1a_make-pac-herring-oms.R ~~~\n")

# Get the herring iscam outputs
# Make OMs for all stocks even if not later used in further analyses
iscamlocs <- paste(here::here(),list.files("Data/Herring_iscam",full.names=T,include.dirs=T),sep="/")
stocks <- pacherringstocks
nstocks <- length(stocks)
hOMs <- list() # pacherring OMs
hist_hMSEs <- list() # pac herring historical MSEs

# Set scenario names.
ScenarioNames <- c("RandomWalkM", "RandomWalkMDriftIncr")
ScenarioNamesHuman <- c("Random Walk in M", "Random Walk in M Drift")
# Save ScenarioNames and ScenarioNamesHuman as a file as they do not get carried through in the OM
saveRDS(ScenarioNamesHuman, here(SpDirOM,"ScenarioNamesHuman.rda"))

# use iSCAM2OM to populate the basic OMs  #1:nOMs
# This is an alternative to rcm() which is a refitting exercise

#1:nstocks
for(j in 1:nstocks){
  stock <- stocks[j]
  cat("~~~ Making OMs for", paste(stock), "~~~\n")

  # Make output directories for OMs, MSEs and figs
  # SpDir should be carried through from 1_make-oms.R
  StockDirOM <- here::here(SpDirOM, paste(stocks[j]))
  if(!file.exists(StockDirOM)) dir.create(StockDirOM, recursive=TRUE)
  StockDirMSE <- here::here(SpDirMSE, paste(stocks[j]))
  if(!file.exists(StockDirMSE)) dir.create(StockDirMSE, recursive=TRUE)
  StockDirFigs <- here::here(SpDirFigs, paste(stocks[j]))
  if(!file.exists(StockDirFigs)) dir.create(StockDirFigs, recursive=TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~1. Make the basic OM from the iscam MCMC outputs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  iscamfolder <- which(stock == allpacherringstocks)
  mcmc_output  <- MSEtool::read.mcmc(iscamlocs[iscamfolder])

  # iSCAM2OM pulls all the outputs and also makes plot of age comp fits
  # First 2500 of 5000 MCMC samples have been removed from iscam output MCMC files
  png(paste0(StockDirFigs, "/Compare_iscam_om_comps",stocks[j], ".png"), height=720, width=720)
     hOMs[[j]]<-MSEtool::iSCAM2OM(iSCAMdir=iscamlocs[iscamfolder],
                                  nsim=nsim,
                                  proyears=pro_years,
                                  mcmc=mcmc_output,
                                  report=T,
                                  Name=stocks[j])
  dev.off()

  # update slots and save OM as rds file
  hOMs[[j]]@maxF <- 20
  hOMs[[j]]@seed <- 999
  hOMs[[j]]@interval<- Interval #stock assessment interval (set in 0_run-analyses.R)
  hOMs[[j]]@Name<-paste0(stocks[j])
  hOMs[[j]]@Common_Name <- "Pacific Herring"
  hOMs[[j]]@Species <- "Clupea pallasii"
  hOMs[[j]]@cpars$spawn_time_frac <- rep(1.,nsim) # Set spawn timing same as iscam (new MSEtool feature)
  hOMs[[j]]@AC <- 0. #autocorrelation in future rec devs
  hOMs[[j]]@beta <- c(1., 1.) # beta=1 means no hyperstability

  stockname <- hOMs[[j]]@Name
  nyears <- hOMs[[j]]@nyears
  nproyears <- hOMs[[j]]@proyears
  maxage <- hOMs[[j]]@maxage

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # code from Quang to fix minor error in some versions of MSEtool
  if(is.null(hOMs[[j]]@cpars[["hs"]]) && !is.null(hOMs[[j]]@cpars[["h"]])) { # Use of $ is often confusing since R deploys partial matching
     hOMs[[j]]@cpars[["hs"]] <- hOMs[[j]]@cpars[["h"]]
     hOMs[[j]]@cpars[["h"]] <- NULL
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~ 2. Now make OMS with alternative future M scenarios ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hOM <- hOMs[[j]]

  # Make a list to put the alternative scenarios
  OMscenarios <- list()

  # Call the function make_MScenarios
  # NOTE: FOR THE PROJECTION YEARS, MSETOOL IS TAKING THE MEAN OF THE LAST TWO HIST YEARS
  # FOR CONSTANT M in cpars$M_ageArray.
  # Before updating cpars$M_ageArray RF has changed the constant pro years
  #   to be the same as M in nyear to avoid a jog up or down (in make_pac_MScenarios()).  sd_numyears
  for(ss in 1:length(ScenarioNames)){

    OMscenarios[[ss]] <- make_pac_MScenarios(hOM,
                                             Scenario=ScenarioNames[ss],
                                             alphaminc = alpha_m_inc,
                                             alphamdec = alpha_m_dec,
                                             sigm=sigma_m,
                                             sdn=5,
                                             knotsdiv=knotdiv,
                                             species=omspecies[1],
                                             stock = stocks[j])
  }

  names(OMscenarios) <- ScenarioNames

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~ 3. SAVE the OMscenarios list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   # The list of OM objects in OMscenarios list will be used in the MSEs going forward
  filename <- here(StockDirOM,"OMScenarios.rda")
  saveRDS(OMscenarios, file=filename)
  rm(OMscenarios)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~ 3. Now run the historical MSE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # This gets all the time series for the historical period, before MPs are applied
  hist_hMSEs[[j]] <- runMSE(hOMs[[j]],Hist=T)

  # Make an html summary of the OM
  plot(hOMs[[j]], output_file="OMreport.html", output_dir=StockDirOM)

} # end stocks j

# Name the list objects (this is different from OM@Name which is intrinsic to each OM)
hOMnames <- paste0(stocks)
names(hOMs) <- hOMnames
names(hist_hMSEs) <- hOMnames

# Save the base OMs and historical MSEs
saveRDS(hOMs,here(SpDirOM,"hOMs.rda"))
saveRDS(hist_hMSEs,here(SpDirMSE,"hist_hMSEs.rda"))

cat("~~~ Finished OMs for Pacific Herring stocks ~~~\n")

source("R/1b_plot-pac-herring-om.R")

cat("~~~ Finished plotting OMs for Pacific Herring stocks ~~~\n")

# ====================================================================================================================
# ======= END OF SCRIPT ==============================================================================================
# ====================================================================================================================

