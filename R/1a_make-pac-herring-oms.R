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
  # First 2500 of 5000 samples removed from MCMC files
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

  # look at rec deviations
  default_perr <- hOMs[[j]]@cpars$Perr_y
  png(paste0(StockDirFigs, "/OM_rec_devs_",stocks[j], ".png"), height=720, width=720)
    matplot(1:(nyears+maxage+pro_years), log(t(default_perr)), type="l", col=2, main=paste(stocks[j]))
    abline(h=0, lty=1, lwd=0.5)
    abline(v=nyears+maxage, lty=2, lwd=0.5)
  dev.off()

  # compare rec devs with iscam outputs
   iscam_perr_file <- file.path(iscamlocs[iscamfolder],"iscam_rdev_mcmc.csv")
   burn <- 4500
   iscam_perr <- read_csv(iscam_perr_file)
   iscam_perr <- iscam_perr[(burn+1):nrow(iscam_perr),]
   png(paste0(StockDirFigs, "/iscam_rec_devs_",stocks[j], ".png"), height=720, width=720)
     matplot(1:(nyears-2), t(iscam_perr), type="l", col=3, main=paste(stocks[j]))
     abline(h=0, lty=1, lwd=0.5)
   dev.off()

  # Check iscam recruitment pars and OM
   iscam_pars_mcmc <- mcmc_output$params %>%
     select(ro_gr1,h_gr1)
   iscam_pars_mcmc <- iscam_pars_mcmc[(burn+1):nrow(iscam_pars_mcmc),]
   post_mean_iscam_ro <- mean(iscam_pars_mcmc[,1])
   post_med_iscam_ro <- median(iscam_pars_mcmc[,1])
   post_mean_iscam_h <- mean(iscam_pars_mcmc[,2])
   post_med_iscam_h <- median(iscam_pars_mcmc[,2])
   post_mean_med_ro_h <- as.data.frame(cbind(post_mean_iscam_ro,
                           post_med_iscam_ro,
                           post_mean_iscam_h,
                           post_med_iscam_h))
   write_csv(post_mean_med_ro_h,file.path(StockDirFigs, "iscam_roh_mcmcsummary.csv"))

  # Plot the M time series before making any adjustments to cpars$M_age_array
  # historical only
  cyr <- hOMs[[j]]@CurrentYr
  syr <- cyr-hOMs[[j]]@nyears+1
  g <- getM(hOMs[[j]], ScenarioNamesHuman[1], age=Mage, type="annual",quant=TRUE, input_type="OM") %>%
    mutate(scenario = factor(scenario, levels = ScenarioNamesHuman)) %>%
    filter(year<=cyr) %>%
    as.data.frame() %>%
    ggplot() +
    geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), alpha = 0.3) +
    geom_line(aes(x=year,y=med), lwd=1) +
    gfplot::theme_pbs() +
    scale_x_continuous(breaks = seq(syr, cyr, by = 4)) +
    labs(x = "Year", y = "M")
    ggsave(file.path(StockDirFigs, paste0("OM-M_All_M_scenarios_hist_",stocks[j],".png")),
           width = 8, height = 5)

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

  # get OM alpha and beta
  if(j==1){
    iscamalpha=199.308
    iscambeta=0.761672
  }
  if(j==2){
    iscamalpha=144.817
    iscambeta=0.08082
  }
  if(j==3){
    iscamalpha=145.839
    iscambeta=0.235396
  }
  alpha <- SRalphaconv( hist_hMSEs[[j]]@SampPars$Stock$hs,  hist_hMSEs[[j]]@SampPars$Stock$SSBpR[, 1])
  beta <-  SRbetaconv(hist_hMSEs[[j]]@SampPars$Stock$hs, hist_hMSEs[[j]]@SampPars$Stock$R0, hist_hMSEs[[j]]@SampPars$Stock$SSBpR[, 1])
  g <- as.data.frame(alpha) %>%
    melt() %>%
    mutate(iscam <- iscamalpha) %>%
    ggplot() +
    geom_boxplot(aes(variable,value))+
    geom_point(aes(variable,iscamalpha), colour="red", size=4)
  g1 <- as.data.frame(beta) %>%
    melt() %>%
    mutate(iscam <- iscambeta) %>%
    ggplot() +
    geom_boxplot(aes(variable,value))+
    geom_point(aes(variable,iscambeta), colour="red", size=4)
  cowplot::plot_grid(g,g1, ncol=2)
  ggsave(file.path(StockDirFigs, paste0("iscam_v_OM_alphabeta_",stocks[j],".png")),
         width = 8, height = 5)

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

