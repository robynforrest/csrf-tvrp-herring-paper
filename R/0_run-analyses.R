# ======================================================================================================
# ======== 0 Run all the analyses from here ================================================
# Load packages, universal settings, source code

# CSRF Project for paper on time-varying reference points for Pacific herring

# AUTHORS/CONTRIBUTORS:
# Robyn Forrest (RF) (DFO)
# Quang Huynh (QH) (Blue Matter Science)
# Adrian Hordyk (AH) (Blue Matter Science)

# Created January 2 2026, following earlier exploratory repository (2023)

# TODO: Ask QH about steepness values in OM. Too high.

library(here)

omspecies <- "pac-herring"

# DO NOT CHANGE (the order is important wrt the iscam files)
allpacherringstocks <- c("HG","SoG","WCVI") #don't change this, need it so that OM picks the right data folder
pacherringstocks <- allpacherringstocks

# MAKE DIRECTORIES
SpDirOM <- here("OMs")
if(!file.exists(SpDirOM)) dir.create(SpDirOM, recursive=TRUE)
SpDirMSE <- here("MSEs")
if(!file.exists(SpDirMSE)) dir.create(SpDirMSE, recursive=TRUE)
SpDirFigs <- here("Figures")
if(!file.exists(SpDirFigs)) dir.create(SpDirFigs, recursive=TRUE)

# Universal settings that are changed frequently
nsim <- 8
pro_years <- 50
Mage <- 3 # age at which to extract constant M from cpars M_age_array

# OM SETTINGS
# Random walk parameters for om M
alpha_m_inc <- 0.009 # setting for random walk in M with upward drift
alpha_m_dec <- -0.012 # setting for random walk in M with downward drift (not used)
sigma_m   <- 0.03 # setting for random walk in M

# what sd for M (across reps) should we base M projections on? nyears-sd_numyears.
# sd across reps for M is really large in final year, sample from sd_numyears earlier
sd_numyears <- 3

# Smoothing parameters for future M
msplines <- TRUE # Choose whether to smooth projected M random walks with spline
knotdiv <- 3 # How many years to divide proyears by for splines (smaller number means more knots, e.g., 30/3 = 10 knots)

# MP SETTINGS
# Settings for assessment model and plots (not used in this version of the repo)
Interval <- 1 #1 # stock assessment interval
# diagnosticMP <- "full" # Setting for make_MP. Can be "none"  "min" or "full". See ??make_MP
# plot_retros <- FALSE #TRUE # set whether to plot retrospectives ... takes a while
# plot_M_timeseries <- TRUE # set whether to plot time series of estimated M - a bit slow

# ======================================================================================================
# Choose whether to run the OMs and MSEs (only need to do this once)
make_oms  <- TRUE #T/F Turn off if you have already made and saved the base OMs
run_mses  <- TRUE #T/F Turn off if you have already run the MSEs.
# ======================================================================================================

# Source files to load packages and set universal settings
# This will also source the file that makes the MPs
# Also sources the files that make all the functions to extract MSE objects
source("R/0_settings.R")

# Build OMs
# Looping through species is handled in 1_make-oms.R
# If make_oms == F, this won't do anything
# Makes plots as well
source("R/1a_make-pac-herring-oms.R")

# Run MSEs with no fishing
# If run_mses == F, this won't do anything
# Makes plots as well
source("R/2a_run-pac-herring-mses.R")

# ====================================================================================================================
# ======= END OF SCRIPT ==============================================================================================
# ====================================================================================================================
