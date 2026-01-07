# ======================================================================================================
# ======== 0 load packages and universal settings ================================================
# Load packages, universal settings
# This code is sourced in Run 0_run-analyses.R
# Universal settings

#Make the OMs and MSEs master folders if they don't exist
# folders for individual stocks are made later
OMDir <- here::here("OMs")
  if(!file.exists(OMDir)) dir.create(OMDir, recursive=TRUE)
MSEDir <- here::here("MSEs")
  if(!file.exists(MSEDir)) dir.create(MSEDir, recursive=TRUE)
FigsDir <- here::here("Figures")
  if(!file.exists(FigsDir)) dir.create(FigsDir, recursive=TRUE)

source("R/0a_load-packages.R")

# Source customized functions
source("R/99_make-pac-Mscenarios.R")
source(here("R/99_plot_funcs.R"))  # A few custom plots
source(here("R/99_get_objects.R"))  #functions for extracting OM and MSE objects
source(here("R/99_get_objects_refpts.R"))  #functions for extracting ref point objects

# Plot settings
# bounds of confidence intervals
conflo <- 0.05
confhi <- 0.95

#colours for reference points, M and SSB (based on aaas palette) if not colouring by scenario
# Not sure if this is the best choice of colours
# May be too similar to the Dark2 palette being used for scenarios
#display.brewer.pal(n = 8, name = 'Dark2')
# dark2cols <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")
colpalette <- "Dark2" #"Set1" # colorBrewer palette

# Updated these colours to match the dark2 palette used in openMSE's plot_TS plots
nfcol <- "#E7298A" #manualcolors[7]
histcol  <- "#D95F02" #"#D95F02" #"#008280FF"
#meanhistcol  <- manualcolors[3] #"#D95F02" #"#008280FF"
meancol  <- "#7570B3" #"#D95F02" #"#008280FF"
meanrecentcol  <- "#66A61E" #"#7570B3" #"#631879FF"
dyncol   <- "#1B9E77" #"#E7298A" #"#EE0000FF"
ssbcol   <- "black"
mortcol  <- "#A20056FF"
fmortcol <- "darkblue"
sc_colours <- c("SSB" = ssbcol, "histB0" = histcol, "meanB0" = meancol, "recentB0" = meanrecentcol, "dynB0" = dyncol)
bo_colours <- c(histcol, meancol, meanrecentcol, dyncol)
bo_colours_wnf <- c(nfcol,histcol, meancol, meanrecentcol, dyncol)
manualcolors <- c("black",bo_colours)

mytheme_lg <- gfplot::theme_pbs() +
  theme(title = element_text(size=12, face="bold"))+
  theme(plot.title = element_text(face="bold", size=20))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.title.x = element_text(size=22,face="bold"))+
  theme(axis.title.y = element_text(size=22,face="bold"))+
  theme(strip.text.x = element_text(size=20,face="bold"))+
  theme(strip.text.y = element_text(size=18))+
  theme(legend.text = element_text(size=18))+
  theme(legend.title = element_text(size=22))
theme_set(mytheme_lg)

mytheme <- gfplot::theme_pbs() +
  theme(title = element_text(size=12, face="bold"))+
  theme(plot.title = element_text(face="bold", size=20))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(size=14,face="bold"))+
  theme(axis.title.y = element_text(size=14,face="bold"))+
  theme(strip.text.x = element_text(size=18,face="bold"))+
  theme(strip.text.y = element_text(size=18))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))
theme_set(mytheme)

mytheme_paper <- gfplot::theme_pbs() +
  theme(title = element_text(size=12, face="bold"))+
  theme(plot.title = element_text(face="bold", size=20))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.title.x = element_text(size=12,face="bold"))+
  theme(axis.title.y = element_text(size=12,face="bold"))+
  theme(strip.text.x = element_text(size=18,face="bold"))+
  theme(strip.text.y = element_text(size=18))+
  theme(legend.text = element_text(size=12))+
  theme(legend.title = element_text(size=14))+
  theme(strip.text.x = element_blank())+
  theme(strip.text.y =  element_blank())+
  theme(plot.margin = unit(c(0, 0.1, 0, 0), "cm"))
theme_set(mytheme_paper)

# smaller fonts
mytheme_sm <- gfplot::theme_pbs() +
  theme(title = element_text(size=12, face="bold"))+
  theme(plot.title = element_text(face="bold", size=20))+
  theme(axis.text.x = element_text(size=7))+
  theme(axis.text.y = element_text(size=7))+
  theme(axis.title.x = element_text(size=12,face="bold"))+
  theme(axis.title.y = element_text(size=12,face="bold"))+
  theme(strip.text.x = element_text(size=10,face="bold"))+
  theme(strip.text.y = element_text(size=3,face="bold"))+
  theme(legend.text = element_text(size=7))+
  theme(legend.title = element_text(size=8))
theme_set(mytheme_sm)

integer_breaks <- function(x)
  seq(floor(min(x)), ceiling(max(x)))

# Make a function to calculate SSB0 for each mcmc sample using long-term mean M
# Assumes M is the same for all ages
# pars is a list containing:
#   B0orig, R0orig, and Steep, default values from OM.
#   B0orig and R0orig are needed to calculate phie0orig, which are needed to get the SR pars, assumed fixed
#   meanM is a mean value of M from an OM with time-varying M
#   meanFec is a mean vector of fecundity-at-age from an OM with time-varying M (wt@age * maturity@age)
#   spawnfrac is the fraction of total mortality that occurs prior to spawning

# This version uses leading steepness
calc_tv_B0 <- function(pars){
  # inputs
  B0orig  <- pars$B0 # this is SSB0 in the Hist@OM object, not B0. Needed to get the original phie0
  R0orig  <- pars$R0
  Steeporig   <- pars$Steep
  # Time-varying parameters
  meanM   <- pars$meanM
  meanFec <- pars$meanFec
  # Fixed parameters
  spawn_frac <- pars$spawn_frac

  phie0orig <- B0orig/R0orig

  # Assume SRR is unchanged.
  # Get parameters from original OM parameters that produced R0 (i.e., M and fec-at-age)
  SRalpha <- MSEtool::SRalphaconv(Steeporig, phie0orig, SR=1, type = 1)
  SRbeta  <- MSEtool::SRbetaconv(Steeporig, phie0orig, R0orig, SR=1, type = 1)

  # Now get the new phie0 (function below)
  phie0new <- calc_phie0(meanM, meanFec, spawn_frac)

  # Now have to re-calculate new R0, Steepness and B0 from the original SRR parameters and the new phie0
  # (from new, time-varying M and fec)
  R0new <- (SRalpha*phie0new - 1)/(SRbeta*phie0new)
  B0new <- phie0new*R0new
  Steepnew <- MSEtool::hconv(SRalpha, phie0new, SR=1,type = 1)

  out <- list()
  out$SRalpha <- SRalpha
  out$SRbeta  <- SRbeta
  out$B0_orig <- B0orig
  out$R0_orig <- R0orig
  out$phie0_orig <- phie0orig
  out$steep_orig <- Steeporig
  out$B0_new  <- B0new
  out$R0_new  <- R0new
  out$phie0_new <- phie0new
  out$steep_new <- Steepnew

  out
}

# FOR FIGURES 3 AND 4 IN PAPER
# This version uses leading alpha and beta
calc_tv_B0_alphabeta <- function(pars){
  # inputs
  SRalpha <- pars$SRalpha
  SRbeta <- pars$SRbeta
  # Time-varying parameters
  M   <- pars$M
  Fec <- pars$Fec
  # Fixed parameters
  spawn_frac <- pars$spawn_frac

  # Now get the new phie0 (function below)
  phie0new <- calc_phie0(M, Fec, spawn_frac)

  # Now have to re-calculate new R0, Steepness and B0 from the original SRR parameters and the new phie0
  # (from new, time-varying M and fec)
  R0new <- (SRalpha*phie0new - 1)/(SRbeta*phie0new)
  B0new <- phie0new*R0new
  Steepnew <- MSEtool::hconv(SRalpha, phie0new, SR=1,type = 1)

  out <- list()
  out$SRalpha <- SRalpha
  out$SRbeta  <- SRbeta
  out$B0_new  <- B0new
  out$R0_new  <- R0new
  out$phie0_new <- phie0new
  out$steep_new <- Steepnew

  out
}

calc_phie0 <- function(M,fec,spawn_frac){

  nage <- length(fec)
  lx <- numeric(length=nage) # stepping stone that doesn't account for spawn timing
  lw <- numeric(length=nage) # unfished survivorship, accounts for spawn timing

  lx[1]<-1
  lw[1]<-1

  # Calculate survivorship
  for(j in 1:nage)
  {
    if(j > 1)
    {
      lx[j] <- lx[j-1] * exp(-M)
    }
    lw[j] <- lx[j] * exp(-M*spawn_frac) # adjust for spawn timing
  }
  lw[nage] <- lw[nage] / (1.0 - exp(-M)) # plus group

  phie0 <- sum(lw*fec)

  phie0
}

#####################################################################################
# Testing spawn fraction with eq 1 in paper
inputFec <- c(0.01206243,0.06466500,0.09290000,0.10735000,0.12520000,0.13905000,0.14245000,0.09600000,0.15850000)
# a b parameterisation
Pars <- list(
  SRalpha=135.7,
  SRbeta=0.64,
  M=0.5,
 Fec=inputFec, # Trying to reproduce what MSEtool is doing - this is the mean of first 2 years
  spawn_frac=1
)

bo1 <- calc_tv_B0_alphabeta(Pars)$B0_new

M <- 0.5
fec <- Pars$Fec
R0 <- calc_tv_B0_alphabeta(Pars)$R0_new

spr <- numeric(length=9)
for(i in 1:9){
   spr[i] <- fec[i]*exp(-M*(i-1))*exp(-M*Pars$spawn_frac)
}
spr[i] <- spr[i]/(1. - exp(-M))

bo2 <- sum(spr)*R0
bo1
bo2

# steep R0 parameterisation
Pars <- list(
  B0 = 118,
  R0 =292,
  Steep  = 0.914,
  meanM=0.229,
  meanFec=inputFec,
  spawn_frac=1
)

bo1 <- calc_tv_B0(Pars)$B0_new

M <- 0.229
fec <- Pars$meanFec
R0 <- calc_tv_B0(Pars)$R0_new

spr <- numeric(length=9)
for(i in 1:9){
  spr[i] <- fec[i]*exp(-M*(i-1))*exp(-M*Pars$spawn_frac)
}
spr[i] <- spr[i]/(1. - exp(-M))

bo2 <- sum(spr)*R0
bo1
bo2



