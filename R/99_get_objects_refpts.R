# ===================================================================================================================================================
# === Pacific Herring: 99 Get ref point objects from OMs and MSEs in formats for plotting  =============================================================================================
# === Sourced by various files  ===========================================================================
# ===================================================================================================================================================
# Robyn Forrest
# October 13 2025 - corrected calculations for long-term mean B0

#########################################################################################
# B0 four ways (different ways of using M to calculate B0)

# MeanB0 could be updateMeanB0 with some notation showing years (mean of t1 to T)

# 1 and 2 are static. The rest are updated.
# 1. histB0 - eqmB0 using mean M from first 5 years of hist ts (currently same as mse@SSB0)
# 2. meanB0 - eqmB0 using mean M from whole hist ts (same as herring assessment)
# 3.recentB0 - eqmB0 using 5 year average recent M
# 4. dynB0 - dynamic B0

# IMPORTANT UPDATE
# October 2025 update:
#  Changing the way that the three equilibrium versions of B0 (hist, mean, recent) are calculated
#  See R/Test_tv_SSB0.R and reports/TVB0_calcs_exploration.Rmd
# There is a notable difference between the "old" way of calculating meanB0, where we took the annual long-term
#  average of mse@RefPoint$ByYear$SSB0; and when we directly calculate annual meanB0 using the annual long-term
#  average of M and fecundity-at age.
# The latter is consistent with iscam and produces B0 values consistent with iscam
# Therefore, there is now a function called calc_tv_B0() in R/0_settings.R which will be used here to get B0
# There is an equivalent function somewhere deep in MSEtool (see comparisons in R/Test_tv_SSB0.R)
# but it is such a simple function that we may as well have it here

# getmeanB0 - Arguments:
# hist = hist object with alternative future M scenarios for one stock and one scenario
# scen = scenario name (usually one of ScenarioNamesHuman.rda)
# age = the age of constant adult M - function only considers a single value of M for each year
# type = how M and fec-at-age are averaged over the time series, as used in alternative B0 calculations
#        The correct averaging has already been done in getM and getFec, which also have the "type" argument:
#        annual: This is just the annual estimate of M (used in assessment and dynamic B0 calcs)
#        hist: This is the mean annual estimate of M from years 1:5 (used in Hist B0 calcs)
#        mean: This is the long-term mean estimate of M (used in mean B0 calcs)
#        recent: This is recent mean estimate of M from the most recent 5 years (used in recent B0 calcs)

# Returns: a dataframe with lower, median and upper estimates of annual mean B0 for the scenario (is the same for all years)
# Use this function for the hist, mean and recent mean types of B0
getmeanB0 <- function(mse,scen="Scenario1", age=3, type="mean", quants=TRUE){
  if(!type %in% c("annual","hist","mean","recent")){
    stop("type must be one of annual,hist,mean or recent \n")
  }

  all_years <- seq(mse@Hist@Data@OM$CurrentYr[1] - mse@nyears + 1, mse@Hist@Data@OM$CurrentYr[1]+mse@proyears)
  nreps  <- mse@nsim
  nyears <- length(all_years)

  # original parameters used by MSEtool to calculate R0
  inputB0  <- mse@OM$SSB0 # A vector of SSB0 needed to get S-R  alpha. Length=nreps
  inputR0  <- mse@OM$R0 # A vector of R0 needed to get S-R  alpha. Length=nreps
  inputSteep <- mse@OM$hs # A vector of steepness. Length=nreps
  # time-varying parameters - will result in a new phie0, steepness and R0
  inputM   <- getM(mse,scen,age=age,type=type, quant=FALSE, input_type="MSE") # A matrix of mean M  Dim: nrow=nreps, ncol=nyears
  inputFec <- getFec(mse,scen,type=type, input_type="MSE") # A matrix of annual long-term mean fecundity-at-age (same for all reps). Dim: nrow=nages, ncol=nyears
  # fixed parameters
  spawn_time_frac <- mse@OM$spawn_time_frac

  # The function calc_tv_B0() has one argument: pars (a list of 6)
  # Populate the pars argument in a loop (fast), then call the function
  SSB0_mean <- matrix(0, nrow=nreps, ncol=nyears)
  for(j in 1:nreps){
    for(i in 1:nyears){
      Pars <- list(
        B0=inputB0[j],
        R0=inputR0[j],
        Steep=inputSteep[j],
        meanM=inputM[j,i], # mean M (determined by type argument) for year and replicate. Correct annual mean already calculated in getM and getFec
        meanFec=inputFec[,i], # same for all reps
        spawn_frac=spawn_time_frac[j]
      )
      SSB0_mean[j,i] <- calc_tv_B0(Pars)$B0_new
    } # end i
  } # end j

  if(quants==TRUE){
    B0 <- SSB0_mean |>
      apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
      as.data.frame() |>
      mutate(year=all_years, scenario=scen, RefPtName=type) |>
      dplyr::rename(lwr=1, med=`50%`, upr=3) |>  as.data.frame()
  }else{
    B0 <- SSB0_mean
  }
  B0
}

# returns the 0.025, 0.5 and 0.975 confidence interval of dynB0 across replicates
getdynB0 <- function(mse, scen,quants=TRUE){
  all_years <- seq(mse@OM$CurrentYr[1] - mse@nyears + 1, mse@OM$CurrentYr[1]+mse@proyears)
  if(quants==TRUE){
    dynB0 <- mse@Hist@Ref$Dynamic_Unfished$SSB0 |>
      apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
      as.data.frame() |>
      mutate(year=all_years, scenario=scen, RefPtName="dyn") |>
      dplyr::rename(lwr=1, med=`50%`, upr=3) |>  as.data.frame()
  }else{
    dynB0 <- mse@Hist@Ref$Dynamic_Unfished$SSB0
  }
  dynB0
}

# asymB0 is the contemporary estimate of B0
# it is internally estimated by SAMtool
getasymB0 <- function(mse, scen){
  all_years <- seq(mse@OM$CurrentYr[1] - mse@nyears + 1, mse@OM$CurrentYr[1]+mse@proyears)

  asymB0 <- mse@RefPoint$ByYear$SSB0

  asymB0 <- asymB0 |>
    apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
    as.data.frame() |>
    mutate(year=all_years, scenario=scen) |>
    dplyr::rename(lwr=1, med=`50%`, upr=3)|>
    as.data.frame() |>
    melt(id.vars=c("year","scenario", "lwr","upr","med"), value.name="RefPtValue") |>
    mutate(RefPtName="contempB0")
  asymB0
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Relative asymptotic (contemporary) and dynamic values of B0
getasymB0rel <- function(mse, scen){
  all_years <- seq(mse@OM$CurrentYr[1] - mse@nyears + 1, mse@OM$CurrentYr[1]+mse@proyears)

  asymB0 <- mse@RefPoint$ByYear$SSB0
  asymB0 <- asymB0/asymB0[1]

  asymB0 <- asymB0 |>
    apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
    as.data.frame() |>
    mutate(year=all_years, scenario=scen) |>
    dplyr::rename(lwr=1, med=`50%`, upr=3) |>
    melt(id.vars=c("year","scenario", "lwr","upr","med"), value.name="RefPtValue") |>
    mutate(RefPtName="contempB0Rel")

  asymB0
}

getdynB0rel <- function(mse, scen){
  all_years <- seq(mse@OM$CurrentYr[1] - mse@nyears + 1, mse@OM$CurrentYr[1]+mse@proyears)

  dynB0 <- mse@Hist@Ref$Dynamic_Unfished$SSB0
  dynB0 <- dynB0/dynB0[1]

  dynB0 <- dynB0 |>
    apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
    as.data.frame() |>
    mutate(year=all_years, scenario=scen) |>
    dplyr::rename(lwr=1, med=`50%`, upr=3) |>
    as.data.frame() |>
    melt(id.vars=c("year","scenario", "lwr","upr","med"), value.name="RefPtValue") |>
    mutate(RefPtName="dynB0Rel")
  dynB0
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MSY-based reference points - summaries
# Just look at contemporary values here but could apply similar ways as for B0
# TODO: look at how default value of Bmsy and B_Bmsy is calculated

# Contemporary value of Bmsy and Fmsy
getasymBMSY <- function(mse, scen){
  all_years <- seq(mse@OM$CurrentYr[1] - mse@nyears + 1, mse@OM$CurrentYr[1]+mse@proyears)

  asymBMSY <- mse@Hist@Ref$ByYear$SSBMSY |>
    apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
    as.data.frame() |>
    mutate(year=all_years, scenario=scen) |>
    dplyr::rename(lwr=1, med=`50%`, upr=3) |>
    as.data.frame()
    melt(id.vars=c("year","scenario", "lwr","upr","med"), value.name="RefPtValue") |>
    mutate(RefPtName="contempBMSY")
  asymBMSY
}

getasymFMSY <- function(mse, scen){
  all_years <- seq(mse@OM$CurrentYr[1] - mse@nyears + 1, mse@OM$CurrentYr[1]+mse@proyears)

  asymFMSY <- mse@Hist@Ref$ByYear$FMSY |>
    apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
    as.data.frame() |>
    mutate(year=all_years, scenario=scen) |>
    dplyr::rename(lwr=1, med=`50%`, upr=3) |>
    as.data.frame()|>
    melt(id.vars=c("year","scenario", "lwr","upr","med"), value.name="RefPtValue") |>
    mutate(RefPtName="contempFMSY")
  asymFMSY
}

getBBMSY <- function(mse, scen){
  proyears <- seq(mse@OM$CurrentYr[1] + 1, mse@OM$CurrentYr[1] + mse@proyears)

  B_BMSY <- mse@SB_SBMSY[,1,] |>
    apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
    as.data.frame() |>
    mutate(year=proyears, scenario=scen) |>
    dplyr::rename(lwr=1, med=`50%`, upr=3) |>  as.data.frame()
  B_BMSY
}

# 2. Basic static parameters, different definitions of B0 from OM
# Arguments:
# hist = one scenario from OMscenarios
# scen = scenario name
# age = the age to use for constant M (pass in Mage, set in 0_run_analyses.R)
# updated October 2025 to include the correct mean values of B0 (from getmeanB0 with calc_tv_B0)
# Returns long-form table of annual estimates of the different B0s for plotting pars and comparing assessed values
getPars_allB0 <- function(mse,scen, age=3){

  refpt_levels <- c("hist", "mean", "recent", "dyn")
  all_years <- seq(mse@Hist@Data@OM$CurrentYr[1] - mse@nyears + 1, mse@Hist@Data@OM$CurrentYr[1]+mse@proyears)
  nreps  <- mse@nsim
  nyears <- length(all_years)

  assess_estimates <- get_Assess_Estimates(mse)
  assess_yrs <- unique(assess_estimates$Year_assess)

  # Time varying B0 measures
  # 1. hist mean B0 (this is constant but still need a value every year)
  # get a matrix of mean B0 with dim: nrow=nrep, ncol=nyears
  histB0 <- getmeanB0(mse, scen,age=age, type="hist", quants=FALSE)
  colnames(histB0) <- allyrs
  # transform into long form and add necessary columns
  histB0 <- histB0 |>
    melt() |>
    rename(Sim=Var1, Year_assess=Var2) |>
    mutate(Scenario=scen, B0type="hist") |>
    filter(Year_assess %in% assess_yrs)

  # 2. long-term mean B0
  # get a matrix of mean B0 with dim: nrow=nrep, ncol=nyears
  meanB0 <- getmeanB0(mse, scen,age=age, type="mean", quants=FALSE)
  colnames(meanB0) <- allyrs
  # transform into long form and add necessary columns
  meanB0 <- meanB0 |>
    melt() |>
    rename(Sim=Var1, Year_assess=Var2) |>
    mutate(Scenario=scen, B0type="mean") |>
    filter(Year_assess %in% assess_yrs)

  # 3. recent mean B0
  # get a matrix of mean B0 with dim: nrow=nrep, ncol=nyears
  recentB0 <- getmeanB0(mse, scen,age=age, type="recent", quants=FALSE)
  colnames(recentB0) <- allyrs
  # transform into long form and add necessary columns
  recentB0 <- recentB0 |>
    melt() |>
    rename(Sim=Var1, Year_assess=Var2) |>
    mutate(Scenario=scen, B0type="recent") |>
    filter(Year_assess %in% assess_yrs)

  # 4.dynB0 - from mse object
  # get a matrix of mean B0 with dim: nrow=nrep, ncol=nyears
  dynB0 <- mse@Hist@Ref$Dynamic_Unfished$SSB0
  colnames(dynB0) <- allyrs
  # transform into long form and add necessary columns
  dynB0 <- dynB0 |>
    melt() |>
    rename(Sim=Var1, Year_assess=Var2) |>
    mutate(Scenario=scen, B0type="dyn") |>
    filter(Year_assess %in% assess_yrs)

  # Now bind them all together
  allB0raw <-rbind(histB0, meanB0, recentB0, dynB0) |>
    mutate(B0type=factor(B0type, levels=refpt_levels))

  allB0raw
} #end func


#=================================================================================
# Probabilities (projection years only)
# P(B > 0.4BMSY)
getPLRP_BMSY <- function(mse, scen){
  proyears <- seq(mse@OM$CurrentYr[1] + 1, mse@OM$CurrentYr[1] + mse@proyears)

  B_BMSY <- mse@SB_SBMSY[,1,]
  PLRP <- apply(B_BMSY>0.4,2,mean,na.rm=T) |>
    as.data.frame() |>
    rename(PLRP=(.)) |>
    mutate(year=proyears,scenario=scen)
  PLRP
}

