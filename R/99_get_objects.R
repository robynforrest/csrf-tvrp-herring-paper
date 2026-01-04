# ===================================================================================================================================================
# === Pacific Herring: 99 Get various objects from OMs and MSEs in formats for plotting  =============================================================================================
# === Sourced by various files  ===========================================================================
# ===================================================================================================================================================
# Robyn Forrest
# October 13 2025
# Modified January 2 2026

###################################################################################################################
# Objects from the OM
###################################################################################################################
# R0 - needed for B0 calculations - used only to get SRalpha and SRbeta
# Arguments:
# om = OM object for one stock and one scenario (saved in OMScenarios.rda for each stock)
# Returns: a vector of nsim values of R0
getR0 <- function(histmse){

  OMR0 <- histmse@OMPars$R0
  OMR0
}
###################################################################################################################

###################################################################################################################
# Natural mortality - assumes that adult M is constant across ages
# Arguments:
# om = OM object with alternative future M scenarios for one stock and one scenario (saved in OMScenarios.rda for each stock)
# scen = scenario name (usually one of ScenarioNamesHuman.rda)
# age = the age of constant adult M - function only considers a single value of M for each year
# type = how M is averaged over the time series, as used in alternative B0 calculations:
#        annual: This is just the annual estimate of M (used in assessment and dynamic B0 calcs)
#        hist: This is the mean annual estimate of M from years 1:5 (used in Hist B0 calcs)
#        mean: This is the long-term mean estimate of M (used in mean B0 calcs)
#        recent: This is recent mean estimate of M from the most recent 5 years (used in recent B0 calcs)
# input_type = "OM" or "MSE". Whether the input is an OM object or a MSE object
# Returns:
#          if quants=T: a dataframe with lower, median and upper estimates of M for the scenario
#          if quants=F: a matrix of mean M with dim(nreps, nyears)
getM <- function(object, scen,age=3, type="annual", quant=FALSE, input_type="OM"){

  # First argument is an MSEtool OM object
  if(input_type=="OM"){
    all_years <- seq(object@CurrentYr - object@nyears + 1, object@CurrentYr+object@proyears)
    nyrs <- length(all_years) #total number of years including projection
    pyrs <- (object@nyears+1):nyrs
    nreps  <- object@nsim
    OMM_Annual <- object@cpars$M_ageArray[,age,] # dim: nrow=nreps, ncol=nyears
  }# end if

  # First argument is an MSE object
  if(input_type=="MSE"){
    all_years <- seq(object@Hist@Data@LHYear - object@nyears + 1, object@Hist@Data@LHYear+object@proyears)
    nyrs <- length(all_years) #total number of years including projection
    nreps  <- object@nsim
    OMM_Annual <- object@Hist@AtAge$N.Mortality[,age,] # dim: nrow=nreps, ncol=nyears
  } # end if

  if(type=="annual"){
    OMM_mean <- OMM_Annual
    # get traces for projection years for plot
    set.seed(3)
    if(nsim<5){
      ntrace <- nsim
    } else{
      ntrace <- 5
    }
    traceSample <- sample(nsim,ntrace, replace=F)
    traces <- OMM_mean[traceSample,] |> t() |>
      as.data.frame() |>
      rename(Trace1=V1,Trace2=V2,Trace3=V3,Trace4=V4,Trace5=V5)
  }#end if

  # mean of first 5 years
  if(type=="hist"){
    OMM_mean <- matrix(nrow=nreps, ncol=nyrs)
    for(j in 1:nreps){
      for(i in 1:nyrs){
        OMM_mean[j,i] <- mean(OMM_Annual[j,1:5]) # same for all years
      }}
  }#end if
  # long-term mean
  if(type=="mean"){
    OMM_mean <- matrix(nrow=nreps, ncol=nyrs)
    for(j in 1:nreps){
      OMM_mean[j,1] <- OMM_Annual[j,1]
      for(i in 2:nyrs){
        OMM_mean[j,i] <- mean(OMM_Annual[j,1:i])
      }
    }
  } #end if
  # mean of most recent 5 years
  if(type=="recent"){
    OMM_mean <- matrix(nrow=nreps, ncol=nyrs)
    for(j in 1:nreps){
      OMM_mean[j,1] <- OMM_Annual[j,1]

      for(i in 2:nyrs){
        if(i <= 4) OMM_mean[j,i] <- mean(OMM_Annual[j,1:i])
        if(i > 4)  OMM_mean[j,i] <- mean(OMM_Annual[j,(i-4):i])
      }
    }
  } #end if

  if(!type %in% c("annual","hist","mean","recent")){
    stop("type must be one of annual,hist,mean or recent \n")
  }

  if(quant==FALSE){
    return(OMM_mean)
  }else{
    if(input_type=="MSE"){
      # get M for the two ages specified so it works for all stocks
      Mdat <- OMM_mean %>%
        apply(2,quantile,probs=c(conflo,0.5,confhi)) %>% t() %>%
        as.data.frame() %>%
        mutate(year=all_years, scenario=scen, Mtype=type) %>%
        dplyr::rename(lwr=1, med=`50%`, upr=3) %>%  as.data.frame()
      return(Mdat)
    }else{
          Mdat <- OMM_mean %>%
          apply(2,quantile,probs=c(conflo,0.5,confhi)) %>% t() %>%
          as.data.frame() %>%
          mutate(year=all_years, scenario=scen, Mtype=type) %>%
          dplyr::rename(lwr=1, med=`50%`, upr=3) %>%  as.data.frame()

          if(type=="annual"){
            Mdat <- Mdat |>
              cbind(traces)
          }
        return(Mdat)
    } #end ifelse(MSE)
  } #end ifelse (quant)
}# end function
###################################################################################################################

###################################################################################################################
# Fecundity - needed for B0 calculations
# Arguments:
# om = OM object with alternative future wa scenarios for one stock and one scenario (saved in OMScenarios.rda for each stock)
# scen = scenario name (usually one of ScenarioNamesHuman.rda)
#    currently wa is the same for all future scenarios but this could change
# type = how fec-at-age is averaged over the time series, as used in alternative B0 calculations:
#        annual: This is just the annual estimate of fec-at-age (used in assessment and dynamic B0 calcs)
#        hist: This is the mean annual estimate of fec-at-age from years 1:5 (used in Hist B0 calcs)
#        mean: This is the long-term mean estimate of fec-at-age (used in mean B0 calcs)
#        recent: This is recent mean estimate of fec-at-age from the most recent 5 years (used in recent B0 calcs)
# input_type = "OM" or "MSE". Whether the input is an OM object or a MSE object
# Returns: a matrix of annual fecundity-at-age for the scenario nrow=nage, ncol=nyear
getFec <- function(object, scen , type="annual", input_type="OM"){

  # First argument is an MSEtool OM object
  if(input_type=="OM"){
    all_years <- seq(object@CurrentYr - object@nyears + 1, object@CurrentYr+object@proyears)
    # Note that wt_age and mat_age (dim=c(nrep,nage,nyear))are time-varying but are identical for each replicate in each year
    #   Therefore only need first replicate each year
    OMwa_Annual <- object@cpars$Wt_age[1,-c(1,2),] # weight at age, cut off age 0 and 1
    OMma_Annual <- object@cpars$Mat_age[1,-c(1,2),] # maturity at age, cut off age 0 and 1
  }# end if
  # First argument is an MSEtool Hist object
  if(input_type=="MSE"){
    all_years <- seq(object@Hist@Data@OM$CurrentYr[1] - object@nyears + 1, object@Hist@Data@OM$CurrentYr[1]+object@proyears)
    # Note that wt_age and mat_age (dim=c(nrep,nage,nyear))are time-varying but are identical for each replicate in each year
    #   Therefore only need first replicate each year
    OMwa_Annual <- object@Hist@AtAge$Weight[1,-c(1,2),] # weight at age, cut off age 0 and 1
    OMma_Annual <- object@Hist@AtAge$Maturity[1,-c(1,2),] # maturity at age, cut off age 0 and 1
  } # end if

  OMfa_Annual <- OMwa_Annual*OMma_Annual # ages 2:10, dim: nrow=nages ncol=nyears
  nage   <- nrow(OMfa_Annual)
  nyears <- length(all_years)

  if(type=="annual"){
    OMfa_mean <- OMfa_Annual
  }
  if(type=="hist"){
    OMfa_mean <- matrix(0,nrow=nage, ncol=nyears)
    for(i in 1:nyears){
      OMfa_mean[,i] <- apply(OMfa_Annual[,1:5],1,mean)
    }
  }
  if(type=="mean"){
    OMfa_mean <- matrix(0,nrow=nage, ncol=nyears)
    OMfa_mean[,1] <- OMfa_Annual[,1]
    for(i in 2:nyears){
      OMfa_mean[,i] <- apply(OMfa_Annual[,1:i],1,mean)
    }
  }
  if(type=="recent"){
    OMfa_mean <- matrix(0,nrow=nage, ncol=nyears)
    OMfa_mean[,1] <- OMfa_Annual[,1]
    for(i in 2:nyears){
      if(i <= 4) OMfa_mean[,i] <- apply(OMfa_Annual[,1:i],1,mean)
      if(i > 4) OMfa_mean[,i] <- apply(OMfa_Annual[,(i-4):i],1,mean)
    }
  }
  if(!type %in% c("annual","hist","mean","recent")){
    stop("type must be one of annual,hist,mean or recent \n")
  }

  colnames(OMfa_mean) <- all_years
  OMfa_mean

} # end function

###################################################################################################################
# Recruitment errors
getperry <- function(om, scen){
  all_years <- seq(om@CurrentYr - om@nyears + 1, om@CurrentYr+om@proyears)
  # Need to add on the devs for the 10 years prior to the start of the series (if maxage=10)
  maxage <- om@maxage
  preyr1 <- all_years[1]-maxage
  preyrs <- preyr1:(all_years[1]-1)

  perr_y <- om@cpars$Perr_y %>%
    apply(2,quantile,probs=c(conflo,0.5,confhi)) %>% t() %>%
    as.data.frame() %>%
    mutate(year=c(preyrs,all_years), scenario=scen) %>%
    dplyr::rename(lwr=1, med=`50%`, upr=3) %>%  as.data.frame()
  perr_y
}
###################################################################################################################

###################################################################################################################
# Objects from the MSEs
##############################################################################################################################################################
# Arguments:
# mse = MSE object with alternative future M scenarios for one stock and one scenario
# scen = scenario name (usually one of ScenarioNamesHuman.rda)
# mp = the mp index
# Returns: a dataframe with lower, median and upper estimates of annual SSB for the scenario
getSSB <- function(mse, scen, mp=1){
  all_years <- seq(mse@OM$CurrentYr[1] - mse@nyears + 1, mse@OM$CurrentYr[1]+mse@proyears)

  # Historical SSB is split into nareas (minimum 2). In this case they are equal but
  # need to be added together
  #histSSB <- mse@Hist@TSdata$SBiomass[,,1] + mse@Hist@TSdata$SBiomass[,,2]
  histSSB <- mse@SSB_hist # same as above but cleaner
  proSSB  <- mse@SSB[,mp,]

  SSB <- cbind(histSSB, proSSB)

  SSB <- SSB |>
    apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
    as.data.frame() |>
    mutate(year=all_years, scenario=scen) |>
    dplyr::rename(lwr=1, med=`50%`, upr=3) |>
    as.data.frame() |>
    melt(id.vars=c("year","scenario", "lwr","upr","med"), value.name="SSB")
  SSB
}

getSSBrel <- function(mse, scen, mp=1){
  all_years <- seq(mse@OM$CurrentYr[1] - mse@nyears + 1, mse@OM$CurrentYr[1]+mse@proyears)

  histSSB <- mse@SSB_hist
  proSSB  <- mse@SSB[,mp,] # specify the first slice (only 1 MP here)

  SSB <- cbind(histSSB, proSSB)
  SSB <- SSB/SSB[1]

  SSB <- SSB |>
    apply(2,quantile,probs=c(conflo,0.5,confhi)) |> t() |>
    as.data.frame() |>
    mutate(year=all_years, scenario=scen) |>
    dplyr::rename(lwr=1, med=`50%`, upr=3) |>  as.data.frame()
  SSB
}

############################################################################################
# Get OM parameters of interest
# 1. Basic static parameters, excluding B0
getPars_om_static <- function(mse, scenam, species="pac-herring") {
  sim <- mse@nsim
  ompars <- data.frame("Scenario"=scenam,
                       "Sim"=1:nsim,
                       "h"= mse@OM$hs,
                       "R0"=mse@OM$R0/1000,
                       "q1"=mse@OM$qs,
                       "q2"=mse@OM$qs,
                       "SSBMSY"=mse@OM$SSBMSY/1000,
                       "MSY"=mse@OM$MSY/1000,
                       "FMSY"=mse@OM$FMSY) |>
    reshape2::melt(id.vars=c("Sim","Scenario"),
                   value.name = "Value", variable.name="Variable")

  ompars
}

#########################################################################################
# Performance metrics
# get perf metric just for projection period
# ONLY LOOK AT CASES WHERE MPS AND B0 TYPE IS MATCHED
# the three mean B0 cases (hist, mean, recent)
getPLRP_B0 <- function(mse, scen, age=3, type="mean", mp=2){
  proyears <- seq(mse@OM$CurrentYr[1] + 1, mse@OM$CurrentYr[1] + mse@proyears)

  SSB <- mse@SSB[,mp,] # just get SSB for the MP that matches the B0 type

  B0 <- getmeanB0(mse, scen, age=age, type=type, quants=FALSE)
  B0 <- B0[,(mse@nyears+1):(mse@nyears+mse@proyears)] #proyears
  B_B0<-SSB/B0

  PLRP <- apply(B_B0>0.3,2,mean,na.rm=T) |>
    as.data.frame() |>
    rename(PLRP=(1)) |>
    mutate(year=proyears,scenario=scen, b0type=type)
  PLRP
}
# Dynamic B0
getPLRP_dynB0 <- function(mse, scen, mp=5){
  proyears <- seq(mse@OM$CurrentYr[1] + 1, mse@OM$CurrentYr[1] + mse@proyears)
  yind <- mse@nyears+(1:mse@proyears)

  SSB <- mse@SSB[,mp,]
  dynB0 <- mse@Hist@Ref$Dynamic_Unfished$SSB0[,yind]
  B_dynB0<-SSB/dynB0

  PLRP <- apply(B_dynB0>0.3,2,mean,na.rm=T) |>
    as.data.frame() |>
    rename(PLRP=(1)) |>
    mutate(year=proyears,scenario=scen, b0type="dyn")
  PLRP
}



