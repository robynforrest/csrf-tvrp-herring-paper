# ===================================================================================================================================================
# === Pacific Herring: 99 Get various objects from OMs and MSEs in formats for plotting  =============================================================================================
# === Sourced by various files  ===========================================================================
# ===================================================================================================================================================
# Robyn Forrest
# October 13 2025

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
      nreps  <- object@nsim
      OMM_Annual <- object@cpars$M_ageArray[,age,] # dim: nrow=nreps, ncol=nyears
  }# end if
  # First argument is an MSEtool Hist object
  if(input_type=="MSE"){
      all_years <- seq(object@Hist@Data@OM$CurrentYr[1] - object@nyears + 1, object@Hist@Data@OM$CurrentYr[1]+object@proyears)
      nreps  <- object@nsim
      OMM_Annual <- object@Hist@AtAge$N.Mortality[,age,] # dim: nrow=nreps, ncol=nyears
  } # end if

  nyears <- length(all_years)

  if(type=="annual"){
    OMM_mean <- OMM_Annual
  }#end if
  # mean of first 5 years
  if(type=="hist"){
    OMM_mean <- matrix(nrow=nreps, ncol=nyears)
    for(j in 1:nreps){
      for(i in 1:nyears){
        OMM_mean[j,i] <- mean(OMM_Annual[j,1:5]) # same for all years
      }}
  }#end if
  # long-term mean
  if(type=="mean"){
    OMM_mean <- matrix(nrow=nreps, ncol=nyears)
    for(j in 1:nreps){
      OMM_mean[j,1] <- OMM_Annual[j,1]
      for(i in 2:nyears){
        OMM_mean[j,i] <- mean(OMM_Annual[j,1:i])
      }
    }
  } #end if
  # mean of most recent 5 years
  if(type=="recent"){
    OMM_mean <- matrix(nrow=nreps, ncol=nyears)
    for(j in 1:nreps){
      OMM_mean[j,1] <- OMM_Annual[j,1]

      for(i in 2:nyears){
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
    # get M for the two ages specified so it works for all stocks
    Mdat <- OMM_mean %>%
      apply(2,quantile,probs=c(conflo,0.5,confhi)) %>% t() %>%
      as.data.frame() %>%
      mutate(year=all_years, scenario=scen) %>%
      dplyr::rename(lwr=1, med=`50%`, upr=3) %>%  as.data.frame()
    return(Mdat)
  } #end ifelse
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




