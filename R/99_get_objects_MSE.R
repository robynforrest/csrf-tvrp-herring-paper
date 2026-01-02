# ===================================================================================================================================================
# === Pacific Herring: 99 Get various objects from MSEs in formats for plotting  =============================================================================================
# Model estimates and performance metrics
# === Sourced by various files  ===========================================================================
# ===================================================================================================================================================
# Robyn Forrest
# October 13 2025

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

###########################################################################################
# Parameter and reference point estimates
###########################################################################################
######################################################################################
# 1. Assessment: Time-varying parameters
#      - only contemporary terminal years
# Uses output object from openMSE get_Assess_Estimates(MSEobject)
getPars_assess_tv_terminal <- function(assess_est, year1) {

  # for time varying objects, remove ten burnin years for recruitment
  # Add SSB later because we want to get SSBt+1
  assess_est_tv <- assess_est |>
    filter(Year_est>=year1)|>
    filter(Variable %in% c("dynamic_SSB0", "FMort","M", "R"))

  assess_yrs <- unique(assess_est$Year_assess)
  cur_yr <- min(assess_yrs)

  # we just want the current year in each assessment year, except SSB, where we want
  # the  projection year, i.e., Year_est = Year_assess+1
  for(kk in assess_yrs){
    tmp <- assess_est_tv |>
      filter(Year_assess==kk) |>
      filter(Year_est==kk)
    if(kk==cur_yr) {
      assess_est_terminal <- tmp
    }else{
      assess_est_terminal <- rbind(assess_est_terminal,tmp)
    }
  } # end for loop

  # Now add SSB_T+1
  assess_est_tv_ssb <- assess_est |>
    filter(Year_est>=year1)|>
    filter(Variable %in% c("SSB"))

  for(kk in assess_yrs){
    tmp <- assess_est_tv_ssb |>
      filter(Year_assess==kk) |>
      filter(Year_est==kk+1)
    assess_est_terminal <- rbind(assess_est_terminal,tmp)
  } # end for loop

  assess_est_terminal

}# end function

# 2. Assessment: static parameters
getPars_assess_static <- function(assess_est){
  assess_estimates_static <- assess_est |>
    filter(!Variable %in% c("FMort" ,"SSB", "R", "VB", "conv",
                            "dynamic_SSB0", "Index_1", "Index_2", "M", "SSB0"))
  assess_estimates_static

}# end function

# 3. getPars_assess_B0 - this is the terminal value of SSB0 obtained from the appropriate
#    averaged years. It is kicked out by the new MP as part of the Rec@Misc object
getPars_assess_B0 <- function(mse, scen){
  assess_B0 <- data.frame(matrix(ncol=6, nrow=0))
  colnames(assess_B0) <- c("Year_assess", "Variable","Value","MP","Sim","Scenario")
  mps <- mse@MPs
  nmps < length(mps)
  nsim <- mse@nsim

  for(ii in 2:nmps){
    for(jj in 1:nsim){


    }
  }


}

#############################################################################################
# Time-varying parameters - all estimated years - summary quantiles
getPars_assess_summary_annual <- function(assess_est, year1) {

  # for time varying objects, remove ten burnin years for recruitment
  assess_est <- assess_est |>
    filter(Year_est>=year1)

  Summary <- assess_est |>
    filter(Variable %in% c("FMort" ,"SSB", "R", "dynamic_SSB0", "M")) |>
    spread(Variable, Value) |>
    group_by(Scenario, MP,Year_assess,Year_est)|>
    summarise(SSB_l=quantile(as.numeric(SSB), probs=conflo, na.rm=T),
              SSB_m=quantile(as.numeric(SSB), probs=0.50, na.rm=T),
              SSB_u=quantile(as.numeric(SSB), probs=confhi, na.rm=T),
              Recruit_l=quantile(as.numeric(R), probs=conflo, na.rm=T),
              Recruit_m=quantile(as.numeric(R), probs=0.50, na.rm=T),
              Recruit_u=quantile(as.numeric(R), probs=confhi, na.rm=T),
              FMort_l=quantile(as.numeric(FMort), probs=conflo, na.rm=T),
              FMort_m=quantile(as.numeric(FMort), probs=0.50, na.rm=T),
              FMort_u=quantile(as.numeric(FMort), probs=confhi, na.rm=T),
              M_l=quantile(as.numeric(M), probs=conflo, na.rm=T),
              M_m=quantile(as.numeric(M), probs=0.50, na.rm=T),
              M_u=quantile(as.numeric(M), probs=confhi, na.rm=T),
              dynB0_l=quantile(as.numeric(dynamic_SSB0), probs=conflo, na.rm=T),
              dynB0_m=quantile(as.numeric(dynamic_SSB0), probs=0.50, na.rm=T),
              dynB0_u=quantile(as.numeric(dynamic_SSB0), probs=confhi, na.rm=T))
  Summary
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

# Get OM SSB, F, R and M for assessment years
getVars_om_assessyrs <- function(msescenarios,
                                 assessyrs){

  curyr <- min(assessyrs)

  # OM: projected SSB, F, Recruits and M
  omSSB <- get_SSB(msescenarios) |>
    mutate(Variable ="SSB") |>
    rename(Year_assess=Year, Scenario=Model) |>
    filter(!Period %in% "Historical")
  omF <- get_F(msescenarios) |>
    mutate(Variable="FMort")|>
    rename(Year_assess=Year, Scenario=Model)|>
    filter(!Period %in% "Historical")
  # get_Recruits not working - ask AH
   # omRecruits <- get_Recruits(msescenarios)|>
   #   mutate(Variable="R")|>
   #   rename(Year_assess=Year, Scenario=Model)|>
   #   filter(!Period %in% "Historical")
  omM <- get_LifeHistory(msescenarios) |>
    filter(Variable %in% "M")|>
    rename(Year_assess=Year, Scenario=Model)

  mps <- unique(omSSB$MP)

  omSSB_raw <- get_SSB(msescenarios)
  write_csv(omSSB,file.path(StockDirLRR,"OM_SSB_MP_raw.csv"))

  omF_raw <- get_F(msescenarios)
  write_csv(omF,file.path(StockDirLRR,"OM_F_MP_raw.csv"))

  # for comparison with terminal assessed estimates (Recruits and F)
  # om_proj_vars_F_R <- rbind(omF, omRecruits) |>
  #   filter(Year_assess %in% assessyrs,
  #          !MP %in% "NFref") |>
  #   filter(Year_assess>curyr) |>
  #   select(Year_assess, Sim, Scenario, MP, Variable, Value)

  om_proj_vars_F <- omF |>
    filter(Year_assess %in% assessyrs,
           !MP %in% "NFref") |>
    filter(Year_assess>curyr) |>
    select(Year_assess, Sim, Scenario, MP, Variable, Value)

  # for SSB, the comparison is with terminalyear+1 assessed estimates
  # this is what SAMtool uses in the HCR
  om_proj_vars_SSB <- omSSB |>
    filter(Year_assess %in% (assessyrs+1), #get the value for terminal year + 1
           !MP %in% "NFref") |>
    mutate(Year_assess=(Year_assess-1)) |> # set the year back to assessment year
    filter(Year_assess>curyr) |>
    select(Year_assess, Sim, Scenario, MP, Variable, Value)

  om_proj_vars <- rbind(om_proj_vars_SSB,om_proj_vars_F)

  # Add M. Need to artificially add MPs so plots work (M same over MPs)
  omM_proj <- omM |>
    filter(Year_assess>curyr) |>
    filter(Year_assess %in% assessyrs) |>
    mutate(MP=mps[2]) # add a fake MP field. Skip NFref MP

  # Now copy all the M values with a new fake MP, for all the other MPs
  for(ii in 3:nMPs){
    tmp <- omM |>
      filter(Year_assess>curyr) |>
      filter(Year_assess %in% assessyrs) |>
      mutate(MP=mps[ii]) # add a fake MP field. Skip NFref MP
    omM_proj <- rbind(omM_proj,tmp)
  }

  omM_proj <- omM_proj |>
    select(Year_assess, Sim, Scenario, MP, Variable, Value)

  om_proj_vars <- om_proj_vars |>
    rbind(omM_proj)

  om_proj_vars
} # end func

#########################################################################################
# Relative errors in parameter estimates
# A messy function that matches up the assessed and OM parameters
# Importantly, it matches the definitions of B0 with the MP
# Not interested in cross referencing estimates of B0 with different MPs
# e.g., dont care about dynB0 estimates from histB0 MP
# scen = scenario names, yr1=first historic year
# assesstv_terminal, assesstv_static, omtv_vars, ompars_static, omB0 are
#   all calculated in 2c_plot-pac-herring-mse-mps.R
getPars_rel_error <- function(scen,
                              assesstv_terminal,
                              assessest_static,
                              omtv_vars,
                              ompars_static,
                              omB0,
                              yr1,
                              species="pac-herring"){

  scenames <- scen
  assess_yrs <- unique(assesstv_terminal$Year_assess)
  nassess_yr <- length(assess_yrs)
  nsim <- length(unique(assesstv_terminal$Sim))
  mps <- unique(assesstv_terminal$MP) # this skips the NFref MP bc no assessment
  nmps <- length(mps)
  species <- species
  yr1 <- yr1
  refpt_levels <- c("hist", "mean", "recent", "dyn")

  #*Contemporary terminal* estimates of time-varying parameters
  #*Includes SSB0tv, which is kicked out of the MP to the mse object
  #*This version is calculated with the estimated average M and fec value for each year appropriate to each MP
  assesstv_terminal <- assesstv_terminal |>
    filter(Year_assess>min(assess_yrs)) |> # don't want curyr
    mutate(Variable=replace(Variable, Variable=="SSB0tv", "SSB0")) # rename the custom SSB0 from the MP

  # TEST: check that the custom SSB0 for the dynamic MP matches the default reported dynamic_SSB0
  # YES (apart from years where assess model did not converge, when MP returns SSB0=NA)
  # Therefore can now delete dynamicSSB0 from assesstv_terminal
  # tmp <- assesstv_terminal |>
  #   filter(MP=="Dyn60.30.02", Variable %in% c("dynamic_SSB0", "SSB0"),
  #          Year_assess==assess_yrs[nassess_yr]) |>
  #   ggplot()+
  #   geom_boxplot(aes(x=Variable,y=Value, fill=Variable)) +
  #   facet_wrap(vars(Scenario))
  # tmp

  # Can remove dyn_SSB0 because it is already reported as SSB0
  # Remove R because not working for OM (see function getVars_om_assessyrs)
  assesstv_terminal <- assesstv_terminal |>
    filter(!Variable %in% c("dynamic_SSB0","R"))

  #*Annual estimates of static (time-invariant)* model parameters
  assessestimates_static <- assessest_static |>
    filter(Year_assess>min(assess_yrs))   # don't want curyr
  # time-varying variables from OM
  omtv_vars <- omtv_vars |>
    filter(Year_assess>min(assess_yrs)) # don't want curyr
  #*Annual B0 from OM
  omB0 <- omB0 |>
    filter(Year_assess>min(assess_yrs)) # don't want curyr

  # For omB0, need to match up omB0 for with MPs
  # The assessment estimates an SSB0
  # The MP tells it how SSB0 is calculated
  # So for the RE plots, we can match my four versions of SSB0
  #   with their MPs
  #   THIS DEPENDS ON THE ORDER OF REF_PT LEVELS BEING THE SAME ORDER AS MPS!
  for(ii in 1:nmps) {
    tmp <- omB0 |>
      filter(B0type==refpt_levels[ii]) |>  #filter so that we only keep the SSB0 associated with each MP
      rename(omValue=value) |>
      mutate(MP=mps[ii],
             Variable="SSB0")
          if(ii==1){
      omB0_all<- tmp
    }else{
      omB0_all<- rbind(omB0_all,tmp)
    }
  }

  # For ompars_static_all, need to repeat ompars_static_all
  # for all MPs and Years_assess so that tables can be joined
  for(ii in 1:nmps) {
    for(jj in 1:nassess_yr){
      tmp <- ompars_static |>
        mutate(MP=mps[ii],
               Year_assess = assess_yrs[jj]) |>
        rename(omValue=Value)
      if(jj==1){
        tmpMP <- tmp
      }else{
        tmpMP <- rbind(tmpMP,tmp)
      } # end ifelse
    }# #end jj
    if(ii==1){
      ompars_static_all <- tmpMP # Contains all years and first MP
    }else{
      tmpYr <- tmpMP |>
        mutate(MP=mps[ii])
      ompars_static_all <- rbind(ompars_static_all,tmpYr)
    } # end ifelse
  }# end ii

  ompars_static_all <- ompars_static_all |>
    filter(Year_assess>min(assess_yrs)) |>
    mutate(Variable=as.character(Variable)) # remove factors before join

  # OK now we should have two dataframes for assessed and OM values
  # where each parameter has a value for each
  # sim, assessment year and MP, where
  # values are repeated if they are constant across
  # assess years and MPs in the OM
  all_assessPar <- assesstv_terminal |>
    rbind(assessestimates_static) |>
    rename(asValue=Value)
  all_omPar <- omB0_all |>
    select(-B0type) |>
    rbind(ompars_static_all) |>
    rbind(omtv_vars) |> # join with SSB,F,R,M in assess years
    select(Year_assess, Sim, Scenario, MP, Variable,omValue)

  # Make sure the parameters match up
  # unique(sort(all_assessPar$Variable))
  # unique(sort(all_omPar$Variable))
  # head(all_assessPar)
  # head(all_omPar)

  # Now join them and calculate relative error
  all_pars <- all_assessPar |>
    left_join(all_omPar,
              by=c("Scenario","Year_assess","MP","Sim","Variable")) |>
    select(Scenario,Year_assess,MP,Sim,Variable,asValue,omValue) |>
    mutate(relError=((asValue-omValue)/omValue))

  all_pars
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

