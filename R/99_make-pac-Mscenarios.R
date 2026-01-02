# ===================================================================================================================================================
# === Pacific Herring: 2 make the M scenarios  =============================================================================================
# Functions to update future M scenarios (constant,  random walk with increase drift,random walk with decrease drift)
# These are sourced in 1_make-pac-herring-oms.R
# Author: Robyn Forrest. August 2024.
# ===================================================================================================================================================

# --- Load Herring OM --------------------
# TODO:

  # Now build alternative OMs with different future M trajectories
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Scenarios can be "ConstantM" (not used), "RandomWalkM", "RandomWalkMDriftIncr","RandomWalkMDriftDecr"
  # alphaminc, alphamdec and sigm are the parameters of the random walk, set in 0_run-analyses.R
  make_pac_MScenarios <- function(om,
                              Scenario="RandomWalkM",
                              alphaminc=0.01, # random walk parameter increasing drift
                              alphamdec=-0.01, # random walk parameter decreasing drift
                              sigm=0.02,    # random walk parameter
                              sdn=2, # nyears-sdn is the basis for setting sd among reps in projections
                              knotsdiv=5,   # how many years to divide proyears for splines
                              species,
                              stock) {

    nsim <- om@nsim
    nyears <- om@nyears
    nproyears <- om@proyears
    startyr <- om@CurrentYr-nyears+1
    maxage <- om@maxage
    years <- 1:nyears
    proyears <- (nyears+1):(nyears+nproyears)
    allyears <- c(years, proyears)
    # Actual years
    yrs <- (startyr-1) + years
    all_years <- seq(om@CurrentYr - om@nyears + 1, om@CurrentYr+om@proyears)
    pyrs <- yrs[nyears]+(1:nproyears)
    pyr1 <- pyrs[1]
    # for traces on M plots
    set.seed(3)
    traceSample <- sample(nsim,5, replace=F)
    # number of knots for splines
    numknots <- as.integer(nproyears/knotsdiv)

    # For sampling future M with fixed sd. To avoid M sd blowing up
    rnorm_fixed = function(n, mu=0, sigma=1) {
      x = rnorm(n)  # from standard normal distribution
      x = sigma * x / sd(x)  # scale to desired SD
      x = x - mean(x) + mu  # center around desired mean
      return(x)
    }

    # 1. start a new Mscenario, identical to the base OM
    if(Scenario=="ConstantM"){

      cat("Running Constant M Scenario\n")
      # Constant M in the proj years
      # Technically we don't need to do anything for ConstantM because
      # the default M projection is constant
      # BUT we want to resample the projected M with smaller sd because
      #  the final historical year had over-inflated sd that we don't
      #  want to project into the future
      Mscenario <- om
      M_age <- Mscenario@cpars$M_ageArray
      #matplot(t(M_age[,3,]), type="l", ylim=c(0,1.8))

      # for some reason, there is a step from nyrs to nyrs+1 in the
      #  default OM from MSEtool
      #  The iscam mcmc files only have M up to nyrs so not sure why this happens
      #    M_age[1,3,nyears:(nyears+1)] # they should be the same
      # First, correct this - M is constant so do for all proj years
      M_age[,3,proyears] <- M_age[,3,nyears]
      #matplot(t(M_age[,3,]), type="l", ylim=c(0,1.8))

      # Make a matrix of projected M for one age only
      # (pick age 3, they are all the same for a>2 - copy to other ages later)
      mt_pro <- M_age[,3,proyears] #matrix(0,nrow=nsim, ncol=nproyears)

      # Now we want to resample these projected Ms with smaller sd because
      # the final historical year had over-inflated sd that we don't want to project into the future
      # For resampling, use the sd across replicates from sdn years before last historical year (set in run-analyses)
      sdm <- sd(M_age[,3,nyears-sdn])#  Or could fix it, e.g., sdm=0.08
      cat("Sdm = ",sdm,"\n")
      for(ii in 1:nproyears){
       # Get mean M for that year across replicates
        meanm <- mean(mt_pro[,ii])

        # Now get nsim new values based on the mean but with smaller sd
        set.seed(9)
        mt_pro[,ii] <- rnorm(nsim, mean=meanm, sd=sdm)
      }

      # Now add to the M_age array for all the age slices
      # they are all identical because M is constant across all ages >= 2 (slot 3 for Pac herring)
      for(jj in 3:(maxage+1)){
        M_age[,jj,proyears] <- mt_pro
       } # end for jj

      # How does it look (just plot one age, they are the same for all ages >2)
      #matplot(t(M_age[,3,]), type="l", ylim=c(0,1.8))

      # Add to cpars in the OM
      Mscenario@cpars$M_ageArray <- M_age

      # Update the OM name
      Mscenario@Name <- paste0(stock,"_",Scenario)

  }else if(Scenario == "RandomWalkM"){
      cat("Running Random Walk M Scenario\n")

      # Use eq 3 of Jiao et al 2012 (no drift or trend)
        # ICES Journal of Marine Science, 69, 105–118. doi:10.1093/icesjms/fsr184
        # ln(Mt) = ln(Mt-1) + epsilonM
        # epsilonM ~ N(0,sigM)

    Mscenario <- om
    M_age <- Mscenario@cpars$M_ageArray

    # for some reason, there is a step down from nyrs to nyrs+1 in the
    #  default OM from MSEtool
    #  The iscam mcmc files only have M up to nyrs so not sure why this happens
    #    M_age[1,3,nyears:(nyears+1)] # they should be the same
    # First, correct this
    M_age[,3,proyears] <- M_age[,3,nyears]
    #matplot(t(M_age[,3,]), type="l", ylim=c(0,1.8))

    # Get a matrix to put the projected M values:
    #   mt_pro is a matrix with 1:nsim rows and 1:nproyears cols
    # just do for one age slice as they will all be the same, then copy later
    #   -- pick age 3
    mt_pro <-  M_age[,3,proyears] #matrix(0,nrow=nsim, ncol=nproyears)
    #matplot(t(mt_pro),type="l", ylim=c(0,1.8))

    # Now we want to resample the projected M with smaller sd because
    #  the final historical year had over-inflated sd that we don't
    #  want to project into the future
    # For resampling, use the sd across replicates from sdn years
    #  before last historical year
    sdm <- sd(M_age[,3,nyears-sdn])
    #sdm <- 0.01 # RF just hardwiring for now
    cat("Sdm = ",sdm,"\n")
    for(ii in 1:nproyears){
      # Get mean M for that year across replicates
      meanm <- mean(mt_pro[,ii])

      # Now get nsim new values based on the mean but with smaller sd
      set.seed(9)
      mt_pro[,ii] <- rnorm(nsim, mean=meanm, sd=sdm)
    }
    #matplot(t(mt_pro),type="l", ylim=c(0,1.8))

    # For each replicate sim, get the annual M values using a random walk
    # Apply function didn't work well for this, just use a loop, only done once
    for(k in 1:nsim){
      logm <- vector(length=nproyears)
      epsilonm <- rnorm(nproyears,0,sigm)
      logm[1] <- log(mt_pro[k,1]) #log(Mscenario@cpars$M_ageArray[k,3,nyears])

      for(ii in 2:nproyears){
        logm[ii] <- logm[ii-1]+ epsilonm[ii]
      } # end ii

      m <- exp(logm)
      mt_pro[k,] <- m  # this is age 3 for nyear (cols) across nsims (rows)

      #Smooth
      if(msplines==T){
        # Smooth the resampled RW with splines
        mt_pro[k,] <- stats::smooth.spline(mt_pro[k,],
                                           nknots=numknots)$y
      }
    }# end k
    #matplot(t(mt_pro),type="l", ylim=c(0,1.8))

    # Now add to the M_age array for all the age slices
    # they are all identical because M is constant across all ages >= 2 (slot 3 for Pac herring)
    for(jj in 3:(maxage+1)){
      M_age[,jj,proyears] <- mt_pro
    } # end for jj

    #matplot(t(M_age[,3,]), type="l", ylim=c(0,1.8))

    # Update proyears cpars in the OM
    Mscenario@cpars$M_ageArray <- M_age

    # Update the OM name
    Mscenario@Name <- paste0(stock,"_",Scenario)


   }else if(Scenario == "RandomWalkMDriftIncr"){
      cat("Running Random Walk M Drift Increasing Scenario\n")

      # A random walk with drift in the projection years
        # https://www.investopedia.com/articles/trading/07/stationary.asp

      Mscenario <- om
      M_age <- Mscenario@cpars$M_ageArray

      # for some reason, there is a step down from nyrs to nyrs+1 in the
      #  default OM from MSEtool
      #  The iscam mcmc files only have M up to nyrs so not sure why this happens
      #    M_age[1,3,nyears:(nyears+1)] # they should be the same
      # First, correct this
      M_age[,3,proyears] <- M_age[,3,nyears]
      #matplot(t(M_age[,3,]), type="l", ylim=c(0,1.8))

      # Get a matrix to put the projected M values:
      #   mt_pro is a matrix with 1:nsim rows and 1:nproyears cols
      # just do for one age slice as they will all be the same, then copy later
      #   -- pick age 3
      mt_pro <-  M_age[,3,proyears] #matrix(0,nrow=nsim, ncol=nproyears)
      #matplot(t(mt_pro),type="l", ylim=c(0,1.8))

      # Now we want to resample the projected M with smaller sd because
      #  the final historical year had over-inflated sd that we don't
      #  want to project into the future
      # For resampling, use the sd across replicates from sdn years
      #  before last historical year
      sdm <- sd(M_age[,3,nyears-sdn])
      #sdm <- 0.05 # RF just hardwiring for now
      cat("Sdm = ",sdm,"\n")
      for(ii in 1:nproyears){
        # Get mean M for that year across replicates
        meanm <- mean(mt_pro[,ii])

        # Now get nsim new values based on the mean but with smaller sd
        set.seed(9)
        mt_pro[,ii] <- rnorm(nsim, mean=meanm, sd=sdm)
      }
      #matplot(t(mt_pro),type="l", ylim=c(0,1.8))

      # Now, for each replicate sim, get the annual M values using a random walk (increasing drift)
      for(k in 1:nsim){
          logm <- vector(length=nproyears)
          epsilonm <- rnorm(nproyears,0,sigm)
          #logm[1] <- log(Mscenario@cpars$M_ageArray[k,3,nyears])
          logm[1] <- log(mt_pro[k,1])

          for(ii in 2:nproyears){
            logm[ii] <- alphaminc + logm[ii-1]+ epsilonm[ii]
          } #end ii

          m <- exp(logm)
          mt_pro[k,] <- m

          #Smooth
          if(msplines==T){
            # Smooth the resampled RW with splines
            mt_pro[k,] <- stats::smooth.spline(mt_pro[k,],
                                                nknots=numknots)$y
          }
         }# end k
        #matplot(t(mt_pro),type="l", ylim=c(0,1.8))

        # Now add to the M_age array for all the age slices
        # they are all identical because M is constant across all ages >= 2 (slot 3 for Pac herring)
        for(jj in 3:(maxage+1)){
          M_age[,jj,proyears] <- mt_pro
        } # end for jj

        #matplot(t(M_age[,3,]), type="l", ylim=c(0,1.8))

        # Update proyears cpars in the OM
        Mscenario@cpars$M_ageArray <- M_age

        # Update the OM name
        Mscenario@Name <- paste0(stock,"_",Scenario)

    }else if(Scenario == "RandomWalkMDriftDecr"){
      cat("Running Random Walk M Drift Decreasing Scenario\n")

      # A random walk with drift in the projection years
      # https://www.investopedia.com/articles/trading/07/stationary.asp

      Mscenario <- om
      M_age <- Mscenario@cpars$M_ageArray

      # for some reason, there is a step down from nyrs to nyrs+1 in the
      #  default OM from MSEtool
      #  The iscam mcmc files only have M up to nyrs so not sure why this happens
      #    M_age[1,Mage,nyears:(nyears+1)] # they should be the same
      # First, correct this
      M_age[,Mage,proyears] <- M_age[,Mage,nyears]
      #matplot(t(M_age[,3,]), type="l", ylim=c(0,1.8))

      # Get a matrix to put the projected M values:
      #   mt_pro is a matrix with 1:nsim rows and 1:nproyears cols
      # just do for one age slice as they will all be the same, then copy later
      #   -- pick age Mage (age 3, set in 0_run-analyses.R)
      mt_pro <-  M_age[,Mage,proyears] #matrix(0,nrow=nsim, ncol=nproyears)
      #matplot(t(mt_pro),type="l", ylim=c(0,1.8))

      # Now we want to resample the projected M with smaller sd because
      #  the final historical year had over-inflated sd that we don't
      #  want to project into the future
      # For resampling, use the sd across replicates from sdn years
      #  before last historical year
      sdm <- sd(M_age[,Mage,nyears-sdn])
      cat("Sdm = ",sdm,"\n")
      for(ii in 1:nproyears){
        # Get mean M for that year across replicates
        meanm <- mean(mt_pro[,ii])

        # Now get nsim new values based on the mean but with smaller sd
        set.seed(9)
        mt_pro[,ii] <- rnorm(nsim, mean=meanm, sd=sdm)
      }
      #matplot(t(mt_pro),type="l", ylim=c(0,1.8))

      # For each replicate sim, get the annual M values using a random walk (decreasing drift)
      for(k in 1:nsim){
        logm <- vector(length=nproyears)
        epsilonm <- rnorm(nproyears,0,sigm)
        #logm[1] <- log(Mscenario@cpars$M_ageArray[k,3,nyears])
        logm[1] <- log(mt_pro[k,1])

        for(ii in 2:nproyears){
          logm[ii] <- alphamdec + logm[ii-1]+ epsilonm[ii]
        } #end ii

        m <- exp(logm)
        mt_pro[k,] <- m

        # Smooth the resampled RW with splines
        if(msplines==T){
          mt_pro[k,] <- stats::smooth.spline(mt_pro[k,],
                                             nknots=numknots)$y
        }
      }# end k
      #matplot(t(mt_pro),type="l", ylim=c(0,1.8))

      # Now add to the M_age array for all the age slices
      # they are all identical because M is constant across all ages >= 2 (slot 3 for Pac herring)
      for(jj in 3:(maxage+1)){
        M_age[,jj,proyears] <- mt_pro
      } # end for jj

      #matplot(t(M_age[,3,]), type="l", ylim=c(0,1.8))

      # Update proyears cpars in the OM
      Mscenario@cpars$M_ageArray <- M_age

      # Update the OM name
      Mscenario@Name <- paste0(stock,"_",Scenario)

    }else {
      stop("Scenario must be ConstantM, RandomWalkM RandomWalkMDriftIncr or RandomWalkMDriftDecr! Go back to 1_a_make-pac-herring-oms.R and fix function call to make_MScenarios\n")
    }

    # Plot
    traces <- Mscenario@cpars$M_ageArray[traceSample,Mage,] %>% t() %>%
      as.data.frame() %>%
      mutate(years=all_years) %>%
      rename(trace1=V1, trace2=V2,trace3=V3)

    g <- Mscenario@cpars$M_ageArray[,Mage,] %>%
      apply(2,quantile,probs=c(conflo,0.5,confhi)) %>% t() %>%
      as.data.frame() %>%
      mutate(year=all_years, Scenario=Scenario) %>%
      cbind(traces) %>%
      dplyr::rename(lwr=1, med=`50%`, upr=3) %>%
      ggplot() +
      geom_ribbon(aes(x=year, ymin=lwr, ymax=upr), fill="black", alpha = 0.3) +
      geom_line(aes(x=year,y=med), color="black", lwd=1) +
      geom_line(aes(x=year, y=trace1), color="black",lwd=0.25)+
      geom_line(aes(x=year, y=trace2), color="black",lwd=0.25)+
      geom_line(aes(x=year, y=trace3), color="black",lwd=0.25)+
      geom_vline(xintercept=pyr1, lty=3)+
      scale_fill_startrek()+  # ggsci package
      scale_color_startrek()+
      #ylim(0,3.)+
      gfplot::theme_pbs() +
      labs(x = "Year", y = "M")+
      mytheme+
      theme(legend.position = "bottom")
    #print(g)
    ggsave(file.path(StockDirFigs, paste0("OM-M_",Scenario,"_",stock,".png")),
           width = 8, height = 5)

    # Return the updated M Scenario
    Mscenario

  } # end function

