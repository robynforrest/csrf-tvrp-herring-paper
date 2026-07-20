# Make the figures for the paper (figs 5 and 7)
# July 20, 2026
# Robyn Forrest

library(here)
source(here("R","0_settings.R"))

# Make 9 panel figure showing sensitivity to settings in the random walk in M
#  for Supplementary Material
# Column 1: Projected M
# Column 2: Projected SB0
# Column 3: Projected P(SB>LRP)

# Sensitivity Base: Same as the base case but only 250 reps Msd=0.03 alpha_m_inc=0.009
# Sensitivity S1: Msd=0.01 alpha_m_inc=0.009
# Sensitivity S2: Msd=0.05 alpha_m_inc=0.009
# Sensitivity S3: Msd=0.03 alpha_m_inc=0.015
# Sensitivity S4: Msd=0.03 alpha_m_inc=0.02

# --- Load Base Herring OMs just to get the number and names of stocks and nsim --------------------
SpDirOM <- here("OMs")
hOMs <- readRDS(here("Sensitivity_results/BaseCase/OMs/hOMs.rda"))
histMSEs <- readRDS(here("Sensitivity_results/BaseCase/MSEs/hist_hMSEs.rda"))
scenameHuman <- readRDS(here(SpDirOM, "ScenarioNamesHuman.rda"))
stocks <- names(hOMs)
nstocks <- length(stocks)
nsim <- hOMs[[1]]@nsim
Mage<-3

for(j in 1:nstocks){
  cat("~~~ Plotting Supp Sensitivity M for", paste(stocks[j]), "~~~\n")

  DirFigs  <- here("Figures")
  stock <- stocks[j]

  # Make lists to put the OMs and MSEs and the rows of the figure
  OMlist <- list()
  MSElist <- list()
  FigList <- list()

  # Get the OM and MSE objects (only scenario 2)
  # Same as the base case but only 250 reps Msd=0.03 alpha_m_inc=0.009
  OMlist[[1]]    <- readRDS(here("Sensitivity_results/BaseCase","OMs", paste(stocks[j]),
                                 "OMScenarios.rda"))[[2]]
  MSElist[[1]]   <- readRDS(here("Sensitivity_results/BaseCase","MSEs", paste(stocks[j]),
                         "hMSEs_NF.rda"))[[2]]

  # S1: Msd=0.01 alpha_m_inc=0.009
  OMlist[[2]]    <- readRDS(here("Sensitivity_results/Msd01_Lambda01","OMs", paste(stocks[j]),
                                 "OMScenarios.rda"))[[2]]
  MSElist[[2]]    <- readRDS(here("Sensitivity_results/Msd01_Lambda01","MSEs", paste(stocks[j]),
                          "hMSEs_NF.rda"))[[2]]

  # S2: Msd=0.05 alpha_m_inc=0.009
  OMlist[[3]]    <- readRDS(here("Sensitivity_results/Msd05_Lambda01","OMs", paste(stocks[j]),
                                 "OMScenarios.rda"))[[2]]
  MSElist[[3]]    <- readRDS(here("Sensitivity_results/Msd05_Lambda01","MSEs", paste(stocks[j]),
                                  "hMSEs_NF.rda"))[[2]]

  # S3: Msd=0.03 alpha_m_inc=0.015
  OMlist[[4]]    <- readRDS(here("Sensitivity_results/Msd03_Lambda015","OMs", paste(stocks[j]),
                                 "OMScenarios.rda"))[[2]]
  MSElist[[4]]    <- readRDS(here("Sensitivity_results/Msd03_Lambda015","MSEs", paste(stocks[j]),
                                  "hMSEs_NF.rda"))[[2]]

  # S4: Msd=0.03 alpha_m_inc=0.02
  OMlist[[5]]    <- readRDS(here("Sensitivity_results/Msd03_Lambda02","OMs", paste(stocks[j]),
                                 "OMScenarios.rda"))[[2]]
  MSElist[[5]]    <-readRDS( here("Sensitivity_results/Msd03_Lambda02","MSEs", paste(stocks[j]),
                                  "hMSEs_NF.rda"))[[2]]

  names(OMlist) <- names(MSElist) <- c("Base", "S1","S2","S3","S4")

  histMSE <- histMSEs[j][[1]]
  pro_years <-  MSElist[[1]]@proyears
  cyr <- MSElist[[1]]@OM$CurrentYr[1] # current year (2023)
  syr <- cyr-MSElist[[1]]@nyears+1
  pyr <- cyr + 1 # get the first of the projection years (currently 2024)
  fyr <- cyr + pro_years #final year of projections

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Column 1: M
    # Just for pro_years
    M_levels <- c("hist", "mean", "recent","annual")
    histM <- purrr::map2_df(MSElist,names(MSElist), getM, age=Mage, type="hist", quant=TRUE, input_type="MSE")
    meanM <- purrr::map2_df(MSElist,names(MSElist), getM, age=Mage, type="mean", quant=TRUE, input_type="MSE")
    recentM <- purrr::map2_df(MSElist,names(MSElist), getM,age=Mage, type="recent", quant=TRUE, input_type="MSE")
    annualM <- purrr::map2_df(MSElist,names(MSElist), getM, age=Mage, type="annual", quant=TRUE, input_type="MSE")

    allM <- rbind(histM,meanM,recentM, annualM) |>
      mutate(`M type`=factor(Mtype, levels=M_levels)) |>
      rename("Sensitivity"="scenario") |>
      mutate(Sensitivity=factor(Sensitivity, levels=names(OMlist))) |>
      select(-Mtype) |>
      filter(year>cyr)

      write_csv(allM, file.path(DirFigs, paste0("MSE-M_SensitivityM_",stock,".csv")))

      # pro years with uncertainty
    g1 <- allM |>
        ggplot()+
        geom_ribbon(aes(x=year, ymin=lwr , ymax=upr, fill=`M type`), alpha = 0.1)+
        geom_line(aes(x=year, y=med, col=`M type`, lty=`M type`),lwd=1.25)+
        scale_color_manual(values=manualcolors[2:5])+
        scale_fill_manual(values=manualcolors[2:5])+
        scale_linetype_manual(values = c("hist" = 2,
                                         "mean" = 2,
                                         "recent" =5,
                                         "annual" = 3))+
        facet_wrap(~Sensitivity, nrow=1)+
        labs(x = "", y = "M")+
        scale_y_continuous(breaks=seq(0,3.,.4))+
        mytheme_sens_plots+
        theme(axis.text.x=element_blank())
    g1

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Plot time series of all alternative B0-based reference points
      refpt_levels <- c("SB","hist", "mean", "recent", "dyn")
      histB0 <- purrr::map2_df(MSElist,names(MSElist), getmeanB0, age=Mage, type="hist", quants=TRUE)
      meanB0 <- purrr::map2_df(MSElist,names(MSElist), getmeanB0, age=Mage, type="mean", quants=TRUE)
      recentB0 <- purrr::map2_df(MSElist,names(MSElist), getmeanB0,age=Mage, type="recent", quants=TRUE)
      dynB0 <- purrr::map2_df(MSElist,names(MSElist), getdynB0,quants=TRUE)
      SSBnf <- purrr::map2_df(MSElist,names(MSElist), getSSB, mp=1) |>
        mutate(RefPtName="SB")

      allB0 <- rbind(histB0,meanB0,recentB0,dynB0,SSBnf) |>
        mutate(`SB0 type`=factor(RefPtName, levels=refpt_levels)) |>
        select(-RefPtName) |>
        rename("Sensitivity"="scenario") |>
        filter(year>cyr)

      write_csv(allB0, file.path(DirFigs, paste0("MSE-B0_SensitivityM_",stock,".csv")))

      # pro years median only
      g2 <- allB0 |>
        ggplot()+
        geom_line(aes(x=year, y=med, col=`SB0 type`, lty=`SB0 type`),lwd=1.25)+
        scale_color_manual(values=manualcolors)+
        facet_wrap(vars(Sensitivity), nrow=1)+
        scale_linetype_manual(values = c("SB"=1,
                                         "hist" = 2,
                                         "mean" = 2,
                                         "recent" =5,
                                         "dyn" = 3))+
        labs(x = "", y = "SB or SB0", title= "")+
        mytheme_paper+
        theme(axis.text.x=element_blank())
     g2

     # Plot time series of P(SB > LRP)
     LRP <- allB0 |>
       filter(!`SB0 type` %in% "SB") |>
       mutate(lwr=0.3*lwr, med=0.3*med, upr=0.3*upr)
     # add back to df with SSB
     LRP <- allB0 |>
       filter(`SB0 type` %in% "SB") |>
       rbind(LRP)
     LRP2023 <- LRP |>
       filter(year==cyr, `SB0 type`=="mean")

      # Plot P(B > 0.3B0)
      PLRPhistB0   <- purrr::map2_df(MSElist,names(MSElist), getPLRP_B0, age=Mage,type="hist", mp=1) |>
        mutate(MP="NFref")
      PLRPmeanB0   <- purrr::map2_df(MSElist,names(MSElist), getPLRP_B0, age=Mage,type="mean", mp=1)|>
        mutate(MP="NFref")
      PLRPrecentB0 <- purrr::map2_df(MSElist,names(MSElist), getPLRP_B0, age=Mage,type="recent", mp=1)|>
        mutate(MP="NFref")
      PLRPdynB0    <- purrr::map2_df(MSElist,names(MSElist), getPLRP_dynB0, mp=1)|>
        mutate(MP="NFref")

      PLRPB0_NF <- rbind(PLRPhistB0, PLRPmeanB0, PLRPrecentB0, PLRPdynB0) |>
        mutate(`SB0 type`=factor(b0type, levels=c("hist", "mean", "recent", "dyn"))) |>
        select(-b0type) |>
        filter(year>cyr)

      write_csv(PLRPB0_NF,file.path(DirFigs, paste0("MSE-PLRP_SensitivityM_",stock,".csv")))

      # Plot all B0 performance metrics on one plot
      # Version that uses SB0 instead of B0
      g3 <- plotPLRP_SB0(PLRPB0_NF,
                    scentext=TRUE,
                    panel=FALSE)
      g3

      cowplot::plot_grid(g1,NULL,g2,NULL,g3, nrow=5,
                         align="v",rel_heights=c(1.,-0.05,1.1,0.08,1.1))

      ggsave(file.path(paste0(DirFigs,"/Supp_Plot_Sensitivity_MSettings_",stock,".png")),
               width = 9, height = 9, bg="white")


} # end for j in stocks

# ====================================================================================================================
# ======= END OF SCRIPT ==============================================================================================
# ====================================================================================================================






