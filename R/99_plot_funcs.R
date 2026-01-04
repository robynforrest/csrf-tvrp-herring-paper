# ==============================================================================================================================
# # === Sourced by various files  ================================================================================================
# ==============================================================================================================================
# Robyn Forrest
# January 15 2021
# Modified Oct 2021
# Modified Aug 2023

################################################################################################
# PLOTTING FUNCTIONS.

# TODO: Document these functions

# plotting function for plotting OM SSB vs contemporary estimated SSB
# Currently ribbon plots and fixing of y limits is turned off
# SSB
plotmpSSB <- function(mpSSBsummary,Dir=Dir, firstyr){
  Ylim <- 1.1*max(c(mpSSBsummary$upr,mpSSBsummary$omupr))

  g <- mpSSBsummary |>
    ggplot() +
    geom_ribbon(aes(x=Year, ymin=lwr, ymax=upr), color="red",fill="red", alpha = 0.15) +
    geom_line(aes(x=Year,y=med,color="red"),  lwd=1) +
    geom_ribbon(aes(x=Year, ymin=omlwr, ymax=omupr), color="blue", fill="blue", alpha = 0.05)+
    geom_line(aes(x=Year,y=ommed,color="blue"),  lwd=1) +
    gfplot::theme_pbs() +
    #ylim(0,Ylim)+
    geom_vline(aes(xintercept = firstyr), lty=2)+
    scale_color_identity(name = "",
                         breaks = c("red", "blue"),
                         labels = c("Assessment", "OM"),
                         guide = "legend")+
    facet_grid(rows=vars(MP), cols=vars(Scenario), scales = "free")+
    labs(x = "Year", y = "SSB", title=paste0("OM SSB vs terminal contemporary assessed SSB"))+
    mytheme
  print(g)
  ggsave(file.path(Dir, "MSE-SSB_OM_vs_MP.png"),
         width = 16, height = 10)
} # end function

# Fishing mortality
plotmpF <- function(mpFsummary,Dir=Dir, firstyr){
  Ylim <- 1.1*max(c(mpFsummary$upr,mpFsummary$omupr))

  g <- mpFsummary |>
    ggplot() +
    geom_ribbon(aes(x=Year, ymin=lwr, ymax=upr), color="red",fill="red", alpha = 0.15) +
    geom_line(aes(x=Year,y=med,color="red"),  lwd=1) +
    geom_ribbon(aes(x=Year, ymin=omlwr, ymax=omupr), color="blue", fill="blue", alpha = 0.05)+
    geom_line(aes(x=Year,y=ommed,color="blue"),  lwd=1) +
    gfplot::theme_pbs() +
    #ylim(0,Ylim)+
    geom_vline(aes(xintercept = firstyr), lty=2)+
    scale_color_identity(name = "",
                         breaks = c("red", "blue"),
                         labels = c("Assessment", "OM"),
                         guide = "legend")+
    facet_grid(rows=vars(MP), cols=vars(Scenario), scales = "free")+
    labs(x = "Year", y = "Fishing mortality", title=paste0("OM F vs terminal contemporary assessed F"))+
    mytheme
  print(g)
  ggsave(file.path(Dir, "MSE-F_OM_vs_MP.png"),
         width = 16, height = 10)
} # end function

#################################################################################################
# plotting function plotting function for plotting OM M vs contemporary estimated M
plotmpM <- function(mpMsummary,Dir=Dir, firstyr){
  Ylim = 1.1*max(c(mpMsummary$upr,mpMsummary$omupr))

  g <- mpMsummary |>
    ggplot() +
    geom_ribbon(aes(x=Year, ymin=lwr, ymax=upr), color="red",fill="red", alpha = 0.15) +
    geom_line(aes(x=Year,y=med,color="red"),  lwd=1) +
    geom_ribbon(aes(x=Year, ymin=omlwr, ymax=omupr), color="blue", fill="blue", alpha = 0.05)+
    geom_line(aes(x=Year,y=ommed,color="blue"),  lwd=1) +
    gfplot::theme_pbs() +
    ylim(0.1,Ylim)+
    geom_vline(aes(xintercept = firstyr), lty=2)+
    scale_color_identity(name = "",
                         breaks = c("red", "blue"),
                         labels = c("Assessment", "OM"),
                         guide = "legend")+
    facet_grid(rows=vars(MP), cols=vars(Scenario))+
    labs(x = "Year", y = "M", title=paste0("OM M vs contemporary assessed M"))+
    mytheme
  print(g)
  ggsave(file.path(Dir, paste0("MSE-M_OM_vs_MP.png")),
         width = 16, height = 10)
}

################################################################################################
# plotting function plotting function for P>0.3B0, 5 ways
plotmpPLRP <- function(mpPLRP,
                      Dir,
                      B0type="hist",
                      Colour=histcol){

  g <- mpPLRP |>
    ggplot() +
    geom_line(aes(x=year,y=PLRP),color=Colour,  lwd=1) +
    gfplot::theme_pbs() +
    ylim(0.,1.)+
    geom_vline(aes(xintercept = pyr1), lty=2)+
    facet_wrap(vars(scenario))+
    labs(x = "Year", y = "P(SSB>LRP)", title=paste0("P(B > B0",B0type,")"))+
    theme(plot.title = element_text(face="bold", size=14),
          axis.title.x = element_text(size=12,face="bold"),
          axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=8),
          strip.text.x = element_text(size=10,face="bold"),
          strip.text.y = element_text(size=6),
          legend.text =  element_text(size=12),
          legend.position="bottom")
  print(g)
  ggsave(file.path(Dir, paste0("MSE-P_LRP_B0",B0type,".png")),
         width = 8, height = 5)
}

##################################################################################################

# OM parameters
# Static
# dfPars is the output of function getPars_om_static()
plot_pars_om_static <- function(dfPars,Dir, Stock="", param="h") {

  Ylab <- param
  Ymax <- as.numeric(quantile(as.numeric(dfPars[param][[1]]),probs=0.85, na.rm=T))

  g <- dfPars |>
    ggplot(aes(x=Scenario, y=!! rlang::sym(param)))+
    geom_boxplot(fill="blue", alpha=0.5)+
    coord_cartesian(ylim = c(0, Ymax))+
    scale_y_continuous(limits=c(0, Ymax))+
    labs(x = "Scenario", y = Ylab, title=paste0(Stock))+
    gfplot::theme_pbs()+
    theme(plot.title = element_text(face="bold", size=14),
          axis.title.x = element_text(size=12,face="bold"),
          axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=8),
          strip.text.x = element_text(size=10,face="bold"),
          strip.text.y = element_text(size=10,face="bold"),
          legend.text =  element_text(size=12),
          legend.position="bottom")
  g
  ggsave(file.path(Dir, paste0("OM-Box_staticPars_",param,".png")),
         width = 8, height = 5)
}

#LOOK AT THIS NEXT
# plot time-varying harvest control rules, either from om or assessment
# Uses the output object from getPars_rel_error
# which already matched the SSB0 estimates to the right MP
plotTVHCR <- function(df_relative_errors,
                      yr1,
                      pyr1,
                      lrp=0.3,
                      usr=0.6,
                      maxf=0.2,
                      phaseplot=FALSE,
                      om=TRUE,
                      dir,
                      species="pac-herring"){

  # Get the relative errors dataframe, which summarises all the parameters
  # matched up from the OM and assessments
  # It has already matched up SSB with the appropriate MP
  # Note that SSB in this function for both OM and AM is assess_yr + 1,
  # i.e., projected year (terminal year +1)
  df <- df_relative_errors
  mps <- unique(df$MP)
  nmps <- length(mps)
  species <- species

  # plot the "true" OM HCR or the one from contemporary assessed values
  # SSB0 has already been matched with the MP

  #OM
  medParsOM <- df |>
  select(-relError, -asValue) |>
  filter(Variable %in% c("SSB0",
                         "FMort",
                         "SSB")) |>
  spread(Variable, omValue) |>
  group_by(Scenario, MP,Year_assess) |>
  summarise(SSB0_m=quantile(as.numeric(SSB0), probs=0.50, na.rm=T),
            FMort_m=quantile(as.numeric(FMort), probs=0.50, na.rm=T),
            SSB_m=quantile(as.numeric(SSB), probs=0.50, na.rm=T))

  #Assessment
  medParsAS <- df |>
  select(-relError, -omValue) |>
  filter(Variable %in% c("SSB0",
                         "FMort",
                         "SSB")) |>
  spread(Variable, asValue) |>
  group_by(Scenario, MP,Year_assess) |>
  summarise(SSB0_m=quantile(as.numeric(SSB0), probs=0.50, na.rm=T),
            FMort_m=quantile(as.numeric(FMort), probs=0.50, na.rm=T),
            SSB_m=quantile(as.numeric(SSB), probs=0.50, na.rm=T))


  write_csv(medParsOM,file.path(dir, paste0("MSE-HCR_Pars_OM.csv")))
  write_csv(medParsAS,file.path(dir, paste0("MSE-HCR_Pars_Assess.csv")))

  # MAKE PLOTS
  maxyrFAS <- medParsAS #|> filter(Year_assess > pyr1)
  maxyrFOM <- medParsOM #|> filter(Year_assess > pyr1)

  # maximum for x axis for plots - make all on the same scale
  #maxX <- 1.1*max(c(usr*medParsOM$SSB0_m),(usr*medParsAS$SSB0_m))
  maxX <- 2.*max(usr*medParsOM$SSB0_m)
  if(phaseplot==FALSE) maxY <- 2*maxf
  if(phaseplot==TRUE)  maxY <- max(c(maxyrFOM$FMort_m),(maxyrFAS$FMort_m)) #lrr

  if(om==TRUE){
    Type="OM"
    medPars <- medParsOM
  }else{
    Type="Assess"
    medPars <- medParsAS
  }

  for(ii in 1:nmps){
    g <- medPars |>
      dplyr::filter(MP==mps[ii],
                    Year_assess > pyr1) |>
      mutate(LRP=lrp*SSB0_m,
             USR=usr*SSB0_m,
             LRR=maxf) |>
      mutate(x1=0,xend1=LRP,y1=0,yend1=0,
             x2=LRP,xend2=USR,y2=0,yend2=LRR,
             x3=USR,xend3=maxX,y3=LRR,yend3=LRR) |>
      ggplot(aes(x=SSB_m, y=FMort_m,label=Year_assess))+
      geom_segment(aes(x=x1,y=y1,xend=xend1,yend=yend1, colour=Year_assess),linewidth=1.,
                    inherit.aes = FALSE) +
      geom_segment(aes(x=x2,y=y2,xend=xend2,yend=yend2, colour=Year_assess),linewidth=1.,
                    inherit.aes = FALSE) +
      geom_segment(aes(x=x3,y=y3,xend=xend3,yend=yend3, colour=Year_assess),linewidth=1.,
                   inherit.aes = FALSE) +
      facet_wrap(~Scenario,nrow=2)+
      ylim(0,maxY) +
      xlim(0,maxX)+
      facet_wrap(~Scenario,nrow=2)+
      scale_colour_viridis_c() +
      gfplot::theme_pbs()+
      labs(x = "Spawning biomass", y = "Fishing mortality", title=paste("MP =", mps[ii]))+
      mytheme_lg

     if(phaseplot==TRUE) {
     g <- g +
        geom_point(size=0.75)+
        geom_path(linewidth=0.5)+
        ggrepel::geom_text_repel(aes(colour=Year_assess), size=4)
      }

    ggsave(file.path(dir, paste0("MSE-HCR_",Type , "_",mps[ii],"_phase_",phaseplot,".png")),
           width = 16, height = 10)

    if(phaseplot==TRUE) {
      # Also plot the phase plot by itself
      phasedat <- medPars |>
        dplyr::filter(Year_assess > pyr1)|>
        mutate(LRP=lrp*SSB0_m,
               USR=usr*SSB0_m,
               LRR=maxf)
      #maxY <- 1.1*max(phasedat$FMort_m)


      g <- phasedat |>
        dplyr::filter(MP==mps[ii])|>
        ggplot(aes(x=SSB_m, y=FMort_m,label=Year_assess))+
        geom_point(size=0.75)+
        geom_path(linewidth=0.5)+
        ggrepel::geom_text_repel(aes(colour=Year_assess), size=4)+
        facet_wrap(~Scenario,nrow=2)+
        ylim(0,maxY)+
        xlim(0,maxX)+
        scale_colour_viridis_c() +
        gfplot::theme_pbs()+
        labs(x = "Spawning biomass", y = "Fishing mortality", title=paste("MP =", mps[ii]))+
        mytheme
      ggsave(file.path(dir, paste0("MSE-Phase_Plot_",Type , "_",mps[ii],".png")),
             width = 16, height = 10)
    } # end if
  } # end ii

}# end function

# plotPLRP
# Plot probability of being above the LRP with different B0 metrics
# Can either all be on the same plot (panel=FALSE), or individual plots (panel=TRUE)
# the PLRP object is made in 2b_plot-pac-herring-mse-nf.R
plotPLRP <- function(PLRPobject,
                     scentext=FALSE,
                     panel=TRUE){

  if(panel==FALSE){
    PLRPobject <- PLRPobject #|>
    #select(year, scenario, b0type) #!!b0type

    g <- PLRPobject |>
      ggplot() +
      geom_line(aes(x=year,y=PLRPobject[,1], color=`B0 type`), lwd=1.5) +
      geom_hline(yintercept=0., lty=1, linewidth=0.5)+
      geom_hline(yintercept=0.25, lty=2, linewidth=0.25)+
      geom_hline(yintercept=0.5, lty=2, linewidth=0.5)+
      geom_hline(yintercept=0.75, lty=2, linewidth=0.25)+
      geom_hline(yintercept=1., lty=2, linewidth=0.5)+
      facet_wrap(vars(scenario), nrow=1)+
      theme(legend.position = "none") +
      labs(x = "Year", y = "")+
      ylim(0,1.05) +
      mytheme+
      theme(axis.text.x = element_text(size=22))+
      theme(axis.text.y = element_text(size=22))+
      theme(axis.title.x = element_text(size=24,face="bold"))+
      theme(axis.title.y = element_text(size=24,face="bold"))+
      theme(strip.text.x = element_text(size=20,face="bold"))+
      theme(legend.text = element_text(size=20))+
      theme(legend.title = element_text(size=20))+
      scale_colour_manual(values=c("hist"=histcol,"mean"=meancol,"recent"=meanrecentcol,"dyn"=dyncol))+
      guides(colour=guide_legend(title="B0 type"))+
      ylab("P(SSB > LRP)")

    if(scentext==FALSE){
      g <- g +theme(strip.text.y = element_blank())
    }
  } #end if

  if(panel==TRUE){
    g <- PLRPobject |>
      ggplot() +
      geom_line(aes(x=year,y=PLRPobject[,1], color=`B0 type`), lwd=1.5) +
      geom_hline(yintercept=0., lty=1, linewidth=0.5)+
      geom_hline(yintercept=0.25, lty=2, linewidth=0.25)+
      geom_hline(yintercept=0.5, lty=2, linewidth=0.5)+
      geom_hline(yintercept=0.75, lty=2, linewidth=0.25)+
      geom_hline(yintercept=1., lty=2, linewidth=0.5)+
      facet_grid(scenario~b0type)+
      theme(legend.position = "none") +
      labs(x = "Year", y = "")+
      ylim(0,1.05) +
      mytheme+
      theme(axis.text.x = element_text(size=22))+
      theme(axis.text.y = element_text(size=22))+
      theme(axis.title.x = element_text(size=24,face="bold"))+
      theme(axis.title.y = element_text(size=24,face="bold"))+
      theme(strip.text.x = element_text(size=20,face="bold"))+
      theme(legend.text = element_text(size=20))+
      theme(legend.title = element_text(size=20))+
      scale_colour_manual(values=c("hist"=histcol,"mean"=meancol,"recent"=meanrecentcol,"dyn"=dyncol))+
      guides(colour=guide_legend(title="B0 type"))
  } #end if
  g
} #end function

# === PLOTTING FUNCTIONS FOR A COUPLE OF COMPLEX PLOTS ============================================================
# 1. plotasymRefPts
# Plot how contemporary values of B0 and Bmsy change as a
#  function of M
plotasymRefPts_allsims <- function(om, mse, scen, scenHuman,age) {
  all_years <- seq(mse@OM$CurrentYr[1] - mse@nyears + 1, mse@OM$CurrentYr[1]+mse@proyears)

  Sim <- 1:om@nsim
  M <- om@cpars$M_ageArray[,age,] |> t() |>
    as.data.frame() |>
    rename(Sim = 1:om@nsim) |>
    mutate(year=all_years) |>
    melt(id.vars="year") |>
    rename(Sim=variable, M=value)

  asymBMSY <- mse@Hist@Ref$ByYear$SSBMSY |> t() |>
    as.data.frame() |>
    rename(Sim = 1:om@nsim) |>
    mutate(year=all_years) |>
    melt(id.vars="year") |>
    rename(Sim=variable, asymBMSY=value)

  asymFMSY <- mse@Hist@Ref$ByYear$FMSY |> t() |>
    as.data.frame() |>
    rename(Sim = 1:om@nsim) |>
    mutate(year=all_years) |>
    melt(id.vars="year") |>
    rename(Sim=variable, asymFMSY=value)

  asymB0 <- mse@Hist@Ref$ByYear$B0 |> t() |>
    as.data.frame() |>
    rename(Sim = 1:om@nsim) |>
    mutate(year=all_years) |>
    melt(id.vars="year") |>
    rename(Sim=variable, asymB0=value)

  asymRefPts <- left_join(M, asymBMSY)
  asymRefPts <- left_join(asymRefPts, asymFMSY)
  asymRefPts <- left_join(asymRefPts, asymB0)

  # M vs asymB0
  g <- ggplot(asymRefPts) +
    geom_jitter(aes(x=year, y=M, colour=Sim), size=3)+
    gfplot::theme_pbs() +
    labs(x = "Year", y = "M", title="")+
    ylim(c(0,2))+
    theme(plot.title = element_text(face="bold", size=28),
          axis.title.y = element_text(size=20,face="bold"),
          axis.title.x = element_text(size=20,face="bold"),
          axis.text.y = element_text(size=20),
          axis.text.x = element_text(size=20),
          legend.position = "none")
  g1 <- ggplot(asymRefPts) +
    geom_jitter(aes(x=M, y=asymB0, colour=Sim), size=3)+
    gfplot::theme_pbs() +
    labs(x = "M", y = "B0", title="")+
    theme(plot.title = element_text(face="bold", size=28),
          axis.title.y = element_text(size=20,face="bold"),
          axis.title.x = element_text(size=20,face="bold"),
          axis.text.y = element_text(size=20),
          axis.text.x = element_text(size=20),
          legend.position = "none")
  g2 <- ggplot(asymRefPts) +
    geom_jitter(aes(x=M, y=asymBMSY, colour=Sim), size=3)+
    gfplot::theme_pbs() +
    labs(x = "M", y = "BMSY", title="")+
    theme(plot.title = element_text(face="bold", size=28),
          axis.title.y = element_text(size=20,face="bold"),
          axis.title.x = element_text(size=20,face="bold"),
          axis.text.y = element_text(size=20),
          axis.text.x = element_text(size=20),
          legend.position = "none")
  g3 <- ggplot(asymRefPts) +
    geom_jitter(aes(x=M, y=asymFMSY, colour=Sim), size=3)+
    gfplot::theme_pbs() +
    labs(x = "M", y = "FMSY", title="")+
    theme(plot.title = element_text(face="bold", size=28),
          axis.title.y = element_text(size=20,face="bold"),
          axis.title.x = element_text(size=20,face="bold"),
          axis.text.y = element_text(size=20),
          axis.text.x = element_text(size=20),
          legend.position = "none")
  cowplot::plot_grid(g,g1,g2, ncol=3)
}

plotasymRefPts_onesim <- function(om, mse, scen, scenHuman,age,sim=1) {
  all_years <- seq(mse@OM$CurrentYr[1] - mse@nyears + 1, mse@OM$CurrentYr[1]+mse@proyears)

  M <- om@cpars$M_ageArray[,age,] |> t() |>
    as.data.frame() |>
    rename(Sim = 1:om@nsim) |>
    mutate(year=all_years) |>
    melt(id.vars="year") |>
    rename(Sim=variable, M=value)

  asymBMSY <- mse@Hist@Ref$ByYear$SSBMSY |> t() |>
    as.data.frame() |>
    rename(Sim = 1:om@nsim) |>
    mutate(year=all_years) |>
    melt(id.vars="year") |>
    rename(Sim=variable, asymBMSY=value)

  asymFMSY <- mse@Hist@Ref$ByYear$FMSY |> t() |>
    as.data.frame() |>
    rename(Sim = 1:om@nsim) |>
    mutate(year=all_years) |>
    melt(id.vars="year") |>
    rename(Sim=variable, asymFMSY=value)

  asymB0 <- mse@Hist@Ref$ByYear$B0 |> t() |>
    as.data.frame() |>
    rename(Sim = 1:om@nsim) |>
    mutate(year=all_years) |>
    melt(id.vars="year") |>
    rename(Sim=variable, asymB0=value)

  asymRefPts <- left_join(M, asymBMSY)
  asymRefPts <- left_join(asymRefPts, asymFMSY)
  asymRefPts <- left_join(asymRefPts, asymB0) |>
    filter(Sim==paste0("Sim",sim))


  # M vs asymB0 - one sim
  g <- ggplot(asymRefPts) +
    geom_point(aes(x=year, y=M), colour=1, size=3)+
    gfplot::theme_pbs() +
    labs(x = "Year", y = "M", title="")+
    ylim(c(0,2))+
    theme(plot.title = element_text(face="bold", size=28),
          axis.title.y = element_text(size=20,face="bold"),
          axis.title.x = element_text(size=20,face="bold"),
          axis.text.y = element_text(size=20),
          axis.text.x = element_text(size=20),
          legend.position = "none")
  g1 <- ggplot(asymRefPts) +
    geom_point(aes(x=M, y=asymB0), colour=1, size=3)+
    gfplot::theme_pbs() +
    labs(x = "M", y = "B0", title="")+
    theme(plot.title = element_text(face="bold", size=28),
          axis.title.y = element_text(size=20,face="bold"),
          axis.title.x = element_text(size=20,face="bold"),
          axis.text.y = element_text(size=20),
          axis.text.x = element_text(size=20),
          legend.position = "none")
  g2 <- ggplot(asymRefPts) +
    geom_point(aes(x=M, y=asymBMSY), colour=1, size=3)+
    gfplot::theme_pbs() +
    labs(x = "M", y = "BMSY", title="")+
    theme(plot.title = element_text(face="bold", size=28),
          axis.title.y = element_text(size=20,face="bold"),
          axis.title.x = element_text(size=20,face="bold"),
          axis.text.y = element_text(size=20),
          axis.text.x = element_text(size=20),
          legend.position = "none")
  g3 <- ggplot(asymRefPts) +
    geom_point(aes(x=M, y=asymFMSY, colour=1), size=3)+
    gfplot::theme_pbs() +
    labs(x = "M", y = "FMSY", title="")+
    theme(plot.title = element_text(face="bold", size=28),
          axis.title.y = element_text(size=20,face="bold"),
          axis.title.x = element_text(size=20,face="bold"),
          axis.text.y = element_text(size=20),
          axis.text.x = element_text(size=20),
          legend.position = "none")
  #cowplot::plot_grid(g,g1,g2, ncol=2, greedy=T)
  cowplot::plot_grid(g,g1,g2, ncol=3)
}

