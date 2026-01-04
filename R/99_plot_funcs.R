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

# plot time-varying harvest control rules from om
plotTVHCRom <- function(allB0,
                      pyr1,
                      lrp=0.3,
                      usr=0.6,
                      maxf=0.2,
                      phaseplot=FALSE,
                      om=TRUE,
                      dir){

  # Note that SSB in this function for the OM is assess_yr + 1,
  # i.e., projected year (terminal year +1)
  df <- allB0 |>
    filter( `B0 type`!= "SSB") |>
    select(-upr,-lwr) |>
    rename(B0=med)

  SSB <- allB0 |>
    filter( `B0 type`== "SSB") |>
    select(-upr,-lwr)

  # Now need to put the same time series SSB with each B0 type/scenario combo
  df <- df |>
    mutate(SSB=rep(SSB$med,4))

  # MAKE PLOTS
  # maximum for x axis for plots - make all on the same scale
  maxX <- 0.8*max(df$SSB)
  maxY <- 1.1*maxf

  if(j==2)maxX <- 1.75*max(df$SSB)

  # Set up the mps we would be using if we were running an assessment with the alternative LRPs
  b0types <- c("hist","mean","recent","dyn")
  nb0 <- length(b0types)

  for(ii in 1:nb0){
    g <- df |>
     filter(`B0 type`==b0types[ii],
            year>=pyr1) |>
      mutate(LRP=lrp*B0,
             USR=usr*B0,
             LRR=maxf) |>
      mutate(x1=0,xend1=LRP,y1=0,yend1=0,
             x2=LRP,xend2=USR,y2=0,yend2=LRR,
             x3=USR,xend3=maxX,y3=LRR,yend3=LRR) |>
      ggplot(aes(x=SSB_m, y=FMort_m,label=year))+
      geom_segment(aes(x=x1,y=y1,xend=xend1,yend=yend1, colour=year),linewidth=1.,
                    inherit.aes = FALSE) +
      geom_segment(aes(x=x2,y=y2,xend=xend2,yend=yend2, colour=year),linewidth=1.,
                    inherit.aes = FALSE) +
      geom_segment(aes(x=x3,y=y3,xend=xend3,yend=yend3, colour=year),linewidth=1.,
                   inherit.aes = FALSE) +
      facet_wrap(~scenario,nrow=1)+
      ylim(0,maxY) +
      xlim(0,maxX)+
      scale_colour_viridis_c() +
      gfplot::theme_pbs()+
      labs(x = "Spawning biomass", y = "Fishing mortality", title=paste("B0 type =", b0types[ii]))+
      mytheme_lg
    g
    ggsave(file.path(dir, paste0("FIGURE6_MSE-HCR_OM_",b0types[ii],".png")),
           width = 16, height = 10)


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

