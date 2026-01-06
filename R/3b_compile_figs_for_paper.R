# Compile the figures for the paper
# January 4, 2026
# Robyn Forrest

# !! This file is intended to be sourced in 0_run_analyses.R after the four
# 3a files are run. These files put the figure lists into the environment, which are
#  used here.

# NOTE. Fig 7 is made directly in 3a_fig7_for_paper.R

# MAKE FIGURE 2 (Historical M and B0, 3 stocks)
fig2ahg <- fig2a[[1]]+
  theme(axis.text.x=element_blank())+
  theme(axis.title.x = element_blank())
fig2asg <- fig2a[[2]]+
  theme(axis.text.x=element_blank())+
  theme(axis.title.x = element_blank())
fig2awc <- fig2a[[3]]

fig2bhg <- fig2b[[1]]+
  theme(axis.text.x=element_blank())+
  theme(axis.title.x = element_blank())
fig2bsg <- fig2b[[2]]+
  theme(axis.text.x=element_blank())+
  theme(axis.title.x = element_blank())
fig2bwc <- fig2b[[3]]

cowplot::plot_grid(fig2ahg,fig2bhg,NULL,NULL,fig2asg,fig2bsg,NULL,NULL,fig2awc,fig2bwc,nrow=5,
                   labels=c("(a)","","","","(b)","","","","(c)"),
                   rel_heights=c(1,-0.11,1,-0.11,1),
                   align="h",  hjust = 0.25)
ggsave(here("Figures","Figure2.png"))

# MAKE FIGURE 3 (relationship between M and B0, 3 stocks)
fig3hg <- fig3[[1]]
fig3sg <- fig3[[2]]+
  ylab("")
fig3wc <- fig3[[3]]+
  ylab("")

cowplot::plot_grid(fig3hg,fig3sg,fig3wc, nrow=1, labels=c("(a)","(b)","(c)"), align="h")
ggsave(here("Figures","Figure3.png"))

# MAKE FIGURE 3 alternative (relationship between M and B0, 3 stocks, 10 replicates)
fig3hg_alt <- fig3_alternative[[1]]+
  theme(legend.position = "none")
fig3sg_alt <- fig3_alternative[[2]]+
  ylab("")+theme(legend.position = "none")
fig3wc_alt <- fig3_alternative[[3]]+
  ylab("")+theme(legend.position = "none")

cowplot::plot_grid(fig3hg_alt,fig3sg_alt,fig3wc_alt, nrow=1, labels=c("(a)","(b)","(c)"), align="h")
ggsave(here("Figures","Figure3_alternative.png"))

# MAKE FIGURE 4 (S-R relationships and replacement lines)
fig4hg <- fig4[[1]]+
  xlab("")
fig4sg <- fig4[[2]]+
  xlab("")
fig4wc <- fig4[[3]]

cowplot::plot_grid(fig4hg,fig4sg,fig4wc, nrow=3, labels=c("(a)","(b)","(c)"),
                   align="v",  hjust = 0.25)
ggsave(here("Figures","Figure4.png"))

# MAKE FIGURE 5 (time series of SSB, M and B0)
# SSB
fig5ahg <- fig5a[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5asg <- fig5a[[2]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5awc <- fig5a[[3]]

#M
fig5bhg <- fig5b[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5bsg <- fig5b[[2]]+
  xlab("")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank())
fig5bwc <- fig5b[[3]]+
  theme(legend.position = "none")

#B0
fig5chg <- fig5c[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5csg <- fig5c[[2]]+
  xlab("")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank())
fig5cwc <- fig5c[[3]]+
  theme(legend.position = "none")

## ~Join plots by type~ ##
# add null plots with negative relative heights to reduce white space
cowplot::plot_grid(fig5ahg,NULL,fig5asg,NULL,fig5awc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure5_ssb_all_stocks.png"))

cowplot::plot_grid(fig5bhg,NULL,fig5bsg,NULL,fig5bwc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure5_M_all_stocks.png"))

cowplot::plot_grid(fig5chg,NULL,fig5csg,NULL,fig5cwc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure5_B0_all_stocks_med_only.png"))

## ~Join plots by stock and maybe put two in the supp material~ ##
# SSB
fig5ahg <- fig5a[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5asg <- fig5a[[2]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5awc <- fig5a[[3]]+
  theme(axis.text.x=element_blank())
#M
fig5bhg <- fig5b[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5bsg <- fig5b[[2]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5bwc <- fig5b[[3]]+
  theme(axis.text.x=element_blank())

#B0
fig5chg <- fig5c[[1]]
fig5csg <- fig5c[[2]]
fig5cwc <- fig5c[[3]]

cowplot::plot_grid(fig5bhg,NULL,fig5ahg,NULL,fig5chg, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure5_HG.png"))

cowplot::plot_grid(fig5bsg,NULL,fig5asg,NULL,fig5csg, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure5_SOG.png"))

cowplot::plot_grid(fig5bwc,NULL,fig5awc,NULL,fig5cwc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure5_WCVI.png"))

# MAKE FIGURE 6 (LRP and PLRP plots)
# LRP
fig6ahg <- fig6a[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig6asg <- fig6a[[2]]+
  xlab("")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank())
fig6awc <- fig6a[[3]]+
  theme(legend.position = "none")

# PLRP
fig6bhg <- fig6b[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig6bsg <- fig6b[[2]]+
  xlab("")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank())
fig6bwc <- fig6b[[3]]+
  theme(legend.position = "none")

## ~Join plots by type~ ##
cowplot::plot_grid(fig6ahg,NULL,fig6asg,NULL,fig6awc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.12,1,-0.12,1))
ggsave(here("Figures","Figure6_LRP_all_stocks.png"))

cowplot::plot_grid(fig6bhg,NULL,fig6bsg,NULL,fig6bwc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.1,1,-0.1,1),  hjust = 0.25)
ggsave(here("Figures","Figure6_PLRP_all_stocks.png"))

## ~Join plots by stock and maybe put two in the supp material~ ##
# LRP
fig6ahg <- fig6a[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig6asg <- fig6a[[2]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig6awc <- fig6a[[3]]+
  xlab("")

# PLRP
fig6bhg <- fig6b[[1]]+
  theme(legend.position = "none")
fig6bsg <- fig6b[[2]]+
  theme(legend.position = "none")
fig6bwc <- fig6b[[3]]+
  theme(legend.position = "none")

cowplot::plot_grid(fig6ahg,NULL,fig6bhg, nrow=3, labels=c("(a)","","(b)"),
                   align="v", rel_heights=c(1,-0.11,1), hjust = 0.25)
ggsave(here("Figures","Figure6_HG.png"))

cowplot::plot_grid(fig6asg,NULL,fig6bsg, nrow=3, labels=c("(a)","","(b)"),
                   align="v", rel_heights=c(1,-0.11,1), hjust = 0.25)
ggsave(here("Figures","Figure6_SOG.png"))

cowplot::plot_grid(fig6awc,NULL,fig6bwc, nrow=3, labels=c("(a)","","(b)"),
                   align="v", rel_heights=c(1,-0.11,1), hjust = 0.25)
ggsave(here("Figures","Figure6_WCVI.png"))


