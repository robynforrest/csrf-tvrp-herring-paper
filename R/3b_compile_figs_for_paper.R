# Compile the figures for the paper
# January 4, 2026
# Robyn Forrest

# !! This file is intended to be sourced in 0_run_analyses.R after the four
# 3a files are run. These files put the figure lists into the environment, which are
#  used here.

# NOTE. Fig six is made directly in 3a_fig6_for_paper.R

# MAKE FIGURE 2 (relationship between M and B0, 3 stocks)
fig2hg <- fig2[[1]]
fig2sg <- fig2[[2]]+
  ylab("")
fig2wc <- fig2[[3]]+
  ylab("")

cowplot::plot_grid(fig2hg,fig2sg,fig2wc, nrow=1, labels=c("(a)","(b)","(c)"), align="h")
ggsave(here("Figures","Figure2.png"))

# MAKE FIGURE 2 alternative (relationship between M and B0, 3 stocks, 10 replicates)
fig2hg_alt <- fig2_alternative[[1]]+
  theme(legend.position = "none")
fig2sg_alt <- fig2_alternative[[2]]+
  ylab("")+theme(legend.position = "none")
fig2wc_alt <- fig2_alternative[[3]]+
  ylab("")+theme(legend.position = "none")

cowplot::plot_grid(fig2hg_alt,fig2sg_alt,fig2wc_alt, nrow=1, labels=c("(a)","(b)","(c)"), align="h")
ggsave(here("Figures","Figure2_alternative.png"))

# MAKE FIGURE 3 (S-R relationships and replacement lines)
fig3hg <- fig3[[1]]+
  xlab("")
fig3sg <- fig3[[2]]+
  xlab("")
fig3wc <- fig3[[3]]

cowplot::plot_grid(fig3hg,fig3sg,fig3wc, nrow=3, labels=c("(a)","(b)","(c)"),
                   align="v",  hjust = 0.25)
ggsave(here("Figures","Figure3.png"))

# MAKE FIGURE 4 (time series of SSB, M and B0)
# SSB
fig4ahg <- fig4a[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig4asg <- fig4a[[2]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig4awc <- fig4a[[3]]

#M
fig4bhg <- fig4b[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig4bsg <- fig4b[[2]]+
  xlab("")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank())
fig4bwc <- fig4b[[3]]+
  theme(legend.position = "none")

#B0
fig4chg <- fig4c[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig4csg <- fig4c[[2]]+
  xlab("")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank())
fig4cwc <- fig4c[[3]]+
  theme(legend.position = "none")

## ~Join plots by type~ ##
# add null plots with negative relative heights to reduce white space
cowplot::plot_grid(fig4ahg,NULL,fig4asg,NULL,fig4awc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure4_ssb_all_stocks.png"))

cowplot::plot_grid(fig4bhg,NULL,fig4bsg,NULL,fig4bwc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure4_M_all_stocks.png"))

cowplot::plot_grid(fig4chg,NULL,fig4csg,NULL,fig4cwc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure4_B0_all_stocks_med_only.png"))

## ~Join plots by stock and maybe put two in the supp material~ ##
# SSB
fig4ahg <- fig4a[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig4asg <- fig4a[[2]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig4awc <- fig4a[[3]]+
  theme(axis.text.x=element_blank())
#M
fig4bhg <- fig4b[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig4bsg <- fig4b[[2]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig4bwc <- fig4b[[3]]+
  theme(axis.text.x=element_blank())

#B0
fig4chg <- fig4c[[1]]
fig4csg <- fig4c[[2]]
fig4cwc <- fig4c[[3]]

cowplot::plot_grid(fig4bhg,NULL,fig4ahg,NULL,fig4chg, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure4_HG.png"))

cowplot::plot_grid(fig4bsg,NULL,fig4asg,NULL,fig4csg, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure4_SOG.png"))

cowplot::plot_grid(fig4bwc,NULL,fig4awc,NULL,fig4cwc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1))
ggsave(here("Figures","Figure4_WCVI.png"))

# MAKE FIGURE 5 (LRP and PLRP plots)
# LRP
fig5ahg <- fig5a[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5asg <- fig5a[[2]]+
  xlab("")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank())
fig5awc <- fig5a[[3]]+
  theme(legend.position = "none")

# PLRP
fig5bhg <- fig5b[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5bsg <- fig5b[[2]]+
  xlab("")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank())
fig5bwc <- fig5b[[3]]+
  theme(legend.position = "none")

## ~Join plots by type~ ##
cowplot::plot_grid(fig5ahg,NULL,fig5asg,NULL,fig5awc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.12,1,-0.12,1))
ggsave(here("Figures","Figure5_LRP_all_stocks.png"))

cowplot::plot_grid(fig5bhg,NULL,fig5bsg,NULL,fig5bwc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.1,1,-0.1,1),  hjust = 0.25)
ggsave(here("Figures","Figure5_PLRP_all_stocks.png"))

## ~Join plots by stock and maybe put two in the supp material~ ##
# LRP
fig5ahg <- fig5a[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5asg <- fig5a[[2]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig5awc <- fig5a[[3]]+
  xlab("")

# PLRP
fig5bhg <- fig5b[[1]]+
  theme(legend.position = "none")
fig5bsg <- fig5b[[2]]+
  theme(legend.position = "none")
fig5bwc <- fig5b[[3]]+
  theme(legend.position = "none")

cowplot::plot_grid(fig5ahg,NULL,fig5bhg, nrow=3, labels=c("(a)","","(b)"),
                   align="v", rel_heights=c(1,-0.11,1), hjust = 0.25)
ggsave(here("Figures","Figure5_HG.png"))

cowplot::plot_grid(fig5asg,NULL,fig5bsg, nrow=3, labels=c("(a)","","(b)"),
                   align="v", rel_heights=c(1,-0.11,1), hjust = 0.25)
ggsave(here("Figures","Figure5_SOG.png"))

cowplot::plot_grid(fig5awc,NULL,fig5bwc, nrow=3, labels=c("(a)","","(b)"),
                   align="v", rel_heights=c(1,-0.11,1), hjust = 0.25)
ggsave(here("Figures","Figure5_WCVI.png"))


