# Compile the figures for the paper
# January 4, 2026
# Robyn Forrest

# !! This file is intended to be sourced in 0_run_analyses.R after the four
# 3a files are run. These files put the figure lists into the environment, which are
#  used here.

# NOTE. Fig 12 is made directly in 3a_fig12_for_paper.R

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
ggsave(here("Figures","Figure2.png"), width = 8, height = 7)

# MAKE FIGURE 4 (relationship between M and B0, 3 stocks)
fig4hg <- fig4[[1]]
fig4sg <- fig4[[2]]+
  ylab("")
fig4wc <- fig4[[3]]+
  ylab("")

cowplot::plot_grid(fig4hg,fig4sg,fig4wc, nrow=1,
                   labels=c("(a)","(b)","(c)"), align="h", vjust=1.)
ggsave(here("Figures","Figure4.png"), width = 8, height = 5)

# MAKE FIGURE 4 alternative (relationship between M and B0, 3 stocks, 10 replicates)
fig4hg_alt <- fig4_alternative[[1]]+
  theme(legend.position = "none")
fig4sg_alt <- fig4_alternative[[2]]+
  ylab("")+theme(legend.position = "none")
fig4wc_alt <- fig4_alternative[[3]]+
  ylab("")+theme(legend.position = "none")

cowplot::plot_grid(fig4hg_alt,fig4sg_alt,fig4wc_alt, nrow=1,
                   labels=c("(a)","(b)","(c)"), align="h", vjust=1.4)
ggsave(here("Figures","Figure4_alternative.png"), width = 8, height = 5)

# MAKE FIGURE 5 (S-R relationships and replacement lines)
fig5hg <- fig5[[1]]+
  xlab("")
fig5sg <- fig5[[2]]+
  xlab("")
fig5wc <- fig5[[3]]

cowplot::plot_grid(fig5hg,fig5sg,fig5wc, nrow=3, labels=c("(a)","(b)","(c)"),
                   align="v",  hjust = 0.25)
ggsave(here("Figures","Figure5.png"), width = 8, height = 7)

# MAKE FIGURE 6 (time series of M)
# M
fig6hg <- fig6[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig6sg <- fig6[[2]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig6wc <- fig6[[3]]

#SSB
fig7hg <- fig7[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig7sg <- fig7[[2]]+
  xlab("")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank())
fig7wc <- fig7[[3]]+
  theme(legend.position = "none")

#B0
fig8hg <- fig8[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig8sg <- fig8[[2]]+
  xlab("")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank())
fig8wc <- fig8[[3]]+
  theme(legend.position = "none")

## ~Join plots by type~ ##
# add null plots with negative relative heights to reduce white space
cowplot::plot_grid(fig6hg,NULL,fig6sg,NULL,fig6wc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1),  hjust = 0.25)
ggsave(here("Figures","Figure6_M_all_stocks.png"), width = 8, height = 7)

cowplot::plot_grid(fig7hg,NULL,fig7sg,NULL,fig7wc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1),  hjust = 0.25)
ggsave(here("Figures","Figure7_ssb_all_stocks.png"), width = 8, height = 7)

cowplot::plot_grid(fig8hg,NULL,fig8sg,NULL,fig8wc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.11,1,-0.11,1),  hjust = 0.25)
ggsave(here("Figures","Figure8_B0_all_stocks_med_only.png"), width = 8, height = 7)

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
ggsave(here("Figures","Figure6_LRP_all_stocks.png"), width = 8, height = 5)

cowplot::plot_grid(fig6bhg,NULL,fig6bsg,NULL,fig6bwc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1,-0.1,1,-0.1,1),  hjust = 0.25)
ggsave(here("Figures","Figure6_PLRP_all_stocks.png"), width = 8, height = 5)

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
                   align="v", rel_heights=c(1,-0.05,1), hjust = 0.25)
ggsave(here("Figures","Figure8_HG.png"), width = 8, height = 5)

cowplot::plot_grid(fig6asg,NULL,fig6bsg, nrow=3, labels=c("(a)","","(b)"),
                   align="v", rel_heights=c(1,-0.05,1), hjust = 0.25)
ggsave(here("Figures","Figure9_SOG.png"), width = 8, height = 5)

cowplot::plot_grid(fig6awc,NULL,fig6bwc, nrow=3, labels=c("(a)","","(b)"),
                   align="v", rel_heights=c(1,-0.05,1), hjust = 0.25)
ggsave(here("Figures","Figure10_WCVI.png"), width = 8, height = 5)


