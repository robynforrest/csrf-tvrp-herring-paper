# Compile the figures for the paper
# January 4, 2026
# Robyn Forrest

# !! This file is intended to be sourced in 0_run_analyses.R after the four
# 3a files are run. These files put the figure lists into the environment, which are
#  used here.

# NOTE. Fig 10 is made directly in 3a_fig10_for_paper.R

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

cowplot::plot_grid(fig2ahg,fig2bhg,fig2asg,fig2bsg,fig2awc,fig2bwc,nrow=3,
                   labels=c("(a)","(b)","(c)"),
                   #rel_heights=c(1,-0.11,1,-0.11,1),
                   align="v",  hjust = 0.25)
ggsave(here("Figures","Figure2.png"), width = 8, height = 7)

# MAKE FIGURE 4 (relationship between M and B0, 3 stocks)
fig4hg <- fig4[[1]] +
  xlab("")+
  theme(axis.text.x=element_blank())
fig4sg <- fig4[[2]]+
 xlab("")+
  theme(axis.text.x=element_blank())
fig4wc <- fig4[[3]]

cowplot::plot_grid(NULL,fig4hg,NULL,fig4sg,NULL,fig4wc, nrow=3,
                   labels=c("(a)","","(b)","","(c)"), align="v", vjust=1.,
                   rel_widths=c(0.2,3))
ggsave(here("Figures","Figure4.png"), width = 8, height = 7, bg="white")

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
  xlab("")+
  theme(legend.position = "none")
fig5wc <- fig5[[3]]+
  theme(legend.position = "none")

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
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")
fig6wc <- fig6[[3]]+
  theme(legend.position = "none")

#FIGURE 7 SSB
fig7hg <- fig7[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())
fig7sg <- fig7[[2]]+
  xlab("")+
  theme(legend.position = "none")+
  theme(axis.text.x=element_blank())
fig7wc <- fig7[[3]]+
  theme(legend.position = "none")

#FIGURE 8 B0
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
                   align="v", rel_heights=c(1.2,-0.1,1.2,-0.1,1.25),  hjust = 0.25)
ggsave(here("Figures","Figure6_M_all_stocks.png"), width = 8, height = 7)


cowplot::plot_grid(fig7hg,NULL,fig7sg,NULL,fig7wc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1.2,-0.1,1.2,-0.1,1.25),  hjust = 0.25)
ggsave(here("Figures","Figure7_ssb_all_stocks.png"), width = 8, height = 7)

cowplot::plot_grid(fig8hg,NULL,fig8sg,NULL,fig8wc, nrow=5, labels=c("(a)","","(b)","","(c)"),
                   align="v", rel_heights=c(1.2,-0.1,1.2,-0.1,1.25),  hjust = 0.25)
ggsave(here("Figures","Figure8_B0_all_stocks_med_only.png"), width = 8, height = 7)

# MAKE FIGURE 9 (LRP and PLRP plots for Increasing M Scenario)
# MAKE FIGURE 9SUPP (LRP and PLRP plots for Constant M Scenario) in Supp Material
# Make combined plots for each scenario

# Had to cut off the y lab and legend so the plots would be the same size
# Get them back and add them at the end
legend_top <- get_legend(
  # create some space to the left of the legend
  fig9a[[1]] + theme(legend.box.margin = margin(0, 0, 0, 0))
)
legend_bottom <- get_legend(
  # create some space to the left of the legend
  fig9b[[1]] + theme(legend.box.margin = margin(0, 0, 0, 0))
)

# LRP Increasing M scenario for Supp material
fig9ahg <- fig9a[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")+
  theme(plot.margin = margin(0.,0.5,0.5,-0.5,unit="cm"))
fig9asg <- fig9a[[2]]+
  xlab("")+
  theme(axis.text.x=element_blank())+
  ylab("")+
  # theme(axis.text.y=element_blank())+
  theme(legend.position = "none")+
  theme(plot.margin = margin(0.,0.5,0.5,-0.5,unit="cm"))
fig9awc <- fig9a[[3]]+
  xlab("")+
  theme(axis.text.x=element_blank())+
  ylab("")+
  # theme(axis.text.y=element_blank())+
  theme(legend.position = "none")+
  theme(plot.margin = margin(0.,0.5,0.5,-0.5,unit="cm"))

# PLRP
fig9bhg <- fig9b[[1]]+
  theme(legend.position = "none")+
  theme(plot.margin = margin(0.,0.5,0.,-0.5,unit="cm"))
fig9bsg <- fig9b[[2]]+
  theme(legend.position = "none")+
  ylab("")+
  # theme(axis.text.y=element_blank())+
  theme(plot.margin = margin(0.,0.5,0.,-0.5,unit="cm"))
fig9bwc <- fig9b[[3]]+
  theme(legend.position = "none")+
  ylab("")+
  # theme(axis.text.y=element_blank())+
  theme(plot.margin = margin(0.,0.5,0.,-0.5,unit="cm"))

plots <- cowplot::plot_grid(NULL,fig9ahg,fig9asg,fig9awc,NULL,fig9bhg,fig9bsg,fig9bwc,
                            ncol=4, byrow=TRUE, labels=c("","(a)","(b)","(c)"),
                            rel_widths=c(0.3,3,3,3))

legends <- cowplot::plot_grid(legend_top,legend_bottom, nrow=2)
cowplot::plot_grid(plots,legends,rel_widths=c(3,0.4))
ggsave(here("Figures","Figure9_LRP_Mincrease.png"), width = 8, height = 5, bg = "white")

# LRP Constant M scenario
#margins: T R B L (TRouBLe)
fig9asupphg <- fig9asupp[[1]]+
  xlab("")+
  theme(axis.text.x=element_blank())+
  theme(legend.position = "none")+
  theme(plot.margin = margin(0.,0.5,0.5,-0.5,unit="cm"))
fig9asuppsg <- fig9asupp[[2]]+
  xlab("")+
  theme(axis.text.x=element_blank())+
  ylab("")+
  # theme(axis.text.y=element_blank())+
  theme(legend.position = "none")+
  theme(plot.margin = margin(0.,0.5,0.5,-0.5,unit="cm"))
fig9asuppwc <- fig9asupp[[3]]+
  xlab("")+
  theme(axis.text.x=element_blank())+
  ylab("")+
  # theme(axis.text.y=element_blank())+
  theme(legend.position = "none")+
  theme(plot.margin = margin(0.,0.5,0.5,-0.5,unit="cm"))

# PLRP
fig9bsupphg <- fig9bsupp[[1]]+
  theme(legend.position = "none")+
  theme(plot.margin = margin(0.,0.5,0.,-0.5,unit="cm"))
fig9bsuppsg <- fig9bsupp[[2]]+
  theme(legend.position = "none")+
  ylab("")+
  # theme(axis.text.y=element_blank())+
  theme(plot.margin = margin(0.,0.5,0.,-0.5,unit="cm"))
fig9bsuppwc <- fig9bsupp[[3]]+
  theme(legend.position = "none")+
  ylab("")+
  # theme(axis.text.y=element_blank())+
  theme(plot.margin = margin(0.,0.5,0.,-0.5,unit="cm"))

plots <- cowplot::plot_grid(NULL,fig9asupphg,fig9asuppsg,fig9asuppwc,NULL,fig9bsupphg,fig9bsuppsg,fig9bsuppwc,
                            ncol=4, byrow=TRUE, labels=c("","(a)","(b)","(c)"),
                            rel_widths=c(0.3,3,3,3))

legends <- cowplot::plot_grid(legend_top,legend_bottom, nrow=2)
cowplot::plot_grid(plots,legends,rel_widths=c(3,0.4))
ggsave(here("Figures","Figure9SUPP_LRP_Mconstant.png"), width = 8, height = 5, bg = "white")

