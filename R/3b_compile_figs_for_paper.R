# Compile the figures for the paper
# January 4, 2026
# Robyn Forrest

# !! This file is intended to be sourced in 0_run_analyses.R after the four
# 3a files are run. These files put the figure lists into the environment, which are
#  used here.

# MAKE FIGURE 2 (relationship between M and B0, 3 stocks)
fig2hg <- fig2[[1]]
fig2sg <- fig2[[2]]+
  ylab("")
fig2wc <- fig2[[3]]+
  ylab("")

cowplot::plot_grid(fig2hg,fig2sg,fig2wc, nrow=1, labels=c("(a)","(b)","(c)"))
ggsave(here("Figures","Figure2.png"))

# MAKE FIGURE 2 alternative (relationship between M and B0, 3 stocks, 10 replicates)
fig2hg_alt <- fig2_alternative[[1]]+
  theme(legend.position = "none")
fig2sg_alt <- fig2_alternative[[2]]+
  ylab("")+theme(legend.position = "none")
fig2wc_alt <- fig2_alternative[[3]]+
  ylab("")+theme(legend.position = "none")

cowplot::plot_grid(fig2hg_alt,fig2sg_alt,fig2wc_alt, nrow=1, labels=c("(a)","(b)","(c)"))
ggsave(here("Figures","Figure2_alternative.png"))

# MAKE FIGURE 3 (S-R relationships and replacement lines)
fig3hg <- fig3[[1]]+
  xlab("")
fig3sg <- fig3[[2]]+
  xlab("")
fig3wc <- fig3[[3]]

cowplot::plot_grid(fig3hg,fig3sg,fig3wc, nrow=3, labels=c("(a)","(b)","(c)"))
ggsave(here("Figures","Figure3.png"))

# MAKE FIGURE 4 (time series of SSB, M and B0)
fig4ahg <- fig4a[[1]]+
  xlab("")+
  theme(strip.text = element_blank())
fig4asg <- fig4a[[2]]+
  xlab("")+
  theme(strip.text = element_blank())
fig4awc <- fig4a[[3]]+
  theme(strip.text = element_blank())

cowplot::plot_grid(fig4ahg,fig4asg,fig4awc, nrow=3, labels=c("(a)","(b)","(c)"))


