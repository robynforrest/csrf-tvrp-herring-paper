# Code by Chris Cahill (Reviewer # 2)
# Looking at alternative parameterization of SB0
# What is being assumed about alpha and beta?
# Annotated by RF June 26, 2026

v <- c(0.05, 0.20, 0.50, 0.85, 0.97, 1.00, 1.00, 1.00) # selectivity
wa <- c(0.02, 0.05, 0.09, 0.13, 0.17, 0.20, 0.22, 0.24) # weight at age
mat <- c(0.00, 0.10, 0.55, 0.95, 1.00, 1.00, 1.00, 1.00) # maturity at age

# RF reparameterise to get K in terms of steepness
h<-0.7
k <-  4*h/(1-h) #5 # k = CR = Compensation ratio (=4h/(1-h))

R0 <- 10
n <- length(mat) # number of ages

# YPR function
phi_ypr <- function(F, M) {
  Za <- F * v + M
  lx <- numeric(n)
  lx[1] <- 1
  for (i in 2:n) lx[i] <- lx[i - 1] * exp(-Za[i - 1])
  lx[n] <- lx[n - 1] * exp(-Za[n - 1]) / (1 - exp(-Za[n]))
  list(
    phi = sum(lx * mat * wa),
    ypr = sum(wa * v * F / Za * (1 - exp(-Za)) * lx)
  )
}

phi0 <- function(M) phi_ypr(0, M)$phi # phi0 = SB0/R0

M0 <- 0.2 # initial M
p0b <- phi0(M0) #Baseline phi0
a0 <- k / p0b # alpha -> baseline alpha a function of K
b0 <- (k - 1) / (R0 * p0b) # beta

# Fmsy
fmsy_for <- function(M, alpha, beta) {
  if (alpha * phi0(M) <= 1) {
    return(0)
  }
  flim <- uniroot(
    function(F) alpha * phi_ypr(F, M)$phi - 1,
    c(1e-6, 12)
  )$root
  yield <- function(F) {
    x <- phi_ypr(F, M)
    (alpha * x$phi - 1) / (beta * x$phi) * x$ypr
  }
  optimize(yield, c(1e-6, flim), maximum = TRUE)$maximum
}

# Cahill Ro_h formulation
# Returns SB0 and FMSY assuming R0 and K fixed
# alpha and beta as functions of R0 and K
roh <- function(M) {
  p0 <- phi0(M)
  a <- k / p0
  b <- (k - 1) / (R0 * p0)
  c(S0 = R0 * p0, Fmsy = fmsy_for(M, a, b))
}

# Cahill suggested alternative
bot <- function(M) {
  p0 <- phi0(M)
  ap <- a0 * p0
  s0 <- if (ap > 1) (a0 * p0 - 1) / b0 else 0
  c(S0 = s0, Fmsy = fmsy_for(M, a0, b0))
}

# What we are doing in R/0_settings.R function calc_tv_B0
# starts with iscam leading parameters
paper <- function(M){
  # get alpha and beta from leading parameters (p0b would come from leading R0 and steepness)
  alpha <- k/p0b # CR/baseline phi0
  beta <- (alpha*p0b-1)/(R0*p0b)
  p0 <- phi0(M) # new phi0
  R0_new <- (alpha*p0 - 1)/(beta*p0)
  S0 <- p0*R0_new
  #S0 <- (a0*p0 - 1)/(b0)
  c(S0 = S0, Fmsy = fmsy_for(M, alpha, beta))
}

hcr <- function(B, S0, Fm) {
  lrp <- 0.2 * S0 # or whatever DFO policy pars you want...
  usr <- 0.4 * S0
  ifelse(B >= usr, Fm, ifelse(B <= lrp, 0, Fm * (B - lrp) / (usr - lrp)))
}
pur <- "#7d4fb0"
blu <- "#1b3b7a"
mcols <- c("#7fb0ec", "#6a51c0", "#3b2569")
bmax <- roh(0.2)["S0"]
bgrid <- seq(0, bmax, length.out = 500)

# ---- equilibrium sweep in M ----
#mseq <- seq(0.20, 0.55, 0.005)
mseq <- seq(0.20, 1., 0.005)

rh <- sapply(mseq, roh)
bo <- sapply(mseq, bot)
pap <-  sapply(mseq, paper)
mr <- c(0.20, 0.30, 0.40)
rrh <- lapply(mr, roh)
rbo <- lapply(mr, bot)
rpap <- lapply(mr, paper)

png(here::here("Figures","Cahill_plots_compare.png"))
  par(mfrow = c(3, 3), mar = c(4, 4.4, 2.6, 1))
  yl1 <- c(0, max(rh["Fmsy", ]))
  plot(mseq, rh["Fmsy", ],
       type = "l", lwd = 3, col = pur, ylim = yl1,
       xlab = "M", ylab = "Fmsy", main = "R0-h approach: Fmsy"
  )
  plot(mseq, bo["Fmsy", ],
       type = "l", lwd = 3, col = blu, ylim = yl1,
       xlab = "M", ylab = "Fmsy", main = "Botsford S*: Fmsy"
  )
  plot(mseq, pap["Fmsy", ],
       type = "l", lwd = 3, col = blu, ylim = yl1,
       xlab = "M", ylab = "Fmsy", main = "Paper: Fmsy"
  )

  yl2 <- c(0, 0.2 * max(rh["S0", ]))
  plot(mseq, 0.2 * rh["S0", ],
       type = "l", lwd = 3, col = pur, ylim = yl2,
       xlab = "M", ylab = "0.2 S0 (LRP)", main = "R0-h approach: 0.2 S0"
  )

  plot(mseq, 0.2 * bo["S0", ],
       type = "l", lwd = 3, col = blu, ylim = yl2,
       xlab = "M", ylab = "0.2 S0 (LRP)", main = "Botsford S*: 0.2 S0"
  )
  plot(mseq, 0.2 * pap["S0", ],
       type = "l", lwd = 3, col = blu, ylim = yl2,
       xlab = "M", ylab = "0.2 S0 (LRP)", main = "Paper: 0.2 S0"
  )

  yl3 <- c(0, max(sapply(rrh, function(z) z["Fmsy"])))
  plot(NA,
       xlim = c(0, bmax), ylim = yl3,
       xlab = "biomass B", ylab = "allowed F", main = "R0-h approach: rule"
  )
  for (i in seq_along(mr)) {
    lines(bgrid, hcr(bgrid, rrh[[i]]["S0"], rrh[[i]]["Fmsy"]),
          lwd = 3, col = mcols[i]
    )
  }

  plot(NA,
       xlim = c(0, bmax), ylim = yl3,
       xlab = "biomass B", ylab = "allowed F", main = "Botsford S*: rule"
  )
  for (i in seq_along(mr)) {
    lines(bgrid, hcr(bgrid, rbo[[i]]["S0"], rbo[[i]]["Fmsy"]),
          lwd = 3, col = mcols[i]
    )
  }

  plot(NA,
       xlim = c(0, bmax), ylim = yl3,
       xlab = "biomass B", ylab = "allowed F", main = "Paper approach: rule"
  )
  for (i in seq_along(mr)) {
    lines(bgrid, hcr(bgrid, rpap[[i]]["S0"], rpap[[i]]["Fmsy"]),
          lwd = 3, col = mcols[i]
    )
  }

  legend("topright",
         bty = "n", lwd = 3, col = mcols, title = "M",
         legend = sprintf("%.2f", mr)
  )
dev.off()
