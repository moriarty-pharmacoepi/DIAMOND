library(tidyverse)
##----given info----------------------------------------------------------------

mu <- 4.447
sigma <- sqrt(0.105)

##----Mean IAD------------------------------------------------------------------

M <- exp(mu + sigma^2 / 2)

##----g(r)----------------------------------------------------------------------

g <- function(r) {
  F_r <- plnorm(r, meanlog = mu, sdlog = sigma)
  (1-F_r) / M
}

##----Curve---------------------------------------------------------------------

curve(g, from=0, to=250, n=800,
      xlab= "Time(Days)",
      ylab= "Density",
      main= "Figure 2")

##----Define L(r)---------------------------------------------------------------
gamma <- 0.5  # gamma is prevalence (50 % are already prescribed at time zero)
delta <- 2500 # delta is the width of the observation window (in days), depends on plot/data

L <- function(r) {
  (gamma * g(r)) + ((1 - gamma) / delta)
}

##---Plot L---------------------------------------------------------------------

##----Plot Both Curves----------------------------------------------------------
curve(g, from = 0, to = 2500, n = 800,
      xlab = "Time (Days)",
      ylab = "Value",
      main = "g(r) vs L(r)",
      col = "blue", lwd = 2)

curve(L, from = 0, to = 2500, n = 800,
      add = TRUE, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = c("g(r)", "L(r)"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2)
##---End---------------------------------------------------------------------------
