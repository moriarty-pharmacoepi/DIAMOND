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
gamma <- 0.9 ##gamma is essentially how much you trust/rely on g(r)?? - max of 1
delta <- 0.01 ##safety net in event of g(r) bring inaccurate?? - 0.01 being the biggest safety net

L <- function(r) {
  gamma * g(r) + (1 - gamma) / delta
}

##---Plot L---------------------------------------------------------------------
curve(L, from=0, to=250, n=800,
      xlab= "Time(Days)",
      ylab= "Density",
      main= "L(r)")

##----Plot Both Curves----------------------------------------------------------
curve(g, from = 0, to = 250, n = 800,
      ylim = c(0, 60),
      xlab = "Time (Days)",
      ylab = "Value",
      main = "g(r) vs L(r)",
      col = "blue", lwd = 2)

curve(L, from = 0, to = 250, n = 800,
      add = TRUE, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = c("g(r)", "L(r)"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2)
##---End---------------------------------------------------------------------------