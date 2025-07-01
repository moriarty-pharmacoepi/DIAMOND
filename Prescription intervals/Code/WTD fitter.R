# Load required package
library(stats4)

## Define logit and inverse-logit functions
logit <- function(p) log(p / (1 - p))
inv_logit <- function(g) 1 / (1 + exp(-g))

# Width of obsevation window
delta = 500

# Reparameterized log-likelihood using transformed parameters
loglikelihood <- function(mu, log_sigma, gamma) {
  
  sigma <- (log_sigma)
  gamma <- inv_logit(gamma)
  M <- exp(mu + 0.5 * sigma^2)
  
  frd_vals <- (1 - pnorm((log(first_times) - mu) / sigma)) / M
  
  density <- gamma * frd_vals + (1 - gamma) / delta
  
  -sum(log(density))
}

# MLE with transformed parameters
fit <- mle(
  loglikelihood ,
  start = list(mu = 5,
               log_sigma = 1,
               gamma = logit(0.2)),
  method = "L-BFGS-B"
)

summary(fit)

mixture<- function(t,mu, log_sigma, gamma) {
  sigma <- (log_sigma)
  gamma <- inv_logit(gamma)
  
  M <- exp(mu + 0.5 * sigma^2)
  frd_vals <- (1 - pnorm((log(t) - mu) / sigma)) / M
  density <- gamma * frd_vals + (1 - gamma) / delta
  return(density)}

# Recover natural-scale parameters
mu_hat <- coef(fit)["mu"]
sigma_hat <- (coef(fit)["log_sigma"])
gamma_hat <- inv_logit(coef(fit)["gamma"])



# Plot histogram
hist(first_times, breaks = 50, probability = TRUE,
     main = "Fitted Mixture Model", xlab = "Time to First Observation",
     col = "lightgray", border = "white", xlim = c(0, T))


# Overlay fitted density
t_vals <- seq(0, delta, length.out = 500)
# Define a proper function of t for plotting
fitted_density <- function(t) mixture(t, mu_hat, sigma_hat, logit(gamma_hat))


curve(fitted_density(x), from = 0, to = delta, col = "blue", lwd = 3, add = TRUE)


