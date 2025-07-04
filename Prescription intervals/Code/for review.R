# Load required package
library(stats4)

## Define logit and inverse-logit functions
logit <- function(p) log(p / (1 - p))
inv_logit <- function(g) 1 / (1 + exp(-g))

# Width of obsevation window
#<<<<<<< HEAD
delta = 1500
#=======
#delta = 1800

first_times<-first_rx[,3]
first_times<-first_times%>%
  drop_na()

first_times<-as.numeric(first_times$days_to_first)
#>>>>>>> 0daf750f39d6dbdf4fa07eaac688f14a99c7a763

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
  start = list(mu = 1,
               log_sigma = 1,
               gamma = 0.1),
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


# Create time values for density line
t_vals <- seq(0, delta, length.out = 500)
fitted_density <- function(t) mixture(t, mu_hat, sigma_hat, logit(gamma_hat))

# Compute fitted density values
density_vals <- fitted_density(t_vals)

# Prepare data frame for the fitted line
density_df <- data.frame(t = t_vals, density = density_vals)

# Plot normalized histogram and fitted line
graph <- ggplot(first_rx, aes(x = days_to_first)) +
  geom_histogram(aes(y = after_stat(density)),  # Normalize histogram
                 binwidth = 30,
                 closed = "left",
                 fill = "steelblue",
                 colour = "black") +
  geom_line(data = density_df, aes(x = t, y = density),  # Fitted density curve
            color = "red", size = 1) +
  labs(
    title = "Graph of WTD for Statin Prescriptions From Study Start",
    x = "Days from study start to first script",
    y = "Density"
  ) +
  theme_minimal()

# Display the plot
plot(graph)



# Overlay fitted density

# Define a proper function of t for plotting






