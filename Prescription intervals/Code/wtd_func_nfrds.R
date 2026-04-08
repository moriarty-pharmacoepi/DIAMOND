library(mclust)
library(dplyr)

logit <- function(p) log(p / (1 - p))
inv_logit <- function(g) 1 / (1 + exp(-g))
softmax <- function(x) {
  ex <- exp(x)  # stable
  ex / sum(ex)
}

WTD_fit <- function(first_times, cluster_id, window, G = 3, B = 0,iter = 20,plots = TRUE,alpha_prior = 0) {
  delta <- window
  
  # Log-likelihood function for mixture of FRDs + uniform
  loglik_frd_uniform <- function(mu, log_sigma, p_raw, w, times, alpha_prior) {
    sigma <- exp(log_sigma)
    p <- softmax(p_raw)
    w_val <- inv_logit(w)
    M <- exp(mu + 0.5 * sigma^2)
    
    frds <- sapply(1:G, function(i) (1 - pnorm((log(times) - mu[i]) / sigma[i])) / M[i])
    dens <- (w_val* frds %*% p) + ((1 - w_val) / delta)
    
    ll <- sum(log(dens))
    
    # optional mild Dirichlet prior to discourage exact-zero components
    if (!is.null(alpha_prior) && alpha_prior > 1) {
      logprior <- sum((alpha_prior - 1) * log(p))
    } else logprior <- 0
    
    # negative for minimization
    return(-(ll + logprior))
  
  }
  
  # Initial fit on full data
  log_times <- log(first_times)
  gmm_fit <- Mclust(log_times, G = G,initialization = list(hcPairs = NULL))
  mu_start <- gmm_fit$parameters$mean
  sigma_start <- log(sqrt(gmm_fit$parameters$variance$sigmasq))
  #print(paste0("mu start: ", mu_start))
  #print(paste0("log sigma start: ", sigma_start))
  p_start <- rep(1, G)
  w_start <- -1
  par_start <- c(mu_start, sigma_start, p_start, w_start)
  
  mu_idx <- 1:G
  sigma_idx <- (G+1):(2*G)
  p_idx <- (2*G+1):(3*G)
  w_idx <- 3*G + 1
  
  lower = c(rep(-Inf, G), rep(log(0.001), G), rep(-Inf, G), -100)
  upper = c(rep(Inf, G), rep(Inf, G), rep(Inf, G), 100)
  
  fit_once <- optim(
    par = par_start,
    fn = function(par) {
      mu <- par[mu_idx]
      log_sigma <- par[sigma_idx]
      p_raw <- par[p_idx]
      w <- par[w_idx]
      loglik_frd_uniform(mu, log_sigma, p_raw, w, first_times,alpha_prior)
    },
    method = "L-BFGS-B",
    lower = lower,
    upper = upper,
    control = list(maxit = iter)
  )
  print(fit_once$convergence)
  par_opt <- fit_once$par
  
  par_opt_mu <- par_opt[mu_idx]
  par_opt_sigma <- par_opt[sigma_idx]
  par_opt_p <- par_opt[p_idx]
  
  ord <- order(par_opt_mu)
  
  par_opt[mu_idx] <- par_opt_mu[ord]
  par_opt[sigma_idx] <- par_opt_sigma[ord]
  par_opt[p_idx] <- par_opt_p[ord]
  
  # Compute log-likelihood at optimum
  logLik_opt <- -loglik_frd_uniform(
    par_opt[mu_idx],
    par_opt[sigma_idx],
    par_opt[p_idx],
    par_opt[w_idx],
    first_times,
    alpha_prior
  )
  
  # Number of parameters
  k <- length(par_opt)
  n <- length(first_times)
  
  # BIC
  AIC_val <- 2*k - 2*logLik_opt
  BIC_val <- log(n)*k - 2*logLik_opt
  
  
  # --- Cluster bootstrap ---
  clusters <- unique(cluster_id)
  n_params <- length(par_opt)
  boot_mat <- matrix(NA, nrow = B, ncol = n_params)
  t_vals <- seq(0, delta, length.out = 10000)
  boot_CDF_mat <- matrix(NA, nrow = B, ncol = length(t_vals))
  if (B!=0){ 
    print("Bootstrapping for standard errors")
  for (b in 1:B) {
    print(b)
    # Sample clusters with replacement
    sampled_clusters <- sample(clusters, length(clusters), replace = TRUE)
    idx <- unlist(lapply(sampled_clusters, function(cl) which(cluster_id == cl)))
    
    first_times_boot <- first_times[idx]
    
    
    
    
    IAD_boot <- function(mu, log_sigma, lambda_hat, times) {
      f <- sapply(1:length(mu), function(i) dlnorm(times, meanlog = mu[i], sdlog = log_sigma[i]))
      f <- colSums(lambda_hat %*% t(f))
      return(f)
    }
    
    IAD_CDF_boot <- function(mu, sigma, lambda_hat, times) {
      F <- sapply(1:length(mu), function(i)
        plnorm(times, meanlog = mu[i], sdlog = sigma[i]))
      F <- colSums(lambda_hat %*% t(F))
      return(F)
    }
    
    # Fit model on bootstrap sample
    boot_fit <- tryCatch({
      res <- optim(
        par = par_opt,
        fn = function(par) {
          mu <- par[mu_idx]
          log_sigma <- par[sigma_idx]
          p_raw <- par[p_idx]
          w <- par[w_idx]
          loglik_frd_uniform(mu, log_sigma, p_raw, w, first_times_boot, alpha_prior)
        },
        method = "L-BFGS-B",
        lower = lower,
        upper = upper,
        control = list(maxit = iter)
      )
      
      boot_par <- res$par
      
      # ----- LABEL SWITCHING FIX -----
      mu_b <- boot_par[mu_idx]
      log_sigma_b <- boot_par[sigma_idx]
      p_raw_b <- boot_par[p_idx]
      
      # sort components by increasing mu
      ord <- order(mu_b)
      
      # reorder all component-specific parameters
      mu_b <- mu_b[ord]
      sigma_b <- (log_sigma_b[ord])
      p_b <- (p_raw_b[ord])
      
      # reassemble full parameter vector
      boot_par[c(mu_idx, sigma_idx, p_idx)] <- c(mu_b, sigma_b, p_b)
      # --------------------------------
      mu_b <- boot_par[mu_idx]
      sigma_b <- exp(boot_par[sigma_idx])
      p_b <- softmax(boot_par[p_idx])
      
      means_b <- exp(mu_b + 0.5 * sigma_b^2)
      lambda_b <- (p_b / means_b) / sum(p_b / means_b)
      
      boot_IAD_mat[b, ] <- IAD_boot(mu_b, sigma_b, lambda_b, t_vals)
      boot_par
    }, error = function(e) rep(NA, n_params))
    
    boot_CDF_mat[b, ] <- IAD_CDF_boot(mu_b, sigma_b, lambda_b, t_vals)
  }
    
    # Compute cluster bootstrap standard errors
    se_boot <- apply(boot_mat, 2, sd, na.rm = TRUE)
    
  } else {
    se_boot <- NA
  }
  
  
 
  
  CDF_CI_lower <- apply(boot_CDF_mat, 2, quantile, probs = 0.025, na.rm = TRUE)
  CDF_CI_upper <- apply(boot_CDF_mat, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  
  
  mu_hat <- par_opt[mu_idx]
  sigma_hat <- exp(par_opt[sigma_idx])
  p_hat <- par_opt[p_idx]
  w_hat <- (par_opt[w_idx])

  
  # Compute means and weights
  means <- exp(mu_hat + 0.5 * sigma_hat^2)
  w <- inv_logit(w_hat)
  p <- softmax(p_hat)
  lambda_hat <- (p / means) / sum(p / means)
  
  # FRD functions
  frd_funcs <- lapply(1:G, function(i) {
    function(t) (1 - pnorm((log(t) - mu_hat[i]) / sigma_hat[i])) / means[i]
  })
  
  # Evaluate mixture
  t_vals <- seq(0, delta, length.out = 10000)
  frd_y <- sapply(1:G, function(i) w*p[i] * frd_funcs[[i]](t_vals))
  uniform_y <- (1 - w) / delta
  mixture_y <- rowSums(frd_y) + uniform_y
  #mixture_y <- mixture_y / sum(mixture_y*0.36)
  
  
  IAD <- function(mu, log_sigma, lambda_hat, times) {
    
    f <- sapply(1:length(mu), function(i) dlnorm(times, meanlog = mu[i],sdlog = log_sigma[i]))
    
    f = colSums(lambda_hat %*% t(f))
    
  }
  
  
  # 1. Prepare data for ggplot
  plot_data <- data.frame(
    t = t_vals,
    Mixture = mixture_y,
    Uniform = rep(uniform_y, length(t_vals)),
    inter_arrival_density = IAD(mu_hat,sigma_hat,lambda_hat, t_vals)
  )
  
  # Add FRDs as separate columns
  for (i in 1:G) {
    plot_data[[paste0("FRD", i)]] <- frd_y[, i]
    plot_data[[paste0("IAD", i)]] <- IAD(mu_hat[i],sigma_hat[i],lambda_hat[i], t_vals)
  }
  #print(str(plot_data))
  # Convert to long format for ggplot
  plot_data_long <- plot_data %>%
    dplyr::select(-inter_arrival_density,-starts_with("IAD")) %>%   
    pivot_longer(-t, names_to = "Component", values_to = "Density")
  
  # 2. Plot mixture, FRDs, and uniform with ggplot
  if (plots==TRUE){
  p<-ggplot() +
    geom_histogram(aes(x = first_times, y = after_stat(density)),
                   binwidth =3, fill = "grey20", color = "grey40", alpha = 0.6) +
    geom_line(data = plot_data_long, aes(x = t, y = Density, color = Component), size = 1) +
    coord_cartesian(xlim = c(0, delta)) +
    labs(x = "Time", y = "Density", title = "Fitted mixture and components", color = "Component") +
    theme_bw() +
    xlim(0, delta) +
    theme(legend.position = "top")
  
  # 3. Print the plot
    print(p)
  }
  
  plot_data_iad <- plot_data %>%
    dplyr::select(t, starts_with("IAD")) %>%
    pivot_longer(-t, names_to = "Component", values_to = "Density")
  
  
  
  # Return
  list(
    mu = par_opt[mu_idx],
    log_sigma = par_opt[sigma_idx],
    p_raw = par_opt[p_idx],
    w = par_opt[w_idx],
    delta = delta,
    lambda_hat = lambda_hat,
    means = means,
    G = G,
    cluster_se = se_boot,
    bootstrap_matrix = boot_mat,
    BIC = BIC_val,
    AIC = AIC_val,
    logLik = logLik_opt,
    plot_data = plot_data,
    CDF_CI_lower = CDF_CI_lower,
    CDF_CI_upper = CDF_CI_upper,
    boot_CDF_mat = boot_CDF_mat
  )
}

#


# first_rx<-first_rx%>%
#   sample_frac(0.1)
# 
# # window = 365
# G <- 3       # number of FRDs
# start_time <- Sys.time()
#fit <- WTD_fit(first_rx$days_to_first,first_rx$UniquePatientID, window, G,iter = 10, B=10)
# end_time = Sys.time()
# print(end_time-start_time)
# 
# 
# Gmax <- 10
# 
# results <- data.frame(
#   G = 1:Gmax,
#   logLik = NA_real_,
#   AIC = NA_real_,
#   BIC = NA_real_,
#   CV_logLik = NA_real_
# )
# 
# n <- length(first_rx$days_to_first)
# 
# K <- 2
# folds <- sample(1:K, n, replace = TRUE)
# 
# for (G in 1:Gmax) {
#   cv_ll <- numeric(K)
#   
#   for (k in 1:K) {
#     train_idx <- which(folds != k)
#     test_idx <- which(folds == k)
#     
#     fit <- tryCatch({
#       WTD_fit(first_rx$days_to_first[train_idx],
#               first_rx$UniquePatientID[train_idx],
#               window = 365,
#               G = G)
#     }, error = function(e) NULL)
#     
#     if (is.null(fit)) {
#       cv_ll[k] <- NA
#       next
#     }
#     
#     # Extract parameters
#     mu <- fit$mu
#     log_sigma <- fit$log_sigma
#     p_raw <- fit$p_raw
#     w_val <- inv_logit(fit$w)
#     p <- exp(p_raw) / sum(exp(p_raw))
#     means <- exp(mu + 0.5 * exp(log_sigma)^2)
#     
#     # Evaluate log-likelihood on test data
#     frds <- sapply(1:G, function(i) (1 - pnorm((log(first_rx$days_to_first[test_idx]) - mu[i]) / exp(log_sigma[i]))) / means[i])
#     dens <- w_val * frds %*% p + (1 - w_val)/365
#     cv_ll[k] <- sum(log(dens))
#   }
#   
#   # Save results
#   results$CV_logLik[results$G == G] <- mean(cv_ll, na.rm = TRUE)
#   results$BIC[results$G == G] <- if (!is.null(fit)) fit$BIC else NA
#   results$AIC[results$G == G] <- if (!is.null(fit)) fit$AIC else NA
# }
# 
# results
# 
# # Convert to long format for plotting
# results_long <- results %>%
#   pivot_longer(cols = c(AIC, BIC),
#                names_to = "Criterion",
#                values_to = "Value")
# 
# # Plot
# plots<- ggplot(results_long, aes(x = G, y = Value, color = Criterion, group = Criterion)) +
#   geom_line(size = 1) +
#   geom_point(size = 2) +
#   scale_x_continuous(breaks = results$G) +
#   labs(
#     x = "Number of Components (G)",
#     y = "Information Criterion",
#     title = "Model Selection Criteria vs. Number of Components",
#     color = "Criterion"
#   ) +
#   theme_bw() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     legend.position = "top"
#   )
# 
# print(plots)


