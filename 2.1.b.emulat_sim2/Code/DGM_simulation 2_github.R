# Author: Ryan Muddiman
# Date of initial development (YYYY-MM-DD): 2026-07-22
# Purpose: This script is the main simulation for generating a large synthetic
# population health dataset, including health records,
# with time-varying confounding.

# library(rstudioapi)
# # Get the path of the current script
# script_path <- rstudioapi::getActiveDocumentContext()$path
# # Print the current scripts path
# print(script_path)
# setwd(dirname(script_path))
#set.seed(2)

script_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(script_path))

# Load the required packages
required_packages <- c("survival",
                       "ipw",
                       "dplyr",
                       "tidyr",
                       "patchwork",
                       "mstate",
                       "parallel",
                       "doRNG",
                       "foreach",
                       "progress",
                       "SimMultiCorrData",
                       "doSNOW",
                       "rsimsum",
                       "flexsurv",
                       "MASS",
                       "data.table",
                       "ggplot2")


missing_packages <- required_packages[!(required_packages %in% installed.packages())]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Now load the packages
lapply(required_packages, library, character.only = TRUE)


num_cores <- detectCores()







generate_simulated_data <- function(nsim = 1,num_patients = 200,treat_strat = "taper",factor_seed = 9){
  
  # initialize cluster for parrallel computation
  cl <- makeCluster(num_cores-1)
  set.seed(factor_seed)
  #setwd("~/R Scripts")
  #source("functions.r")
  registerDoSNOW(cl)
  
  if (!dir.exists("results")) {
    dir.create("results")
  }
  
  sim_seed <- vector("list", nsim)
  
  
  # Progress bar for parallel processing (1 tick = 1 simulation repetition)
  pb <- progress_bar$new(
    format = "  Progress [:bar] :percent ETA: :eta Elapsed: :elapsed",
    total = nsim , clear = FALSE, width = 60
  )
  # Define progress function
  progress <- function(n) pb$tick()
  opts <- list(progress = progress)  
  
  num_covariates<-6

  simResults<-  foreach(
    sim = 1:nsim,
    .options.snow = opts,
    .options.RNG = 1,
    .packages = c("tidyr","dplyr", "mstate", "flexsurv", "survival","MASS")
  ) %do% {

covariate_names <- paste0("cov", seq_len(num_covariates))

ids <- seq_len(num_patients)

cov1<-rbinom(num_patients,1,0.5)

mu <- c(10, 5)

# Covariance matrix
Sigma <- matrix(c(
  100, 4,
  4, 4
), nrow = 2)

# Generate 1000 observations
data <- mvrnorm(
  n = num_patients,
  mu = mu,
  Sigma = Sigma
)


cov2<-data[,1]
cov3<-data[,2]
cov4<- 1 # starting dose
cov5<- 0 # AE indicator
cov6<-0

data<-cbind(cov1,cov2,cov3,cov4,cov5,cov6)

init_covariates <- matrix(data = data, nrow = num_patients, ncol = num_covariates)

# Create initial covariate dataframe
Patient_pop <- data.frame(
  id = ids,
  init_covariates
)

colnames(Patient_pop)[-1] <- covariate_names

# Function to generate a Q-matrix from hazard rates
generate_Q_matrix_from_hazards <- function(hazards) {
  # Get the number of states (n)
  n <- nrow(hazards)
  # Initialize the Q matrix with zeros
  Q <- matrix(0, nrow = n, ncol = n)
  # Fill in the off-diagonal elements (hazard rates)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        Q[i, j] <- hazards[i, j]
      }
    }
  }
  # Adjust the diagonal elements to ensure row sums are zero
  for (i in 1:n) {
    Q[i, i] <- -sum(Q[i, -i])  # Ensure row sum is 0
  }
  return(Q)
}

# Compute cumulative hazard and sample sojourn time
compute_cumulative_hazard <- function(hazard_params, clock, covariates_fn,dose_reduction,adverse_event_occur) {
  
  # Total hazard at time t (scalar or vector)
  total_hazard <- function(t) {
    t_plus_clock <- t + clock
    covariates_at_t <- covariates_fn(t_plus_clock,dose_reduction,adverse_event_occur)  # n x p matrix if multiple subjects
   
    total_hazard_value <- rep(0, length(t))
    
    for (i in seq_len(nrow(hazard_params))) {
      hazard <- hazard_params[i, ]
      #print(hazard)
      # compute covariate effect: sum_j cov_j * (beta_j + gamma_j * t)
      beta <- as.numeric(hazard$effects)          # numeric vector
      gamma <- as.numeric(hazard$time_effects)    # numeric vector
      
      cov_effect <- rowSums(covariates_at_t * (matrix(beta, nrow = length(t), ncol = length(beta), byrow = TRUE) +
                                                 outer(t_plus_clock, (gamma))))
      
      
      # baseline hazard
      h0 <- switch(hazard$type,
                   exp = rep(hazard$rate, length(t)),
                   weibull = (hazard$shape / hazard$scale) * (t + tsince_d)^(hazard$shape - 1)
                   )  # baseline Gompertz hazard
      
      total_hazard_value <- total_hazard_value + h0 * exp(cov_effect)
    }
    
    total_hazard_value
  }
  
  # cumulative hazard up to time t
  H <- function(t) integrate(total_hazard, lower = 1e-15, upper = t, subdivisions = 100L)$value
  
  # inversion sampling: solve H(T) = -log(U)
  target_H <- -log(runif(1))
  sojourn_time <- uniroot(function(t) H(t) - target_H, interval = c(1e-15, 1000),
                          maxiter = 1000, tol = 1e-10)$root
  
  return(sojourn_time)
}

# Computes the static cause specific hazard at a specific time
get_cause_specific_hazard <- function(state, clock, sojourn_time, hazard_params, covariates_fn,dose_reduction,adverse_event_occur) {
  
  state_hazards <-subset(hazard_params, !is.na(type) & from==state)  # list of hazards for this state
  #print(state_hazards)
  n_causes <- nrow(state_hazards)
  
  # compute covariates at current clock time
  covariates_at_t <- covariates_fn(clock,dose_reduction,adverse_event_occur)   # numeric vector
  
  # precompute time effect (scalar)
  time_effect <- clock
  
  # initialize output
  cause_specific_hazards <- numeric(n_causes)
  
  for (i in seq_len(n_causes)) {
    h <- state_hazards[i,]
    
    beta  <- as.numeric(h$effects)        # numeric vector
    gamma <- as.numeric(h$time_effects)   # numeric vector
    
    # covariate + time effect
    linear_predictor <- beta + gamma * time_effect
    covariate_effect <- exp(sum(covariates_at_t * linear_predictor))  # scalar multiplier
    
    # baseline hazard
    base_haz <- switch(h$type,
                       exp      = h$rate,
                       weibull  = if (h$scale > 0 && is.finite(h$scale)) (h$shape / h$scale) * sojourn_time^(h$shape - 1) else 0
                        
    )
    
    cause_specific_hazards[i] <- base_haz * covariate_effect
  }
  
  return(cause_specific_hazards)
}



# Defining continuous time multi-state model function ---------------------------------------------------
simulate_cmc <- function(time, hazard_params,baseline_covariates,starting_state) {
  # State space is indexed from 1 
  state_space <- 1:4 
  # Vector to keep track of time spent in each state
  time_spent <- 0
  # The simulation clock
  clock <- 0  
  # Initial state 
  current_state <- starting_state  
  
  previous_state <- starting_state
  previous_reduction <- 0
  adverse_event_occur <- 0
  
  names <- c('state', 'time_spent')
  
  max_steps <- 1000  
  history_data <- vector("list", max_steps)
  step <- 1
  
  history_data[[step]] <- data.frame(
    state = current_state,
    time_spent = 0,
    matrix(baseline_covariates, nrow = 1)
    
  )
  
  step <- step + 1
  
  n_events = 0
  is_deprescribed_ind = if_else(starting_state==1,0,1)
  ever_deprescribed_ind = if_else(starting_state==1,0,1)
  deprescribed_time = 0
  flag<-0
  
  dose_reduction<-0
  
  # function defining covariates
  covariates_fn <- function(t,dose_reduction,adverse_event_occur) {
    # Sex
    z1 <- rep(baseline_covariates[1], length(t))             
    # Age
    z2 <- rep( baseline_covariates[2], length(t))    + t
    # Comorbidity
    z3 <- rep( baseline_covariates[3], length(t))    + 0.01*t
    
    if (treat_strat == "taper"){
    # Dose now
    z4 <- max(rep(history_data[[step-1]]$X4, length(t))  - dose_reduction*ifelse(current_state==3, 0.5,0),0)
    }else if (treat_strat == "continue"){
    z4 <- rep(history_data[[step-1]]$X4, length(t)) 
    }
    
    z5 <- dose_reduction
    
    z6 <- adverse_event_occur
    
    # Combine into a matrix
    return(cbind(z1, z2, z3, z4, z5, z6))
  }

  #print(history_data[[1]]$X1)
  
  while (clock < time) {
    # Get hazard parameters for all transitions from the current state
    state_hazards <- subset(hazard_params, !is.na(type) & from==current_state)
    
     
    sojourn_time <- compute_cumulative_hazard(state_hazards, clock, covariates_fn,dose_reduction,adverse_event_occur)
    
    eta <- -2 +
     1.6 * previous_reduction -
      -1 * adverse_event_occur
    
    p <- plogis(eta)
    
    dose_reduction <- rbinom(1, 1, p)
    
    previous_reduction <-dose_reduction
    
    #print(step)
    covs <- matrix(covariates_fn(clock,dose_reduction,adverse_event_occur), nrow = 1)
     # Update the simulation clock
    clock <- clock + sojourn_time
    # Update time spent in the current state
    time_spent <- time_spent + sojourn_time
  
    # Record history and note covariates at the time of exiting previous state (entering current state)
    
    # Define a temporary history dataframe for saving results
    history_data[[step]] <- data.frame(
      state = current_state,
      time_spent = sojourn_time,
      covs
    )
    step <- step + 1
    
    # Expand list size if necessary
    if (step > length(history_data)) {
      length(history_data) <- length(history_data) * 2
    }
    
    # Determine possible states to transition to
    possible_states <- state_hazards$to
    
    #possible_states <- possible_states[possible_states != current_state]
    # This defines the clock forward behaviour for deprescribing
    cause_specific_hazards <- get_cause_specific_hazard(current_state,clock,sojourn_time,hazard_params,covariates_fn,dose_reduction,adverse_event_occur)
    
    # Calculate vector of probabilities using relative hazard
    transition_probs = cause_specific_hazards/sum(cause_specific_hazards)
    
    previous_state <- current_state
    #print(transition_probs)
    # If only one state possible, thats next with probability 1
    if (length(possible_states)==1){
      current_state <- possible_states
    }
    else {
      # Next state is based on a multinomial draw  
      current_state <- sample(possible_states, size = 1, prob = transition_probs)
    }
    
    # Internal covariates ------
    
    # adverse event indicator
    if (current_state %in%  4) {
      adverse_event_occur <- 1
    }
    
    
    # If dead, stop simulation
    if (current_state %in%  2) {
      history_data[[step]] <- data.frame(state = current_state, time_spent = 0, covs)
      step <- step + 1
      break
    }
    
    
   
    
  
  }
  history <- do.call(rbind, history_data[1:(step - 1)])
  colnames(history) <- names
  return(list(time_spent = time_spent, history = history))
}


hazards <- matrix(c(
  0.0, 0.05, 0.6, 0.08,   # 1 Initial → 1,2,3,4
  0.0, 0.0, 0.0, 0.0, # 2 Death
  0.0, 0.05, 0.9, 0.1,    # 3 Visit
  0.0, 0.1, 0.5, 0.0     # 4 AE
), nrow = 4, ncol = 4, byrow = TRUE)


effects_matrix<- matrix(
  c(
    0, 0, 0, 0, 0,0, #1
    0,  0.1, 0.1,0,0,0,#2 #1->2
    0, 0, 0, 0,  0,0, #3
    0, 0, 0, 0,  0,0, #4
    0, 0, 0, 0,  0,0, #5 2->1
    0, 0, 0, 0,  0,0, #6 # 2->2
    0, 0, 0, 0,  0,0, #7
    0, 0, 0, 0,  0,0, #8  2->4
    0, 0, 0, 0,  0,0, #9
    0, 0, 0, 2,  0,0, #10  3->2
    0, 0, 0, 0,  2,1, #11  3->3
    0.1,0.1, 0,0,0,0, #12 Sex 3->4
    0, 0, 0, 0, 0 ,0, #13
    0.1,0.1, 0,  0.0, 0,0, #14 4->2
    0, 0, 0, 0,  0,0, #15
    0, 0, 0, 0, 0,0 #16
  ),
  nrow = 16,
  byrow = TRUE
)

library(knitr)
kable(as.data.frame(effects_matrix), digits = 1)

library(tibble)
  
hazard_params <- tibble(
  from = rep(1:4, each = 4),
  to   = rep(1:4, 4),
  type = c(
    NA,"exp","exp","exp",
    NA,NA,NA,NA,
    NA,"exp","exp","exp",
    NA,"exp","exp",NA
  ),
  rate = c(
    hazards[1,1], hazards[1,2], hazards[1,3], hazards[1,4],
    hazards[2,1], hazards[2,2], hazards[2,3], hazards[2,4],
    hazards[3,1], hazards[3,2], hazards[3,3], hazards[3,4],
    hazards[4,1], hazards[4,2], hazards[4,3], hazards[4,4]
  ),
  shape = rep(NA, 16),
  scale = rep(NA, 16),
  a = rep(NA, 16),
  b = rep(NA, 16),
  effects = effects_matrix,
  time_effects = matrix(0, 16, num_covariates)
)


Q <- generate_Q_matrix_from_hazards(hazards)


#time unit: months
time <- 1000

# Initialize an empty list to store the full histories of states and times for each patient
history_list <- vector(mode = "list", length = num_patients)
# Initialize an empty vector to store the time spent in all states for each patient
history_time_spent <- vector(mode = "list", length = 5)


# Main simulation loop ----------------------------------------------------

# Single simulation repetition loop
covariate_matrix <- as.matrix(Patient_pop[, 2:(num_covariates+1)])

results <- lapply(1:num_patients, function(i) {
  baseline_covariates <- covariate_matrix[i, ]
  out <- simulate_cmc(time, hazard_params, baseline_covariates,starting_state=1)
  history <- out$history
  # Sample a random censoring time per patient
  censor_time = rexp(1,0.02) 
  patient_id <- rep(i, nrow(history))
  history_with_id <- cbind(Patient_ID = patient_id, history,time_to_censor = censor_time)
  time_spent <- history[, 2]
  list(history = history_with_id, time_spent = time_spent)
})

history_list <- lapply(results, `[[`, "history")
history_time_spent <- lapply(results, `[[`, "time_spent")
# Flatten the list into a single data frame
flat_history <- do.call(rbind, history_list)
# Assign column names (adjust according to the structure of your `history`)
colnames(flat_history) <- c("Patient_ID", "State", "Time_Spent",(covariate_names),"censor_time")
# Convert the flattened history into a data frame
df_history <- as.data.frame(flat_history)
# Convert Patient_ID and State to factors
df_history$Patient_ID <- factor(df_history$Patient_ID)
df_history$State <- factor(df_history$State)









# This dataframe is just for plotting
df_history2 <- df_history %>%
  group_by(Patient_ID) %>%
  mutate(Cumulative_Time = cumsum(Time_Spent)) %>%
  mutate(next_time = lag(Cumulative_Time)) %>%  # Get the next time for each state transition
  filter(!is.na(next_time))



state_names <-c("Baseline", "Death",  "Visit", "AE")
tmat <- transMat(x = list(c(2, 3, 4),c(), c(2,3,4), c(2,3)),
                 names = state_names)
# 
# # Convert the transition matrix to a regular matrix
trans_matrix <- as.matrix(tmat)
# # Get the indices for the states
state_indices <- seq_along(rownames(trans_matrix))
# # Create a data.frame using the indices for from and to
tmat_df <- as.data.frame(as.table(trans_matrix)) %>%
  rename(trans = Freq) %>%
  rowwise%>%
  mutate(from =  as.factor(which(state_names==from)),
         to =  as.factor(which(state_names==to)))
# # Now, use the left_join to merge with ms_history
# 
tmat_df <- tmat_df%>%
  rowwise()%>%
  mutate(transition = paste(state_names[from],">",state_names[to]))%>%
  ungroup()


# 
# ###############
# # Ensure the data is ordered correctly
df_history <- df_history %>%
  group_by(Patient_ID) %>%
  mutate(time_to_death = sum(Time_Spent)) %>%
  ungroup()




ms_history <- df_history %>%
  filter(Time_Spent != 0) %>%
  group_by(Patient_ID) %>%
  mutate(
    from = State,
    to = lead(State, default = as.factor(2)),
    TStart = lag(cumsum(Time_Spent), default = 0),
    TStop = cumsum(Time_Spent),
    status = 1
  ) %>%
  left_join(tmat_df, by = c("from", "to")) %>%
  ungroup()

class(ms_history) <- c("msdata", class(ms_history))
attr(ms_history, "trans") <- tmat

#print(events(ms_history))

#M<-ms_history
result_list<-list(ms_history=ms_history,
                  factor_seed = factor_seed)

#saveRDS(result_list, file = file.path("results", paste0("sim_", sim, ".rds")))
result_list

}
  stopCluster(cl)
  
  return(simResults)
} # end generate_simulated_data function

print("Finished generating data")


# Calculating the true value of the estimand ------------------------------
data_taper_raw<-generate_simulated_data(treat_strat = "taper")
data_continue_raw<-generate_simulated_data(treat_strat = "continue")

library(dplyr)
library(purrr)

overall_prop <- bind_rows(map(data_taper_raw, "ms_history")) %>%
  group_by(Patient_ID) %>%
  summarise(nvisits = sum(to == 3), .groups = "drop") %>%
  summarise(prop = mean(nvisits >= 5)) %>%
  pull(prop)



visit_dist <- bind_rows(map(data_taper_raw, "ms_history")) %>%
  group_by(Patient_ID) %>%
  summarise(nvisits = sum(to == 3), .groups = "drop")


ggplot(visit_dist, aes(x = nvisits)) +
  geom_histogram(binwidth = 1, boundary = -0.5) +
  scale_x_continuous(breaks = 0:max(visit_dist$nvisits)) +
  labs(
    x = "Number of visits",
    y = "Number of patients",
    title = "Distribution of visits per patient"
  ) +
  theme_bw()


print(overall_prop)

library(dplyr)
library(tidycmprsk)
library(ggsurvfit)

prepare_data <- function(data) {
  data %>%
    mutate(
      status = as.factor(if_else(to == 2, 1, 0))
    ) %>%
    filter(to %in% c(2)) %>%
    group_by(Patient_ID) %>%
    slice_head(n = 1) %>%
    ungroup()
}

nsim <- length(data_taper_raw)

risk_diffs_true <- numeric(nsim)

for (i in seq_len(nsim)) {
  
  data_taper <- prepare_data(data_taper_raw[[i]]$ms_history)
  data_continue <- prepare_data(data_continue_raw[[i]]$ms_history)
  
  result <- bind_rows(
    mutate(data_continue, group = "Continue"),
    mutate(data_taper, group = "Taper")
  )
  
  marginal_risk <- cuminc(
    Surv(TStop, status) ~ group,
    data = result
  )
  
  risk5 <- tidy(marginal_risk, times = 5)
  
  risk_curves <- marginal_risk$tidy %>%
    dplyr::select(time, strata, estimate)
  
  risk_diffs_true[i] <- with(risk5, estimate[strata == "Taper"] - estimate[strata == "Continue"])
}

risk_diffs_true


# Generating the natural course (data for estimation) --------------------------

data_natural_course <- lapply(data_taper_raw, function(x) {
  x$ms_history %>%
    group_by(Patient_ID) %>%
    mutate(
      censored = as.integer(TStop > censor_time),
      first_censored = censored == 1 & cumsum(censored) == 1
    ) %>%
    filter(censored == 0 | first_censored)
})
  


# Plotting ----------------------------------------------------------------



library(ggplot2)
library(GGally)
library(patchwork)


# Function for plotting event histories

# This code plots the 1st 20 patients event histories for dataset 1 (nsim=1)
  
  df<-data_taper_raw[[1]]$ms_history%>%
    ungroup()%>%   #important: remove any existing grouping
    filter(Patient_ID %in% seq(1, 20))
  num_patients <- dplyr::n_distinct(df$Patient_ID)
  
  plot<-ggplot(df, aes(x = TStart, xend = TStop, y = Patient_ID, yend = Patient_ID)) +
    geom_segment(data = dplyr::filter(df, State != 5 & Time_Spent > 0),
                 aes(color = as.factor(State)), size = 5) +
    geom_point(
      aes(x = time_to_death, y = Patient_ID,
      shape = "dead"),
      size = 5,
      color = "black"
    )+
    
    geom_point(data = dplyr::filter(df, to==3),
      aes(x = TStop, y = Patient_ID,
          shape = "Visit"),
      size = 2,
      color = "black"
    )+
     scale_shape_manual(values = c("dead" = 20,"Visit" = 3),
                        labels = c("Dead","Visit")) 
     theme_bw() 
     labs(title = "Ground Truth",
          x = "Time (months)",
          y = "Patient ID",
          color = "State",
          shape = "State") 
    # theme(text = element_text(size = 12))

  print(plot)
  
df_dose<-data_taper_raw[[1]]$ms_history%>%
    dplyr::select(cov4,TStart,Patient_ID)
    
# This code plots the dose trajectories of all patients for nsim=1
plot_dose <- ggplot(df_dose) +
  geom_line(
    aes(x = TStart, y = cov4, color = Patient_ID),
    alpha = 0.4,
    linewidth = 2,
  ) +
  #geom_point(aes(y = cov4, x=TStart))+
  labs(
    title = "",
    x = "Time Since Baseline",
    y = "Dose"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
    
print(plot_dose)



