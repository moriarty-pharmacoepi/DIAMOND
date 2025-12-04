# Author: Ryan Muddiman
# Date of initial development (YYYY-MM-DD): 2024-10-22
# Purpose: This script is the main simulation for generating a large synthetic
# population health dataset, including health records,
# with time-varying confounding.



# Load the required packages
library(survival)
library(dplyr)
library(patchwork)
library(mstate)
library(parallel)
library(doRNG)
library(foreach)
library(progress)
library(SimMultiCorrData)
library(doSNOW)


seed<-1

num_cores <- detectCores()
cl <- makeCluster(num_cores-1)

setwd("~/R Scripts")
source("functions.r")
registerDoSNOW(cl)


start_time <- Sys.time()
# Initialization of simulation parameters ---------------------------------

# function for generating multiple synthetic datasets
generate_simulated_data <- function(nsim = 40,num_patients = 2000){
  source("functions.r")
  
# Progress bar for parallel processing (1 tick = 1 simulation repetition)
pb <- progress_bar$new(
  format = "  Progress [:bar] :percent ETA: :eta Elapsed: :elapsed",
  total = nsim , clear = FALSE, width = 60
)
# Define progress function
progress <- function(n) pb$tick()
opts <- list(progress = progress)

# Names of each covariate
covariate_names <- c("Age","Comorbidity","Sex","n_events","ever_Deprescribed","is_Deprescribed")



##  Parameters for the baseline covariate distribution
M1 <- calc_theory(Dist = "Gaussian", params = c(50, 10))  # Age
M2 <- calc_theory(Dist = "Gaussian", params = c(0, 1))    # Comorbidity
M <- cbind(M1, M2)
marginal <- 0.5
lam <- c()
size <- c()
prob <- 0

# Static correlation matrix
Rey <- diag(1, nrow = 3)
Rey[lower.tri(Rey)] <- 0.2
Rey[upper.tri(Rey)] <- t(Rey)[upper.tri(Rey)]


# Parallel simulation loop for one DGM
simResults<-  foreach(
    sim = 1:nsim,
    .options.snow = opts,
    .options.RNG = seed,
   .packages = c("dplyr", "mstate", "flexsurv", "survival","SimMultiCorrData")
  ) %dorng% {
    
    source("functions.r")
    
# Function for generating patient baseline covariates
invisible(capture.output(init_covariates <- Generate_baseline_covariates(num_patients, Rey, M, marginal, lam, size, prob,round(runif(1)*10000))))

ids <- seq_len(num_patients)

# Create initial covariate dataframe
Patient_pop <- data.frame(
  id = ids,
  init_covariates,
  n_events = 0L,
  ever_Deprescribed = 0L,
  is_Deprescribed = 0L
)

# Convert Sex to binary  (male is zero)
Patient_pop$Sex <- as.integer(Patient_pop$Sex == "female")
#print(Patient_pop[1])

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
compute_cumulative_hazard <- function(hazard_params, clock, covariates_fn,tsince_d) {
# Total hazard function: sums individual cause-specific hazards#
total_hazard <- function(t) {
  t_plus_clock <- t + clock
  covariates_at_t <- covariates_fn(t_plus_clock)
  total_hazard_value <- 0
  covariate_effect <- 0

  for (hazard in hazard_params) {
    beta_effect <- hazard$effects
    gamma_effect <- hazard$time_effects
    
    time_effect_values <- matrix(rep(beta_effect, each = length(t)), ncol = length(beta_effect)) +
                          outer(t_plus_clock, gamma_effect)
    covariate_effect <- rowSums(covariates_at_t * time_effect_values)
    
    if (hazard$type == "exp") {
      total_hazard_value <- total_hazard_value + rep(hazard$rate, length(t))*exp(covariate_effect)
    } else if (hazard$type == "weibull") {
      baseline_hazard <- (hazard$shape / hazard$scale) * (t + tsince_d)^(hazard$shape - 1)
      total_hazard_value <- total_hazard_value + baseline_hazard*exp(covariate_effect)
    } else if (hazard$type == "gompertz") {
      baseline_hazard <- hazard$a * exp(hazard$b * covariates_at_t[,1])
      total_hazard_value <- total_hazard_value + baseline_hazard*exp(covariate_effect)
    }
  }

  return(total_hazard_value)
}
  
  # Cumulative hazard function: integral of total hazard at specific time
  H <- function(t) {
    integrate(total_hazard, lower = 1e-15, upper = t,subdivisions = 100L)$value
  }
  # Sample sojourn time using inversion sampling
  target_H <- -log(runif(1))
  find_T <- Vectorize(function(t) H(t) - target_H)
  #curve(find_T,from=0, to = 100)
  sojourn_time <- uniroot(find_T, interval = c(1e-15, 1000),maxiter = 40,tol = 0.0001)$root
  return(sojourn_time)
}

# Computes the static cause specific hazard at a specific time
get_cause_specific_hazard <- function(state, clock,sojourn_time, hazard_params, covariates_fn) {
  state_hazards <- hazard_params[[state]]
  n_causes <- length(state_hazards)
  covariates_at_t <- covariates_fn(clock)  # Compute once
  effect_function <- function(t) t
  time_effect <- effect_function(clock)
  
  cause_specific_hazards <- numeric(n_causes)
  
  for (i in seq_len(n_causes)) {
    h <- state_hazards[[i]]
    
    beta <- h$effects
    gamma <- h$time_effects
    time_effect_values <- beta + time_effect * gamma
    
    covariate_effect <- exp(sum(covariates_at_t * time_effect_values))  # scalar
    #print(covariate_effect)
    if (h$type == "exp") {
      cause_specific_hazards[i] <- h$rate * covariate_effect
      
    } else if (h$type == "weibull") {
      if (h$scale > 0 && is.finite(h$scale)) {
        #print(h$scale)
        base_haz <- (h$shape / h$scale) * (sojourn_time)^(h$shape - 1)
        cause_specific_hazards[i] <- base_haz * covariate_effect
      }
      
    } else if (h$type == "gompertz") {
      base_haz <- h$a * exp(h$b * covariates_at_t[1])
      cause_specific_hazards[i] <- base_haz * covariate_effect
    }
  }
  
  return(cause_specific_hazards)
}



# Defining continuous time multi-state model function ---------------------------------------------------
simulate_cmc <- function(time, warm_up = 0, hazard_params,baseline_covariates,starting_state) {
  # State space is indexed from 1 
  state_space <- 1:5  
  # Vector to keep track of time spent in each state
  time_spent <- 0
  # The simulation clock
  clock <- 0  
  # Initial state 
  current_state <- starting_state  
  
  names <- c('state', 'time_spent', covariate_names)
  
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
  is_deprescribed_ind = 0
  ever_deprescribed_ind = 0
  deprescribed_time = 0
  flag<-0
  
  # function defining covariates
  covariates_fn <- function(t) {
    # Age
    z1 <- baseline_covariates[1] + t / 12               
    # Comorbidity
    z2 <- baseline_covariates[2] + baseline_covariates[2]*t
    # Sex
    z3 <- rep(baseline_covariates[3], length(t))
    # Number of major adverse events
    z4 <- rep(baseline_covariates[4] + n_events, length(t))
    # Ever deprescribed
    z5 <- rep(baseline_covariates[5] + ever_deprescribed_ind, length(t))
    # Is deprescribed
    z6 <- rep(baseline_covariates[6] + is_deprescribed_ind, length(t))
    
    # Combine into a matrix
    return(cbind(z1, z2, z3, z4, z5, z6))
  }

  
  while (clock < time) {
    # Get hazard parameters for all transitions from the current state
    state_hazards <- hazard_params[[current_state]]
    
   if (deprescribed_time!=clock && current_state==4){
     flag<-1
     #message(sprintf("flag on state/clock/tdep is %s %f %f",current_state,clock,clock-deprescribed_time))
    sojourn_time <- compute_cumulative_hazard(state_hazards, clock, covariates_fn,clock-deprescribed_time)
   }else{sojourn_time <- compute_cumulative_hazard(state_hazards, clock, covariates_fn,0)}
    covs <- matrix(covariates_fn(clock), nrow = 1)
     # Update the simulation clock
    clock <- clock + sojourn_time
    # Update time spent in the current state
    time_spent <- time_spent + sojourn_time
    # Record history (if past warm-up period) and note covariates at the time of exiting previous state (entering current state)
    
    
    history_data[[step]] <- data.frame(
      state = current_state,
      time_spent = sojourn_time,
      covs
    )
    step <- step + 1
    
    # Expand list if necessary
    if (step > length(history_data)) {
      length(history_data) <- length(history_data) * 2
    }
    
    # Determine possible states to transition to
    possible_states <- seq_len(5)
    
    possible_states <- possible_states[possible_states != current_state]
    if (flag==1){
      #message(sprintf("flag on %f",clock-deprescribed_time-sojourn_time))
    cause_specific_hazards <- get_cause_specific_hazard(current_state,clock,clock-deprescribed_time,hazard_params,covariates_fn)
    }else{cause_specific_hazards <- get_cause_specific_hazard(current_state,clock,sojourn_time,hazard_params,covariates_fn)}
    flag<-0
    # Calculate vector of probabilities using relative hazard
    transition_probs = cause_specific_hazards/sum(cause_specific_hazards)
    #print(deprescribed_time-clock)
    # If only one state possible, thats next with probability 1
    if (length(possible_states)==1){
      current_state <- possible_states
      }
    else {
    # Next state is based on a multinomial draw  
    current_state <- sample(possible_states, size = 1, prob = transition_probs)}
    
   # internal covariates
    # If dead, stop simulation
    if (current_state == 5){
      history_data[[step]] <- data.frame(
        state = current_state,
        time_spent = 0,
        covs
      )
      step <- step + 1
      break}
    
    # Counting number of major adverse events
    if (current_state == 3){
      n_events <- n_events + 1
    }
    
    # Tracking deprescribing status
    if (current_state == 4) {
      if (is_deprescribed_ind == 0) {
        deprescribed_time <- clock
        is_deprescribed_ind <- 1
      }
      ever_deprescribed_ind <- 1
    } 
    
    if (current_state == 1){
      is_deprescribed_ind <- 0
      deprescribed_time <- 0}
  }
  history <- do.call(rbind, history_data[1:(step - 1)])
  colnames(history) <- names
  return(list(time_spent = time_spent, history = history))
}



# Define hazard rates for a 5-state system (off-diagonal elements)
hazards <- matrix(c(0.0,  0.01, 0.005, 0.00296, 0.005,              # Healthy - prescribed
                    0.0005,      0,    0.01,     0.00592,    0.005,      # Adverse minor
                    0.0001,   0,  0.0,  0.0296, 0.5,                  # Adverse major
                    0.001,    0.0083,  0.0083,     0.001, 0.005,     # Deprescribed
                    0,      0,    0,     0,   0),                 # Dead
                  nrow = 5, ncol = 5, byrow = TRUE)


# Define hazard parameters for each state
hazard_params <- list(
  list(  # State 1
    list(type = "exp",                # Transition to state 2
         rate = hazards[1,2],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),        
    list(type = "exp",            # Transition to state 3
         shape = 1, 
         rate = hazards[1,3],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "exp",            # Transition to state 4
         rate = hazards[1,4],
         effects = c(0.01,0.02,0,1,-1,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "gompertz",             # Transition to state 5
         a = 3.28E-05,
         b = 9.03E-02,
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0))
  ),
  list(  # State 2
    list(type = "exp",         # Transition to state 1
         rate= hazards[2,1],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),  
    list(type = "exp",             # Transition to state 3
         rate = hazards[2,3],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "exp",         # Transition to state 4
         rate = hazards[2,4],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),  
    list(type = "gompertz",             # Transition to state 5
         a = 3.28E-05,
         b = 9.03E-02,
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0))
  ),
  list(  # State 3
    list(type = "exp",             # Transition to state 1
         rate = hazards[3,1],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "exp",             # Transition to state 2
         rate = hazards[3,2],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "exp",             # Transition to state 4
         rate = hazards[3,4],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "gompertz",             # Transition to state 5
         a = 3.28E-05,
         b = 9.03E-02,
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0))
    
  ),
  list(  # State 4
    list(type = "exp",             # Transition to state 1
         rate = hazards[4,1],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "weibull",            # Transition to state 2
         shape = 1.02,
         scale = 1/hazards[4,2],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "weibull",             # Transition to state 3
         shape = 0.95,
         scale = 1/hazards[4,3],
         effects = c(0,0,1,0.1,0,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "gompertz",             # Transition to state 5
         a = 3.28E-05,
         b = 9.03E-02,
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0))
    
  ),
  list(  # State 5
    list(type = "exp",             # Transition to state 1
         rate = hazards[5,1],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "exp",             # Transition to state 2
         rate = hazards[5,2],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "exp",             # Transition to state 3
         rate = hazards[5,3],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0)),
    list(type = "exp",             # Transition to state 4
         rate = hazards[5,4],
         effects = c(0,0,0,0,0,0),
         time_effects = c(0,0,0,0,0,0))))


Q <- generate_Q_matrix_from_hazards(hazards)

# Parameters: time to simulate, warm-up time
#time unit: months
time <- 1000
warm_up <- 0

# Initialize an empty list to store the full histories of states and times for each patient
history_list <- vector(mode = "list", length = num_patients)
# Initialize an empty vector to store the time spent in all states for each patient
history_time_spent <- vector(mode = "list", length = 5)


# Main simulation loop ----------------------------------------------------

# Single simulation repetition loop
covariate_matrix <- as.matrix(Patient_pop[, 2:7])
results <- lapply(1:num_patients, function(i) {
  baseline_covariates <- covariate_matrix[i, ]
  out <- simulate_cmc(time, warm_up, hazard_params, baseline_covariates,starting_state=1)
  history <- out$history
  censor_time = rexp(1,0.002)
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


# Analysis ----------------------------------------------------------------

# Create a Surv object
#We will set event = 5 for all transitions (you can modify this based on actual exit criteria)
df_history$Event <- ifelse(df_history$State == 3, 1, 0)
df_history$Deprescribed <- ifelse(df_history$State == 4, 1, 0)
df_history$AE <- ifelse(df_history$State == 3, 1, 0)
df_history$Sex <- factor(df_history$Sex, levels = c(0, 1))

# This dataframe is just for plotting
df_history2 <- df_history %>%
  group_by(Patient_ID) %>%
  mutate(Cumulative_Time = cumsum(Time_Spent)) %>%
  mutate(next_time = lag(Cumulative_Time)) %>%  # Get the next time for each state transition
  filter(!is.na(next_time))



 state_names <-c("Prescribed", "Adverse event-minor",  "Adverse event-major", "Deprescribed", "Dead")
 tmat <- transMat(x = list(c(2, 3, 4, 5),c(1,3, 4, 5), c(1, 4, 5), c(1,2,3,5), c()),
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

# # Ensure the data is ordered correctly
df_history <- df_history %>%
  group_by(Patient_ID) %>%
  mutate(time_to_death = cumsum(Time_Spent)) %>%
  ungroup()


ms_history <- df_history %>%
  filter(Time_Spent != 0) %>%
  group_by(Patient_ID) %>%
  mutate(
    from = State,
    to = lead(State, default = as.factor(5)),
    TStart = lag(cumsum(Time_Spent), default = 0),
    TStop = cumsum(Time_Spent),
    status = lead(Event,default = 0)
  ) %>%
  left_join(tmat_df, by = c("from", "to")) %>%
  ungroup()

class(ms_history) <- c("msdata", class(ms_history))
attr(ms_history, "trans") <- tmat



#M<-ms_history

ms_history <- ms_history %>%
  arrange(Patient_ID, TStart) %>%
  group_by(Patient_ID) %>%
  mutate(
    origin = {
      result <- numeric(n())
      current_origin <- 0
      reset_needed <- FALSE
      
      for (i in seq_along(from)) {
        if (!is.na(from[i]) && from[i] == 4 && (current_origin)==0) {
          current_origin <- TStart[i]
        }
        
        if (!is.na(to[i]) && from[i] == 1) {
          reset_needed <- TRUE
        }
        
        if (reset_needed && from[i] == 4 && TStart[i] > current_origin) {
          current_origin <- TStart[i]
          reset_needed <- FALSE
        }
        
        result[i] <- current_origin
      }
      
      result
    }
  ) %>%
  ungroup()%>%
  #filter(from == 4 | from == 1)%>%
  #filter(from == 4)%>%
  mutate(TStart = TStart-origin,
         TStop = TStop-origin)


tryCatch({
  # weibull_model_flex <- flexsurvreg(Surv(Time_Spent_in4, Event) ~ 1,
  #                                   data = df_inD,
  #                                   dist = "weibull")
  weibull_model_flex <- flexsurvreg(Surv(TStart, TStop, status) ~ Sex + n_events,
                                    data = ms_history,
                                    dist = "weibullPH")
  
  
  
  # If successful, store parameters
  scale_param <- exp(weibull_model_flex$coefficients[2])
  shape_param <- exp(weibull_model_flex$coefficients["shape"])
  
  
  # This is the true AG model (in calendar time since treatment initiation) for effect of deprescribing on AE major
  fit_cox <- coxph(Surv(TStart, TStop, status)~  is_Deprescribed + tt(is_Deprescribed),
                   data = ms_history,id=Patient_ID, tt = function(is_Deprescribed,t,...) is_Deprescribed*log(t))
  AG_scale_param<-exp(coef(fit_cox)[1])/(coef(fit_cox)[2]+1)
  AG_shape_param<-coef(fit_cox)[2]+1
  
}, error = function(e) {
  message(sprintf("Skipping sim %d due to error: %s", sim, e$message))
  scale_param <- NA  # optional: mark skipped iterations
  shape_param <- NA
  AG_scale_param <-NA
  AG_shape_param <- NA
})

ms_history



  }

stopCluster(cl)

return(simResults)

}





#data<-generate_simulated_data()

end_time <- Sys.time()

print(end_time - start_time)

