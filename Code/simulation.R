# Author: Ryan Muddiman
# Date of initial development (YYYY-MM-DD): 2024-10-22
# Purpose: This script is the main simulation for generating a large synthetic
# population health dataset, including health records,
# with time-varying confounding.

# References: Link to relevant documentation


# Load the required packages
library(usethis)
library("tidyverse")
library(ggplot2)
library(survival)
library(survminer)
library(dplyr)
library(patchwork)
#library('plot.matrix')
library("lattice")                       
library("ggplotify")    

# Initialization of simulation parameters ---------------------------------
num_patients = 2

# number of continuous random covariates
n_continuous_covariates = 1

# number of binary random covariates
n_binary_covariates = 0

# Names of each covariate, continuous first then binary
covariate_names <- c("Age","Sex")



nsim = 1
betaHat <- rep(NA, nsim)

for (sim in 1:nsim){
  #print(sim)

# Function for generating patient baseline covariates
generate_random_covariates <- function(covariate_names) {

  covariate_value<- c(runif(1,18,60),
                      rbinom(1,1,0.5))
  # transpose the dataframe
  covariates = t(data.frame(covariate_value))
  # add column names to dataframe
  colnames(covariates) <- covariate_names
  return(covariates)
}

# Constructor for patient observation using 'Patient' class
new_Patient <- function(id) {
  covariates <- generate_random_covariates(covariate_names)  # Generate random covariates
  patient <- structure(list(id = id, init_covariates = covariates), class = "Patient")
  return(patient)
}

# Patient population generation
Patient_pop <- list()
for (x in 1:num_patients) {
  Patient_pop[[x]] <- new_Patient(x)  # Assign a unique ID to each patient
}



#print(Patient_pop[1])

#Unlisting the patient population to a DF for plotting
df_patient_covariates <- data.frame(matrix(unlist(Patient_pop), nrow=length(Patient_pop), byrow=TRUE))
colnames(df_patient_covariates) <- c('id',covariate_names)


# Function to compute cumulative hazard and sample sojourn time
compute_cumulative_hazard <- function(hazard_params,  covariates_fn) {
  # Define the total hazard function that sums individual cause-specific hazards
  total_hazard <- function(t) {
    total_hazard_value <- 0
    #total_hazard_value <- rep(0, length(t)) # new
    # Loop over the hazard parameters to sum the individual hazards
    for (i in 1:length(hazard_params)) {
      hazard <- hazard_params[[i]]  # Access the i-th cause-specific hazard
      hazard_type <- hazard$type  # Extract hazard type for the transition
      covariates_at_t <- covariates_fn(t)  # Get the covariates for this time
      #print(dim(covariates_at_t))
      effect_function <- function(t) { (t) }  # linear time effect
      
      beta_effect <- hazard$effects  # Baseline effect
      gamma_effect <- hazard$time_effects
      
      #print(beta_effect)
      
      time_effect_values <- t((beta_effect) + t(outer(effect_function(t),gamma_effect)))
      #print((time_effect_values))
      # unexponentiated covariate value (vector of t)
      covariate_effect <- rowSums(covariates_at_t * time_effect_values)
      #print((covariate_effect))
      #print(length(covariate_effect))
      if (hazard_type == "exp") {
        # Exponential hazard function: constant rate
        total_hazard_value <- total_hazard_value + rep(hazard$rate, length(t))
        #print(total_hazard_value)
      } 
      else{
        # Weibull hazard function: (shape / scale) * (t / scale)^(shape - 1)
        if (hazard$scale > 0) {
          baseline_hazard<-(hazard$shape / hazard$scale) * (t / hazard$scale)^(hazard$shape - 1)
          total_hazard_value <- total_hazard_value + baseline_hazard
        }else{total_hazard_value<- total_hazard_value}
        # No need to add 0 explicitly, just skip if scale == 0
      }
    } 
    return(total_hazard_value*exp(covariate_effect))
  }
  
  # Cumulative hazard function is the integral of the total hazard
  H <- function(t) {
    # Integrate total hazard to get cumulative hazard
    result <- integrate(total_hazard, lower = 0, upper = t,subdivisions=2000)
    return(result$value)
  }
  # Sample sojourn time using inversion sampling
  u <- runif(1)  # Uniform random sample [0, 1]
  target_H <- -log(u)  # Target cumulative hazard (using inversion sampling)
  
  # Solve for T such that H(T) = target_H using root-finding (uniroot)
  find_T <- function(t) H(t) - target_H
  sojourn_time <- uniroot(find_T, interval = c(0, 100),extendInt ="upX")$root
  return(sojourn_time)  # Return the sampled sojourn time
}


# Function to compute the all-cause hazard for a given state
total_hazard <- function(hazard_params, time) {
  hazard_sum <- 0
  for (h in hazard_params) {
    if (h$type == "exp") {
      hazard_sum <- hazard_sum + h$rate  # Exponential hazard rate (constant)
    } else if (h$type == "weibull") {
      # Weibull hazard rate, depends on time
      if (h$scale>0){
      hazard_sum <- hazard_sum + (h$shape / h$scale) * (time / h$scale)^(h$shape - 1)}
      else{
        hazard_sum <- hazard_sum+0
      }
      #print(hazard_sum)
    }
  }
  return(hazard_sum)
}


# Function to get the cause-specific hazard for a given state at a given time
get_cause_specific_hazard <- function(state, time, hazard_params, covariates_fn) {
  # Extract the list of transitions for the given state
  state_hazards <- hazard_params[[state]]
  
  # Initialize an empty vector to store cause-specific hazard values
  cause_specific_hazards <- numeric(length(state_hazards))
  
  # Loop over each cause (transition) for the state and calculate the hazard
  for (i in 1:length(state_hazards)) {
    h <- state_hazards[[i]]
    covariates_at_t <- covariates_fn(time)  # Get the covariates for this time
    #print(covariates_at_t)
    
    # Define effect function (linear time effect as an example)
    effect_function <- function(t) { (t) }  # This could be extended to other time-varying effects (e.g., log(t))
    
    # Extract the baseline effect (beta) and time-varying effect (gamma)
    beta_effect <- h$effects  # Baseline effects (coefficients)
    gamma_effect <- h$time_effects  # Time-varying effects (coefficients)
    
    # Calculate the time-varying effect: beta + gamma * f(t)
    time_effect_values <- beta_effect + effect_function(time)*gamma_effect
    
    # Unexponentiated covariate effect (the product of covariates and time-varying effect values)
    covariate_effect <- exp(sum(covariates_at_t * time_effect_values))
    
    # Calculate the cause-specific hazard based on hazard type
    if (h$type == "exp") {
      # For exponential hazard, the hazard is constant over time
      cause_specific_hazards[i] <- h$rate * covariate_effect
    } else if (h$type == "weibull") {
      # For Weibull, the hazard depends on time
      if (h$scale > 0) {
        baseline_hazard <- (h$shape / h$scale) * (time / h$scale)^(h$shape - 1)
        cause_specific_hazards[i] <- baseline_hazard * covariate_effect
      } else {
        cause_specific_hazards[i] <- 0  # No hazard if scale is zero
      }
    }
  }
  # Return the vector of cause-specific hazard values
  return(cause_specific_hazards)
}




# Defining CMC function ---------------------------------------------------
simulate_cmc <- function(time, warm_up = 0, hazard_params,baseline_covariates) {
  # State space is indexed from 1 
  state_space <- 1:5  
  # Vector to keep track of time spent in each state
  time_spent <- rep(0, length(state_space))  
  # The simulation clock
  clock <- 0  
  # Initial state (1-based indexing in R)
  current_state <- 1  
  history <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(history) <- c('state', 'time_spent')
  
  while (clock < time) {
    # Get hazard parameters for all transitions from the current state
    state_hazards <- hazard_params[[current_state]]
    
    # A deterministic function of time for simulating covariate evolution
    covariates_fn <- function(t) {
      fn <- Vectorize(function(t) {
        z1 <- baseline_covariates[1] + t/12  # age is in years but sim-time is months
        z2 <- baseline_covariates[2]
        c(z1, z2) # Return a vector for each t
      }, vectorize.args = "t")
      
      result <- fn(t)
      if (is.vector(t)) {
        # Transpose for vector input
        return(t(result)) 
      }
      # No transpose for scalar input
      return(result) 
    }
    
    # Sample the sojourn time using the cumulative hazard
    sojourn_time <- compute_cumulative_hazard(state_hazards, covariates_fn)
    # Update time spent in the current state
    time_spent[current_state] <- time_spent[current_state] + sojourn_time
    
    # Record history (if past warm-up period)
    if (clock >= warm_up) {
      history <- rbind(history, data.frame(state = current_state, time_spent = sojourn_time,covs = covariates_fn(clock)))
      #history <- cbind(history, covs = covariates_fn(time))
      #print(history)
    }
    # Update the simulation clock
    clock <- clock + sojourn_time
    # Determine possible states to transition to
    possible_states <- which(Q[current_state, ] != 0)
    possible_states <- possible_states[possible_states != current_state]
    #message('possible states:',possible_states)
    total_hazard_current_state <- total_hazard(state_hazards, clock)
    # Compute the cause-specific hazards for each possible state
    cause_specific_hazards <- get_cause_specific_hazard(current_state,time,hazard_params,covariates_fn)

    # Calculate vector of probabilities using relative hazard
    transition_probs = cause_specific_hazards/sum(cause_specific_hazards)
    
    # If only one state possible, thats next with probability 1
    if (length(possible_states)==1){
      current_state <- possible_states
    }else {
    # Next state is based on a multinomial draw  
    current_state <- sample(possible_states, size = 1, prob = transition_probs)}

    # If dead, stop simulation
    if (current_state == 5){
      history[nrow(history) + 1,] = c(current_state, 0, covariates_fn(clock))
      break}
  }
  return(list(time_spent = time_spent, history = history))
}


## Function to generate a Q-matrix from hazard rates
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

# Define hazard rates for a 4-state system (off-diagonal elements)
hazards <- matrix(c(0,  0.1, 0.01, 0.001, 0.1,   # Healthy - prescribed
                    0.1,  0.0, 0.1,   0, 0,     # Adverse minor
                    0,    0,   0,   1, 0.1,     # Adverse major
                    0,    0,   0,   0, 0.1,     # Deprescribed
                    0,    0,   0,   0, 0),      # Dead
                  nrow = 5, ncol = 5, byrow = TRUE)

# Define hazard parameters for each state
hazard_params <- list(
  list(  # State 1
    list(type = "exp",                # Transition to state 2
         rate = hazards[1,2],
         effects = c(0,0),
         time_effects = c(0,0)),        
    list(type = "weibull",            # Transition to state 3
         shape = 1, 
         scale = 1/hazards[1,3],
         effects = c(0,0),
         time_effects = c(0,0)),
    list(type = "weibull",            # Transition to state 4
         shape = 1, 
         scale = 1/hazards[1,4],
         effects = c(0,0),
         time_effects = c(0,0)),
    list(type = "weibull",            # Transition to state 5
         shape = 1, 
         scale = 1/hazards[1,5],
         effects = c(0,0.3),
         time_effects = c(0,0))
  ),
  list(  # State 2
    list(type = "weibull",         # Transition to state 1
         shape = 1,
         scale = 1/hazards[2,1],
         effects = c(0.0, 0),
         time_effects = c(0,0)),  
    list(type = "exp",             # Transition to state 3
         rate = hazards[2,3],
         effects = c(0,0),
         time_effects = c(0,0))      
  ),
  list(  # State 3
    list(type = "exp",             # Transition to state 4
         rate = hazards[3,4],
         effects = c(0,0),
         time_effects = c(0,0)),
    list(type = "exp",             # Transition to state 5
              rate = hazards[3,5],
              effects = c(0,0),
              time_effects = c(0,0))  
  ),
  list(  # State 4
    list(type = "exp",             # Transition to state 1
         rate = hazards[4,5],
         effects = c(0,0),
         time_effects = c(0,0))
))


# Generate the Q-matrix based on the hazard rates
Q <- generate_Q_matrix_from_hazards(hazards)


# Parameters: time to simulate, warm-up time
#time unit: months
time <- 100
warm_up <- 0



# Initialize an empty list to store the full histories of states and times for each patient
history_list <- list()
# Initialize an empty vector to store the time spent in all states for each patient
history_time_spent <- list()

# Main simulation loop ----------------------------------------------------

# Simulating histories for many patients
for (i in 1:num_patients) {
  # get initial covariates
  baseline_covariates <- unlist(Patient_pop[[i]]$init_covariates)
  #print(baseline_covariates)
  out <- simulate_cmc(time, warm_up, hazard_params, baseline_covariates)
  history <- out$history  
  history <- rbind(c(1,0), history)
  # Add a Patient_ID column to the history to identify the patient for each state-time pair
  # Replicate the patient ID for each row in their history
  patient_id <- rep(i, nrow(history))  
  # Combine Patient_ID with history data
  history_with_id <- cbind(Patient_ID = patient_id, history)  
  #covariate_extend<-matrix(rep(baseline_covariates, times = nrow(history)), nrow = nrow(history), byrow = TRUE)
  #history_with_id <- cbind(history_with_id,covariate_extend)
  # Store the entire history for patient i (with Patient_ID)
  history_list[[i]] <- history_with_id
  # Store time spent in all states for patient i
  # Assuming the second column of history contains the time spent in each state
  history_time_spent[[i]] <- history[, 2]  # Extracting the time spent in each state for patient i
}
# Flatten the list into a single data frame
flat_history <- do.call(rbind, history_list)
# Assign column names (adjust according to the structure of your `history`)
colnames(flat_history) <- c("Patient_ID", "State", "Time_Spent",(covariate_names))
# Convert the flattened history into a data frame
df_history <- as.data.frame(flat_history)



# Convert Patient_ID and State to factors
df_history$Patient_ID <- factor(df_history$Patient_ID)
df_history$State <- factor(df_history$State)

# Calculate the mean time spent in State 1 for each patient
mean_time_in_state_1 <- df_history %>%
  filter(State == 1) %>%
  group_by(Patient_ID) %>%
  slice(-1) %>%     # Filter for rows where State is 1           
  summarise(total_time_in_state_1 = mean(Time_Spent, na.rm = TRUE)) # Calculate the mean

# Print the result
#print('mean sojourn empirically calculated:')
#print(mean(mean_time_in_state_1$total_time_in_state_1))
#print('mean sojourn predicted from Q:')
#print(-1/Q[1,1])



# Analysis ----------------------------------------------------------------

# Create a Surv object
#We will set event = 5 for all transitions (you can modify this based on actual exit criteria)
df_history$Event <- ifelse(df_history$State == 5, 1, 0)


df_history <- df_history %>%
  group_by(Patient_ID) %>%
  mutate(time_to_death = sum(Time_Spent))%>%
  ungroup()
#print(df_history,n=30)

df_history$Sex <- factor(df_history$Sex, levels = c(0, 1))

survival_data <- df_history %>%
  group_by(Patient_ID) %>%
  summarize(
    Total_Time = sum(Time_Spent),
    Event = max(Event)  # Assumes Event = 1 if patient experienced the event
  ) %>%
  ungroup()
# Create a survival object
surv_obj <- Surv(time = survival_data$Total_Time, event = survival_data$Event)
# Inspect the survival object
surv_obj
fit <- survfit(surv_obj ~ 1, data = survival_data)
# Print the survival curve summary
#print(fit,rmean= 2000)
# Plot the Kaplan-Meier curves by Group
ggsurv <- ggsurvplot(fit,
                     risk.table = F,
                     xlab = "Time (Months)", 
                     ylab = "Survival",
                     conf.int = F,
                     palette = "lancet")



df_history2 <- df_history %>%
  group_by(Patient_ID) %>%
  mutate(Cumulative_Time = cumsum(Time_Spent)) %>%
  mutate(next_time = lag(Cumulative_Time)) %>%  # Get the next time for each state transition
  filter(!is.na(next_time))



# Plot individual state trajectories for each patient
state_labels <- c("1" = "Prescribed", "2" = "Adverse event", "3" = "Dead")

h<-ggplot(df_history2, aes(x = Cumulative_Time, y = State, group = Patient_ID, color = Patient_ID)) +
  geom_step(direction = "vh",linewidth = 1.2, alpha = 0.5) +  # Use a step plot to show state changes over time
  labs(title = "State Transitions for Each Patient",
       x = "Time (Months)",
       y = "State",
       color = "Patient") +
  scale_y_discrete(labels = state_labels)+
  theme_minimal() +
  theme(legend.position = "right") +
  theme(
    panel.border = element_rect(colour = "black", fill=NA, size=1)  # Add a black border around the plot
  )#
#print(h)

#ggsave(
#  "ggtest.png",
#  width = 4.25,
#  height = 3.25,
#  dpi = 300
#)




df_death <- df_history2 %>% filter(Event == 1)  # Keep only death events
fit_cox <- coxph(Surv(time_to_death, Event) ~ Sex, data=df_death)
betaHat[sim]<- fit_cox$coef
}
#hist(betaHat, main = "Distribution of Estimated Beta", xlab = "Estimated Beta", breaks = 30)
#print("bias in estimate:")
#print(mean(betaHat)-0.3)

# Plot with State 3 handled as a separate entry under the same 'State' legend
h <- ggplot(df_history2, aes(x = Cumulative_Time, xend = next_time, y = Patient_ID, yend = Patient_ID, color = State)) +
  # Plot colored segments for states where Time_Spent > 0 (excluding State 3)
  geom_segment(data = filter(df_history2, State != 5 & Time_Spent > 0), size = 2.5) +  
  # Add symbol for State 3 when Time_Spent = 0 (plotting State 3 as a triangle symbol)
  geom_point(data = filter(df_history2, State == 5 & Time_Spent == 0),  
             aes(x = Cumulative_Time, y = Patient_ID, shape = as.factor(State)),
             size = 2, color = "black") +
  
  geom_point(data = filter(df_history2, State == 5 & Time_Spent == 0),  
             aes(x = Cumulative_Time, y = Patient_ID, shape = as.factor(State)),
             size = 2, color = "cyan")+
  
  
  
   #geom_vline(xintercept=20)+
  # Set color for State 1 and State 2
  scale_color_manual(values = c("red", "green","blue","orange"), 
                     breaks = c("1", "2","3","4"),  # Explicitly define which states should appear in the color legend
                     labels = c("Prescribed", "Adverse event-minor","Adverse event-major","Deprescribed")) +  # Labels for the color legend
  # Set shape for State 3 (triangle shape 17) and include in the legend
  scale_shape_manual(values = c("5" = 16),labels='dead') +  # Assign shape 17 (triangle) for State 3
  labs(title = "Population State Transitions Over Time",
       x = "Time (months)",
       y = "Patient ID") +  # Removed shape legend title
  # Combine color and shape legends under a single title "State"
  guides(  # Title for the color legend (States 1 and 2)
    shape = guide_legend(title = ""),color = guide_legend(nrow = 2, byrow = TRUE)   # Title for the shape legend (State 3)
  ) +
  theme_minimal() +
  theme(legend.position = "top")



par(mfrow = c(1, 2))
par(mar=c(4.1, 4.1, 4.1, 4.1)) # adapt margins
#class(Q)
#k<-plot(Q,breaks=10,digits=4,asp=TRUE,col=viridis)

q_df <-Q
rownames(q_df) <- c("Prescribed",
                    "Adverse event - minor",
                    "Adverse event - major",
                    "Deprescribed",
                    "Dead")
library(gplots) # not to be confused with `ggplot2`, which is a very different package
color_palette <- bluered(100)
max_abs <- max(abs(q_df))
brk <- do.breaks(c(-max_abs, max_abs), 15)
q_df<-q_df[nrow(q_df):1, ]
my_lattice<-levelplot(t(q_df),col.regions = color_palette,at = brk,
                      colorkey = list(col = color_palette, 
                                      at = brk))
all_plots<-h#+ggsurv$plot
#print(all_plots)
#p<-as.ggplot(h)+
p<-  as.ggplot(my_lattice)
  #ggsurv$plot
#print(p)



