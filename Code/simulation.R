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
library(msm)
library(survival)
library(survminer)
library(dplyr)
library(patchwork)
# Initialization of simulation parameters ---------------------------------
num_patients = 30

# number of continuous random covariates
n_continuous_covariates = 2

# number of binary random covariates
n_binary_covariates = 0

# Names of each covariate, continuous first then binary
covariate_names <- c("Age", "deprescribing")

# Function for generating patient baseline covariates
generate_random_covariates <- function(covariate_names) {
  # Example of probabilistic mapping using a logistic function
  map_to_binary_probabilistic <- function(covariate_value, beta_0 = 0, beta_1 = 1) {
    # Logistic function for probability of binary outcome = 1
    prob <- 1 / (1 + exp(-(beta_0 + beta_1 * covariate_value)))
    prob<-0.5
    # Draw a random number between 0 and 1, and map to 0 or 1
    return(rbinom(1, 1, prob))  # rbinom draws from a Bernoulli distribution
  }
  covariate_value<- c(
    runif(1, 18, 35),
    sapply(runif(1, 0, 1), map_to_binary_probabilistic))
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
start.time <- Sys.time()
Patient_pop <- list()
for (x in 1:num_patients) {
  Patient_pop[[x]] <- new_Patient(x)  # Assign a unique ID to each patient
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


#print(Patient_pop[1])

#Unlisting the patient population to a DF for plotting
df_patient_covariates <- data.frame(matrix(unlist(Patient_pop), nrow=length(Patient_pop), byrow=TRUE))
colnames(df_patient_covariates) <- c('id',covariate_names)



#generate_event_history(Patient_pop[1])
#
#for (x in 1:num_patients) {
#  Patient_pop[[x]] <- generate_event_history(Patient_pop[x])  
#}


sample_from_rate <- function(rate) {
  if (rate == 0) {
    return(Inf)# Return Inf if rate is 0 (no transition)
  }
  if (rate > 0) {
    return(rexp(1, rate))# Sample from an exponential distribution for positive rates
  }
  # If the rate is negative, we return Inf 
  # because we're not "sampling" from a negative rate in this context.
  return(Inf)
}

# Function to compute cumulative hazard and sample sojourn time
compute_cumulative_hazard <- function(hazard_params,  covariates_fn) {
  # Define the total hazard function that sums individual cause-specific hazards
  total_hazard <- function(t) {
    total_hazard_value <- 0
    total_hazard_value <- rep(0, length(t)) # new
    # Loop over the hazard parameters to sum the individual hazards
    for (i in 1:length(hazard_params)) {
      #message('i is:',i)
      hazard <- hazard_params[[i]]  # Access the i-th hazard transition
      hazard_type <- hazard$type  # Extract hazard type for the transition
      #print(hazard_type)
      
      covariates_at_t <- covariates_fn(t)  # Get the covariates for this time
      
      # Calculate covariate effect: exp(covariates * coefficients)
      covariate_effect <- exp(sum(covariates_at_t * hazard$effects))  # Apply covariate effect
      
      if (hazard_type == "exp") {
        #print('hazard is exp')
        # Exponential hazard function: constant rate
        total_hazard_value <- total_hazard_value + rep(hazard$rate*covariate_effect, length(t))
        #print(total_hazard_value)
      } 
      else{
        #print('hazard is weibull')
        # Weibull hazard function: (shape / scale) * (t / scale)^(shape - 1)
        if (hazard$scale > 0) {
          #print('doing this')
          baseline_hazard<-(hazard$shape / hazard$scale) * (t / hazard$scale)^(hazard$shape - 1)
          total_hazard_value <- total_hazard_value + baseline_hazard*covariate_effect
            
        }else{total_hazard_value<- total_hazard_value}
        # No need to add 0 explicitly, just skip if scale == 0
      }
    } 
    #print(total_hazard_value)
    return(total_hazard_value)
  }
  
  #print(total_hazard(100))
  # Cumulative hazard function is the integral of the total hazard
  H <- function(t) {
    # Integrate total hazard to get cumulative hazard
    result <- integrate(total_hazard, lower = 0, upper = t)
    return(result$value)
  }
  
  # Sample sojourn time using inversion sampling
  u <- runif(1)  # Uniform random sample [0, 1]
  target_H <- -log(u)  # Target cumulative hazard (using inversion sampling)
  
  # Solve for T such that H(T) = target_H using root-finding (uniroot)
  find_T <- function(t) H(t) - target_H
  sojourn_time <- uniroot(find_T, interval = c(0, 100))$root
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
get_cause_specific_hazard <- function(state, time, hazard_params) {
  # Extract the list of transitions for the given state
  state_hazards <- hazard_params[[state]]
  # Initialize an empty vector to store cause-specific hazard values
  cause_specific_hazards <- numeric(length(state_hazards))
  # Loop over each cause (transition) for the state and calculate the hazard
  for (i in 1:length(state_hazards)) {
    h <- state_hazards[[i]]
    if (h$type == "exp") {
      # For exponential, the hazard is constant over time
      cause_specific_hazards[i] <- h$rate
    } else if (h$type == "weibull") {
      # For Weibull, the hazard depends on time
      if (h$scale>0){
      cause_specific_hazards[i] <- (h$shape / h$scale) * (time / h$scale)^(h$shape - 1)}
      else {cause_specific_hazards[i] <- 0 }
    }
  }
  return(cause_specific_hazards)
}




simulate_cmc <- function(time, warm_up = 0, hazard_params,covariates) {
  state_space <- 1:3  # State space is indexed from 1 to 3
  time_spent <- rep(0, length(state_space))  # Vector to keep track of time spent in each state
  clock <- 0  # The simulation clock
  current_state <- 1  # Initial state (1-based indexing in R)
  history <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(history) <- c('state', 'time_spent')
  
  while (clock < time) {
    # Get hazard parameters for all transitions from the current state
    state_hazards <- hazard_params[[current_state]]
    # Sample the sojourn time using the cumulative hazard
    
    # Define a simple time-dependent covariate function
    
    covariates_fn <- function(t) {
      z1 <- 0*covariates[1] + 0.0 * t  # e.g., Treatment effect increases with time
      z2 <- covariates[2]       # e.g., Deprescribed
      return(c(z1, z2))    # Return a vector of covariates at time t
    }
    
    sojourn_time <- compute_cumulative_hazard(state_hazards, covariates_fn)
    #print(state_hazards)
    
    # Update time spent in the current state
    time_spent[current_state] <- time_spent[current_state] + sojourn_time
    
    # Record history (if past warm-up period)
    if (clock >= warm_up) {
      history <- rbind(history, data.frame(state = current_state, time_spent = sojourn_time))
    }
    # Update the simulation clock
    clock <- clock + sojourn_time
    #print(clock)
    # Determine possible states to transition to
    possible_states <- which(Q[current_state, ] != 0)
    possible_states <- possible_states[possible_states != current_state]
    
    total_hazard_current_state <- total_hazard(state_hazards, clock)
    
    # Compute the cause-specific hazards for each possible state
    cause_specific_hazards <- get_cause_specific_hazard(current_state,time,hazard_params)
    #print(possible_states)
    transition_probs = cause_specific_hazards/sum(cause_specific_hazards)
    #message('tprobs:',transition_probs)
    #message('possible states:',possible_states)
    
    if (length(possible_states)==1){
      current_state <- possible_states
    }else {
    current_state <- sample(possible_states, size = 1, prob = transition_probs)}
    #message('current state:', current_state)
    if (current_state == 3){
      history[nrow(history) + 1,] = c(current_state, 0)
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
hazards <- matrix(c(0,  0.1, 0.1,  # Healthy
                    1,  0.0,   0.1,  # Adverse
                    0.0,   0.00,     0), # Death
                  nrow = 3, ncol = 3, byrow = TRUE)

# Define hazard parameters for each state
hazard_params <- list(
  list(  # State 1
    list(type = "exp",                # Transition to state 2
         rate = hazards[1,2],
         effects = c(0, 0)),        
    list(type = "weibull",            # Transition to state 3
         shape = 1, 
         scale = 1/hazards[1,3],
         effects = c(0, 0))
  ),
  list(  # State 2
    list(type = "weibull",         # Transition to state 1
         shape = 1,
         scale = 1/hazards[2,1],
         effects = c(0.0, 0)),  
    list(type = "exp",             # Transition to state 3
         rate = hazards[2,3],
         effects = c(0, 0))      
  ),
  list(  # State 3
    list(type = "exp",             # Transition to state 1
         rate = hazards[3,1],
         effects = c(0, 0)),  
    list(type = "weibull",         # Transition to state 2
         shape = 1,
         scale = 1/hazards[3,2],
         effects = c(0, 0))  
  )
)


# Generate the Q-matrix based on the hazard rates
Q <- generate_Q_matrix_from_hazards(hazards)


# Parameters: time to simulate, warm-up time
#time unit: months
time <- 100
warm_up <- 0

# Run the simulation
#out <- simulate_cmc(Q, time, warm_up,hazard_params)
#steady_state_probabilities <-out$pi
#history <- out$history
# Print the result
#print((steady_state_probabilities))


# Initialize an empty list to store the full histories of states and times for each patient
history_list <- list()

# Initialize an empty vector to store the time spent in all states for each patient
history_time_spent <- list()

# Simulating histories for 100 patients
for (i in 1:num_patients) {
  # Simulate the patient history (assuming simulate_cmc returns an object with a $history attribute)
  covariates <- unlist(Patient_pop[[i]]$init_covariates)[1:2]
  #print(covariates)
  out <- simulate_cmc(time, warm_up,hazard_params,covariates)
  history <- out$history  # Assuming history is a data frame or matrix
  history <- rbind(c(1,0),history)
  #print(history)
  # Add a Patient_ID column to the history to identify the patient for each state-time pair
  patient_id <- rep(i, nrow(history))  # Replicate the patient ID for each row in their history
  history_with_id <- cbind(Patient_ID = patient_id, history)  # Combine Patient_ID with history data
  covariate_extend<-matrix(rep(covariates, times = nrow(history)), nrow = nrow(history), byrow = TRUE)
  #print(covariate_extend)
  history_with_id <- cbind(history_with_id,covariate_extend)
  #print(history_with_id)
  # Store the entire history for patient i (with Patient_ID)
  history_list[[i]] <- history_with_id
  
  # Store time spent in all states for patient i
  # Assuming the second column of history contains the time spent in each state
  history_time_spent[[i]] <- history[, 2]  # Extracting the time spent in each state for patient i
}

# Flatten the list into a single data frame
flat_history <- do.call(rbind, history_list)

# Assign column names (adjust according to the structure of your `history`)
colnames(flat_history) <- c("Patient_ID", "State", "Time_Spent","Age","Deprescribed")

# Convert the flattened history into a data frame
df_history <- as.data.frame(flat_history)

# Inspect the first few rows of the flattened data frame
#print(head(df_history))

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

# Assuming df_history is the data frame that contains the simulation history with Patient_ID, State, and Time_Spent


hazards_123 <- hazards[-4, -4]
# Adjust diagonal elements to ensure row sums are zero (Hazard matrix definition)
for(i in 1:nrow(hazards_123)) {
  hazards_123[i, i] <- -sum(hazards_123[i, -i])
}
# Solve the system pi Q = 0
library(MASS)  # For ginv (generalized inverse)
null_space_123 <- ginv(t(hazards_123))  # Generalized inverse to find the null space
# Find steady-state probabilities (null space corresponds to pi Q = 0)
steady_state_123 <- null_space_123[, 1]  # First column of the null space
steady_state_123 <- steady_state_123 / sum(steady_state_123)  # Normalize so that the sum is 1
# Output the steady-state distribution for states 1, 2, and 3
#print(steady_state_123)



# Create a Surv object
#We will set event = 1 for all transitions (you can modify this based on actual exit criteria)
df_history$Event <- ifelse(df_history$State == 3, 1, 0)

df_history <- df_history %>%
  group_by(Patient_ID) %>%
  mutate(Group = if_else(Deprescribed > 0, 1, 0)) %>%
  ungroup()
df_history$Group <- factor(df_history$Group, levels = c(0, 1), labels = c("Continuing", "Deprescribed"))


df_history <- df_history %>%
  group_by(Patient_ID) %>%
  mutate(time_to_death = sum(Time_Spent))%>%
  ungroup()
print(df_history,n=30)

#df_history$Group <- factor(df_history$Group, levels = c(0, 1), labels = c("No State 3", "State 3 Entered"))

survival_data <- df_history %>%
  group_by(Patient_ID, Group, Age, Deprescribed) %>%
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
print(fit,rmean= 2000)
par(mfrow = c(1, 2))
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
  ungroup()

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

data_sorted <- df_history2 %>%
  group_by(Patient_ID) %>% # Ensure the states are in the correct order
  mutate(cumulative_time = cumsum(Time_Spent),  # Add cumulative time for each patient
         next_time = lag(cumulative_time)) %>%  # Get the next time for each state transition
  filter(!is.na(next_time))  # Remove the last row for each patient, as it has no next time


# Plot with State 3 handled as a separate entry under the same 'State' legend
h <- ggplot(data_sorted, aes(x = cumulative_time, xend = next_time, y = Patient_ID, yend = Patient_ID, color = State)) +
  # Plot colored segments for states where Time_Spent > 0 (excluding State 3)
  geom_segment(data = filter(data_sorted, State != 3 & Time_Spent > 0), size = 2.5) +  
  # Add symbol for State 3 when Time_Spent = 0 (plotting State 3 as a triangle symbol)
  geom_point(data = filter(data_sorted, State == 3 & Time_Spent == 0),  
             aes(x = cumulative_time, y = Patient_ID, shape = as.factor(State)),
             size = 2, color = "black") +
  # Set color for State 1 and State 2
  scale_color_manual(values = c("red", "green"), 
                     breaks = c("1", "2"),  # Explicitly define which states should appear in the color legend
                     labels = c("Prescribed", "Adverse event")) +  # Labels for the color legend
  # Set shape for State 3 (triangle shape 17) and include in the legend
  scale_shape_manual(values = c("3" = 16),labels='dead') +  # Assign shape 17 (triangle) for State 3
  labs(title = "Population State Transitions Over Time",
       x = "Time (months)",
       y = "Patient ID") +  # Removed shape legend title
  # Combine color and shape legends under a single title "State"
  guides(  # Title for the color legend (States 1 and 2)
    shape = guide_legend(title = "")   # Title for the shape legend (State 3)
  ) +
  theme_minimal() +
  theme(legend.position = "top",legend.background = element_rect(fill = "lightblue", # Background
                                                                 colour = 1))


all_plots<-h+ggsurv$plot
print(all_plots)


