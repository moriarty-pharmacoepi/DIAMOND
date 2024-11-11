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

# Initialization of simulation parameters ---------------------------------
num_patients = 10

# number of continuous random covariates
n_continuous_covariates = 1

# number of binary random covariates
n_binary_covariates = 2

# Names of each covariate, continuous first then binary
covariate_names <- c("Age", "risk1", "risk2")

# Function for generating patient baseline covariates
generate_random_covariates <- function(covariate_names) {
  covariate_value<- c(
    runif(1, 18, 35),
    sample(0:1, 1),
    sample(0:10, 1))
  # transpose the dataframe
  covariates = t(data.frame(covariate_value))
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

# generating the patient history
generate_event_history <- function(patient){
  # generate random day
  rand_day = sample(0:365, 1, replace=TRUE)
  
  # date of first event in patient history
  init_date <- as.Date("20/09/2021",format = "%d/%m/%Y") + rand_day
  
  # simulate disease onset, given covariates - currently independent (Bernoulli)
  prob_disease = 0.5
  if (runif(1) < prob_disease){
    # disease onset
    disease_state <- 1}
  else{
    # no disease
    disease_state <- 0}
  patient$disease <- disease_state
  return(patient)

}

#generate_event_history(Patient_pop[1])
#
#for (x in 1:num_patients) {
#  Patient_pop[[x]] <- generate_event_history(Patient_pop[x])  
#}



sample_from_rate <- function(rate) {
  if (rate == 0) {
    return(Inf)  # Return Inf if rate is 0 (no transition)
  }
  
  if (rate > 0) {
    return(rexp(1, rate))  # Sample from an exponential distribution for positive rates
  }
  
  # If the rate is negative (diagonal Q-matrix element), we return Inf 
  # because we're not "sampling" from a negative rate in this context.
  return(Inf)
}



# Define the 'simulate_cmc' function
simulate_cmc <- function(Q, time, warm_up) {
  Q <- as.matrix(Q)  # Ensure Q is a matrix
  state_space <- 1:nrow(Q)  # State space is indexed from 1 to number of rows
  time_spent <- rep(0, length(state_space))  # Vector to keep track of time spent in each state
  clock <- 0  # The simulation clock
  current_state <- 1  # Initial state (1-based indexing in R)
  
  history <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(history) <- c('state', 'time spent')
  while (clock < time) {
    
    # Initialize a vector to store the sojourn times for each possible transition
    sojourn_times <- numeric(length(state_space)) 
    
    # Generate sojourn times for transitions before the current state
    for (i in 1:(current_state )) {
      #message('i is:', i)
      #message('current state is:', i)
      sojourn_times[i] <- sample_from_rate(Q[current_state, i])  # Rates for transitions before the current state
    }
    
    # An infinite sojourn to the same state
     sojourn_times[current_state] <- Inf
    
    # Generate sojourn times for transitions after the current state
    for (i in (current_state):ncol(Q)) {
      sojourn_times[i] <- sample_from_rate(Q[current_state, i])  # Rates for transitions after the current state
    }
    
    # Identify the next state (state with the minimum sojourn time)
    next_state <- which.min(sojourn_times)  # Find the index of the minimum sojourn time
    sojourn <- sojourn_times[next_state]  # The sojourn time to the next state
    clock <- clock + sojourn  # Advance the clock by the sojourn time
    #print(sojourn_times)
    history[nrow(history) + 1,] = c(current_state, sojourn)
    # If the clock is past the warm-up period, record the time spent in the current state
    if (clock > warm_up) {
        #print(sojourn_times)
      time_spent[current_state] <- time_spent[current_state] + sojourn
    }
    
    current_state <- next_state  # Transition to the next state
    
    # if in the absorbing state, finish simulation and append last state
    if (current_state == 4){
      history[nrow(history) + 1,] = c(current_state, 0)
      break} 
  }
  
  # Calculate the steady-state probabilities
  pi <- time_spent / sum(time_spent)  # Normalize the time spent to get probabilities
  return(list(pi = pi, history = history))
  # Return the steady-state probabilities
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
hazards <- matrix(c(0,   0.1,   0.05,  0.0001,  # Prescribed
                    0.000,   0,   0,  0.001,  # Adverse
                    0, 0.0,   0, 0.002, # Deprescribed
                    0,   0, 0.0,  0), # Death
                  nrow = 4, ncol = 4, byrow = TRUE)

# Generate the Q-matrix based on the hazard rates
Q <- generate_Q_matrix_from_hazards(hazards)


# Parameters: time to simulate, warm-up time
#time unit: months
time <- 5000
warm_up <- 0

# Run the simulation
out <- simulate_cmc(Q, time, warm_up)
steady_state_probabilities <-out$pi
history <- out$history
# Print the result
#print((steady_state_probabilities))


# Initialize an empty list to store the full histories of states and times for each patient
history_list <- list()

# Initialize an empty vector to store the time spent in all states for each patient
history_time_spent <- list()

# Simulating histories for 100 patients
for (i in 1:1000) {
  # Simulate the patient history (assuming simulate_cmc returns an object with a $history attribute)
  out <- simulate_cmc(Q, time, warm_up)
  history <- out$history  # Assuming history is a data frame or matrix
  #history <- rbind(c(1,0),history)
  # Add a Patient_ID column to the history to identify the patient for each state-time pair
  patient_id <- rep(i, nrow(history))  # Replicate the patient ID for each row in their history
  history_with_id <- cbind(Patient_ID = patient_id, history)  # Combine Patient_ID with history data
  # Store the entire history for patient i (with Patient_ID)
  history_list[[i]] <- history_with_id
  # Store time spent in all states for patient i
  # Assuming the second column of history contains the time spent in each state
  history_time_spent[[i]] <- history[, 2]  # Extracting the time spent in each state for patient i
}

# Flatten the list into a single data frame
flat_history <- do.call(rbind, history_list)

# Assign column names (adjust according to the structure of your `history`)
colnames(flat_history) <- c("Patient_ID", "State", "Time_Spent")

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
  slice(-1) %>%     # Filter for rows where State is 1           # Group by Patient ID
  summarise(total_time_in_state_1 = mean(Time_Spent, na.rm = TRUE)) # Calculate the mean

# Print the result
print('mean sojourn empirically calculated:')
print(mean(mean_time_in_state_1$total_time_in_state_1))
print('mean sojourn predicted from Q:')
print(-1/Q[1,1])

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

df_history$Event <- ifelse(df_history$State == 4, 1, 0)

df_history <- df_history %>%
  group_by(Patient_ID) %>%
  mutate(Group = if_else(sum(State == 3) > 0, 1, 0)) %>%
  ungroup()
df_history$Group <- factor(df_history$Group, levels = c(0, 1), labels = c("Continuing", "Deprescribed"))
print(df_history,n=200)

df_history <- df_history %>%
  group_by(Patient_ID) %>%
  mutate(time_to_death = sum(Time_Spent))%>%
  ungroup()

#df_history$Group <- factor(df_history$Group, levels = c(0, 1), labels = c("No State 3", "State 3 Entered"))
#print(df_history,n=200)


# Create a survival object
surv_obj <- Surv(time = df_history$time_to_death, event = df_history$Event)

# Inspect the survival object
surv_obj

fit <- survfit(surv_obj ~ Group, data = df_history)

# Print the survival curve summary
print(fit)

# Plot the Kaplan-Meier curves by Group
plot(fit, main = "Survival Curves by Group", xlab = "Time", ylab = "Survival Probability",
     col = c("blue", "red"), lty = 1:2)  # Different colors and line types for the two groups

# Add a legend to the plot
legend("topright", legend = c("Continuing", "Deprescribed"), col = c("blue", "red"), lty = 1:2)


df_history2 <- df_history %>%
  group_by(Patient_ID) %>%
  mutate(Cumulative_Time = cumsum(Time_Spent)) %>%
  ungroup()

# Plot individual state trajectories for each patient
h<-ggplot(df_history2, aes(x = Cumulative_Time, y = State, group = Patient_ID, color = Patient_ID)) +
  geom_step(direction = "vh") +  # Use a step plot to show state changes over time
  labs(title = "State Transitions for Each Patient",
       x = "Time (Days)",
       y = "State",
       color = "Patient") +
  theme_minimal() +
  theme(legend.position = "none")  #
#print(h)
#print(history_list)