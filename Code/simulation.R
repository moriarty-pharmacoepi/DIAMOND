# Load the required package
library(usethis)
library("tidyverse")
library(ggplot2)


# Initialization of simulation parameters ---------------------------------
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
    sample(0:1, 1))
  # transpose the dataframe
  covariates = t(data.frame(covariate_value))
  colnames(covariates) <- covariate_names
  return(covariates)
}

# Constructor for patient observation
new_Patient <- function(id) {
  covariates <- generate_random_covariates()  # Generate random covariates
  patient <- structure(list(id = id, init_covariates = covariates), class = "Patient")
  return(patient)
}

# Patient population generation
start.time <- Sys.time()
Patient_pop <- list()
for (x in 1:6000) {
  Patient_pop[[x]] <- new_Patient(x)  # Assign a unique ID to each patient
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


print(Patient_pop[1])

#Unlisting the patient population to a DF for plotting
df <- data.frame(matrix(unlist(Patient_pop), nrow=length(Patient_pop), byrow=TRUE))
colnames(df) <- c('id',covariate_names)

ggplot(df, aes(risk1)) +
  geom_bar()