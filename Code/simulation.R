# Load the required package
library(usethis)
library("tidyverse")

# Function for generating patient baseline covariates
generate_random_covariates <- function() {
  covariate_names <- c("Age", "smoker", "disease")
  covariate_value<- c(runif(1, 18, 35), sample(0:1, 1), sample(0:1, 1))
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
Patient_pop <- list()
for (x in 1:1) {
  Patient_pop[[x]] <- new_Patient(x)  # Assign a unique ID to each patient
}
print(Patient_pop[1])

