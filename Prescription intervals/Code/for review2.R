##---##Running Software##-------------------------------------------------------
library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)
library(stringr)

##---##Isolating & Formatting Statin Data##-------------------------------------
df <- read.csv("/Users/padraicdonoghue/Library/CloudStorage/OneDrive-SharedLibraries-RoyalCollegeofSurgeonsinIreland/Frank Moriarty - RSS 2025/prescriptions_subsample_170625.csv")

statins <- df %>% 
  filter(str_detect(atc_final, "C10AA"))

statins <- statins %>% 
  mutate(script_date = as.Date(as.character(script_date), 
                               format = "%d%b%Y"))        

##---##Gathering 1st Prescription Dates##---------------------------------------
first_rx <- statins %>% 
  arrange(UniquePatientID, script_date) %>%     
  group_by(UniquePatientID) %>% 
  summarise(first_script = first(script_date), .groups = "drop")
first_times <- first_rx$days_to_first
#view(first_times)


##---##Days To First Prescription##---------------------------------------------
study_start <- min(first_rx$first_script, na.rm = TRUE)

first_rx <- first_rx %>% 
  mutate(days_to_first = as.integer(first_script - study_start)) 


##---##Plotting The Histogram##-------------------------------------------------
graph <- ggplot(first_rx, aes(x = days_to_first)) +
  geom_histogram(binwidth = 30, closed = "left", fill = "steelblue", colour = "black") +
  labs(title = "Graph of WTD for Statin Prescriptions From Study Start",
       x = "Days from study start to first script",
       y = "Number of patients" +
         theme_minimal())

plot(graph)









