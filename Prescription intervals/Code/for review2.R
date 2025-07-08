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

#view(first_times)


##---##Days To First Prescription##---------------------------------------------
study_start <- min(first_rx$first_script, na.rm = TRUE)

first_rx <- first_rx %>% 
  mutate(days_to_first = as.integer(first_script - study_start)) 
first_times <- first_rx$days_to_first

##---##Plotting The Histogram##-------------------------------------------------
graph <- ggplot(first_rx, aes(x = days_to_first)) +
  geom_histogram(binwidth = 30, closed = "left", fill = "steelblue", colour = "black") +
  labs(title = "Graph of WTD for Statin Prescriptions From Study Start",
       x = "Days from study start to first script",
       y = "Number of patients" +
         theme_minimal())

plot(graph)

##---##Summary Table of Interarrival Gap Times by GP Practice##----------------
statins_gaps <- statins %>%
  arrange(UniquePatientID, script_date) %>%
  group_by(UniquePatientID) %>%
  mutate(gap_days = as.numeric(difftime(script_date, lag(script_date), units = "days"))) %>%
  filter(!is.na(gap_days))  # Remove first prescriptions (no gap)



#---CDF Plot##------------------------------------------------------------------
patient_avg_gaps <- statins_gaps %>%
  group_by(UniquePatientID) %>%
  summarise(avg_gap = mean(gap_days, na.rm = TRUE), .groups = "drop")

cdf_plot <- ggplot(statins_gaps, aes(x = gap_days)) +
  stat_ecdf(geom = "step", colour = "skyblue", linewidth = 1) +
  scale_x_continuous(
    breaks = seq(0, max(patient_avg_gaps$avg_gap, na.rm = TRUE), by = 60)
  ) +
  labs(
    title = "CDF of Interarrival Times for Statin Prescriptions",
    x = "Days Between Prescriptions",
    y = "Cumulative Probability"
  ) +
  theme_minimal()

plot(cdf_plot)

##---## Smoothed Histogram of Average Gap Time Per Patient##--------------------
density_plot <- ggplot(patient_avg_gaps, aes(x = avg_gap)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  scale_x_continuous(
    breaks = seq(0, max(patient_avg_gaps$avg_gap, na.rm = TRUE), by = 60)
  ) +
  labs(
    title = "Smoothed Histogram of Average Gap Time per Patient (Statins)",
    x = "Average Days Between Prescriptions",
    y = "Density"
  ) +
  theme_minimal()

plot(density_plot)








