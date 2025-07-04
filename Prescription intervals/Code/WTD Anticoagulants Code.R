##---##Getting Started##--------------------------------------------------------
library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)

##---##Isolating Desired ATC Codes From Data set##------------------------------
anticoagulants <- read.csv("/Users/padraicdonoghue/Library/CloudStorage/OneDrive-SharedLibraries-RoyalCollegeofSurgeonsinIreland/Frank Moriarty - RSS 2025/prescriptions_subsample_170625.csv")
view(anticoagulants)
codes <- c("B01AA03", "B01AB01", "B01AB04", "B01AB05", "B01AB10", "B01AE07", "B01AF01", "B01AF02", "B01AF03" )
anticoagulants <- anticoagulants %>%
  filter(atc_final %in% codes)
view(anticoagulants)

##---##Altering ATC's##---------------------------------------------------------
  anticoagulants <- anticoagulants %>% 
  mutate(atc_final = case_when(
    atc_final == "B01AA03" ~ "B01AZ",
    atc_final == "B01AB04" ~ "B01AZ",
    atc_final == "B01AB05" ~ "B01AZ",
    atc_final == "B01AB10" ~ "B01AZ",
    atc_final == "B01AE07" ~ "B01AZ",
    atc_final == "B01AF01" ~ "B01AZ",
    atc_final == "B01AF02" ~ "B01AZ",
    atc_final == "B01AF03" ~ "B01AZ",
    TRUE ~ atc_final
  ))

##---##Formatting Date##--------------------------------------------------------
anticoagulants <- anticoagulants %>% 
  mutate(script_date = as.Date(as.character(script_date), 
                               format = "%d%b%Y"))        

##---##Gathering 1st Prescription Dates##---------------------------------------
first_rx <- anticoagulants %>% 
  arrange(UniquePatientID, script_date) %>%     
  group_by(UniquePatientID) %>% 
  summarise(first_script = first(script_date), .groups = "drop")

##---##Days To First Prescription##---------------------------------------------
study_start <- min(first_rx$first_script, na.rm = TRUE)

first_rx <- first_rx %>% 
  mutate(days_to_first = as.integer(first_script - study_start)) 


##---##Plotting The Histogram##-------------------------------------------------
ggplot(first_rx, aes(x = days_to_first)) +
  geom_histogram(binwidth = 30, closed = "left", fill = "steelblue", colour = "black") +
  labs(title = "Graph of WTD for Anticoagulant Prescriptions From Study Start",
       x = "Days from study start to first script",
       y = "Number of patients who recieved a prescription for an anticoagulant")

##---Adding in Figure 2---------------------------------------------------------
g_data <- data.frame(
  days = seq(0, 250, length.out = 800)
) %>%
  mutate(density = g(days))

##---##Combining both-----------------------------------------------------------

ggplot(first_rx, aes(x = days_to_first)) +
  geom_histogram(aes(y = ..density..), binwidth = 30, closed = "left", fill = "steelblue", colour = "black") +
  geom_line(data = g_data, aes(x = days, y = density), color = "red", size = 1.2) +
  labs(
    title = "Overlay of g(r) Function on Anticoagulant Prescription Histogram",
    x = "Days from study start to first script",
    y = "Density"
  ) +
  theme_minimal()

  