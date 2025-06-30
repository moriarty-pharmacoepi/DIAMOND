##---##Running Software##-------------------------------------------------------
library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)  

##---##Isolating Statin Data##--------------------------------------------------
master <- prescriptions_subsample_170625

codes <- c("A02BC01","A02BC02", "A02BC03", "A02BC05" )

master <- master %>% 
  filter(atc_final %in% codes)
view(master)

##---##Formatting Date##--------------------------------------------------------
master <- master %>% 
  mutate(script_date = as.Date(as.character(script_date), 
                               format = "%d%b%Y"))        

##---##Gathering 1st Prescription Dates##---------------------------------------
first_rx <- master %>% 
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
  labs(title = "Graph of WTD for PPI Prescriptions From Study Start",
       x = "Days from study start to first script",
       y = "Number of patients" +
         theme_minimal())

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
    title = "Overlay of g(r) Function on PPI Prescription Histogram",
    x = "Days from study start to first script",
    y = "Density"
  ) +
  theme_minimal()







