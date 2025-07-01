##---##Getting Started##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)

##---##Isolating Statin Data---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

df <- read.csv("/Users/padraicdonoghue/Library/CloudStorage/OneDrive-SharedLibraries-RoyalCollegeofSurgeonsinIreland/Frank Moriarty - RSS 2025/prescriptions_subsample_170625.csv")
codes <- c("C10AA07","C10AA01", "C10AA03", "C10AA05" )

statins <- df %>% 
  filter(atc_final %in% codes)
view(statins)

##---##Formatting Date---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

statins <- statins %>% 
  mutate(script_date = as.Date(as.character(script_date), 
                               format = "%d%b%Y"))  

##---##Isolating one practice--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

statinGP37all <- statins %>% 
  filter(UniquePracticeID == 37) %>% 
  arrange(script_date) %>%
  mutate(gap_days = as.numeric(difftime(script_date, lag(script_date), units = "days")))
view(statinGP37all)

##---##Generating a column that includes the gap between previous and current RX-----------------------------------------------------------------------------------------------------------------------

statinGP37all <- statinGP37all %>%
  arrange(UniquePatientID, script_date) %>%  
  group_by(UniquePatientID) %>%
  mutate(gap_days = as.numeric(script_date - lag(script_date))) %>%
  ungroup()

##---Plotting------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(statinGP37all, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black") +
  labs(
    title = "Density of Gaps Between Statin Prescriptions (Practice 37) Over A 1 Year Period",
    x = "Days Since Last Statin Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()

##---Generating Simple Stats on the Histogram----------------------------------------------------------------------------------------------------------------------------------------------------------

summary(statinGP37all)

##---Nice code that generates a line of best fit that could be good for presentations------------------------------------------------------------------------------------------------------------------
ggplot(statinGP37all, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black", alpha = 0.6) +
  geom_density(colour = "darkred", size = 1) +
  labs(
    title = "Density of Gaps Between Statin Prescriptions (Practice 37)",
    x = "Days Since Last Statin Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()

##---Generating Individual statin histograms-----------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(statinGP37all, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black", alpha = 0.6) +
  labs(
    title = "Density of Gaps Between Statin Prescriptions (by ATC Code)",
    x = "Days Since Last Statin Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  facet_wrap(~ genericname) +  # Facet by statin ATC code/generic name
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##---Statin Summary By ATC-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
statin_summary_by_atc <- statinGP37all %>%
  group_by(atc_final) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_gap = mean(gap_days, na.rm = TRUE),
    median_gap = median(gap_days, na.rm = TRUE),
    sd_gap = sd(gap_days, na.rm = TRUE),
    iqr_gap = IQR(gap_days, na.rm = TRUE),
    min_gap = min(gap_days, na.rm = TRUE),
    max_gap = max(gap_days, na.rm = TRUE)
  )

View(statin_summary_by_atc)
