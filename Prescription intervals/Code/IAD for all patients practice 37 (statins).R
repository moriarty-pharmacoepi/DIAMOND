##---##Getting Started##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)

##---##Isolating Statin Data---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

df <- read.csv("/Users/padraicdonoghue/Library/CloudStorage/OneDrive-SharedLibraries-RoyalCollegeofSurgeonsinIreland/Frank Moriarty - RSS 2025/prescriptions_subsample_170625.csv")
statins <- df %>% 
  filter(str_detect(atc_final, "C10AA"))

##---##Formatting Date---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

statins <- statins %>% 
  mutate(script_date = as.Date(as.character(script_date), 
                               format = "%d%b%Y"))  

##---##Isolating one practice--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

statinGP37all <- statins %>% 
  filter(UniquePracticeID == 37) %>% 
  arrange(script_date) %>%
  mutate(gap_days = as.numeric(difftime(script_date, lag(script_date), units = "days")))

##---##Generating a column that includes the gap between previous and current RX-----------------------------------------------------------------------------------------------------------------------

statinGP37all <- statinGP37all %>%
  arrange(UniquePatientID, script_date) %>%  
  group_by(UniquePatientID) %>%
  mutate(gap_days = as.numeric(script_date - lag(script_date))) %>%
  ungroup()


##---Group for number of issues = 1 (30 days)---------------------------------------------
onemonth <- statinGP37all %>% 
  filter(numberofissues==1)

graphone <- ggplot(onemonth, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black") +
  labs(
    title = "Density of Gaps Between 30 Day Statin Prescriptions (Practice 37) Over A 1 Year Period",
    x = "Days Since Last Statin Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()
plot(graphone)

##---Group for number of issues = 3 (90 days)---------------------------------------------

threemonth <- statinGP37all %>% 
  filter(numberofissues==3)

graphtwo<- ggplot(threemonth, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black") +
  labs(
    title = "Density of Gaps Between 90 Day Statin Prescriptions (Practice 37) Over A 1 Year Period",
    x = "Days Since Last Statin Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()
plot(graphtwo)

##---Plotting------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

graphthree <- ggplot(statinGP37all, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black") +
  labs(
    title = "Density of Gaps Between Statin Prescriptions (Practice 37) Over A 1 Year Period",
    x = "Days Since Last Statin Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()
plot(graphthree)

##---Generating Simple Stats on the Histogram----------------------------------------------------------------------------------------------------------------------------------------------------------

summary(statinGP37all)

##---Nice code that generates a line of best fit that could be good for presentations------------------------------------------------------------------------------------------------------------------
graphfour <- ggplot(statinGP37all, aes(x = gap_days, y = ..density..)) +
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
plot(graphfour)

##---Generating Individual statin histograms-----------------------------------------------------------------------------------------------------------------------------------------------------------

multigraph<- ggplot(statinGP37all, aes(x = gap_days, y = ..density..)) +
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

plot(multigraph)

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
view(statin_summary_by_atc)
