##---##Getting Started##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)

##---##Isolating Data---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

statins <- read.csv("/Users/padraicdonoghue/Desktop/GitHub/DIAMOND/Prescription intervals/Code/merged_output.csv")

##---##Formatting Date and Adding Month/Year Covariates-------------------------------------------------------------------------------

statins <- statins %>% 
  filter(str_detect(atc_final, "C10AA")) %>% 
  mutate(
    script_date = as.Date(as.character(script_date), format = "%d%b%Y"),
    prescription_month = month(script_date, label = TRUE, abbr = FALSE),  # Full month name
    prescription_year = year(script_date))

statins <- statins %>%
  arrange(UniquePatientID, script_date) %>%  # Critical for correct ordering
  group_by(UniquePatientID) %>%             # Essential for patient-specific gaps
  mutate(
    gap_days = as.numeric(difftime(lead(script_date), script_date, units = "days")),
    gap_days = abs(gap_days)  # Ensures positive values
  ) %>%
  ungroup()

##---Group for number of issues = 1 (30 days)---------------------------------------------
onemonth <- statins %>% 
  filter(numberofissues==1)

graphone <- ggplot(onemonth, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black") +
  labs(
    title = "Density of Gaps Between 30 Day Statin Prescriptions Over A 1 Year Period",
    x = "Days Since Last Statin Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()
plot(graphone)

##---Group for number of issues = 3 (90 days)---------------------------------------------

threemonth <- statins %>% 
  filter(numberofissues==3)

graphtwo<- ggplot(threemonth, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black") +
  labs(
    title = "Density of Gaps Between 90 Day Statin Prescriptions Over A 1 Year Period",
    x = "Days Since Last Statin Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()
plot(graphtwo)

##---Plotting------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

graphthree <- ggplot(statins, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black") +
  labs(
    title = "Density of Gaps Between Statin Prescriptions Over A 1 Year Period",
    x = "Days Since Last Statin Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()
plot(graphthree)

##---Generating Simple Stats on the Histogram----------------------------------------------------------------------------------------------------------------------------------------------------------

summary(statins)

##---Generating Individual statin histograms-----------------------------------------------------------------------------------------------------------------------------------------------------------

multigraph<- ggplot(statins, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black", alpha = 0.6) +
  labs(
    title = "Density of Gaps Between Statin Prescriptions (by ATC Code)",
    x = "Days Since Last Statin Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  # facet_wrap(~ genericname) +  # Facet by statin ATC code/generic name
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(multigraph)

##---Statin Summary By ATC-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
statin_summary_by_atc <- statins %>%
  # group_by(atc_final, genericname) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_IAD = mean(gap_days, na.rm = TRUE),
    median_IAD = median(gap_days, na.rm = TRUE),
    sd_IAD = sd(gap_days, na.rm = TRUE),
    iqr_IAD = IQR(gap_days, na.rm = TRUE),
    min_IAD = min(gap_days, na.rm = TRUE),
    max_IAD = max(gap_days, na.rm = TRUE)
  )
#view(statin_summary_by_atc)

##---Working with merged Data--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Create age brackets
statins <- statins %>%
  mutate(age_bracket = case_when(
    age >= 60 & age <= 64 ~ "60–64",
    age >= 65 & age <= 69 ~ "65–69",
    age >= 70 & age <= 74 ~ "70–74",
    age >= 75 & age <= 79 ~ "75–79",
    age >= 80 & age <= 84 ~ "80–84",
    age >= 85 & age <= 90 ~ "85–90",
    age > 90 ~ "90+",
    TRUE ~ NA_character_
  ))

#Calculate gap_days by patient
statins <- statins %>%
  arrange(UniquePatientID, script_date) %>%
  group_by(UniquePatientID) %>%
  mutate(gap_days = as.numeric(script_date - lag(script_date))) %>%
  ungroup()

# Step 6: Group by sex,  scheme, age bracket
statin_summary <- statins %>%
  group_by(scheme, sex, age_bracket) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_IAD = ifelse(count == 0, NA, mean(gap_days, na.rm = TRUE)),
    median_IAD = ifelse(count == 0, NA, median(gap_days, na.rm = TRUE)),
    sd_IAD = ifelse(count < 2, NA, sd(gap_days, na.rm = TRUE)), # SD needs ≥2 obs
    iqr_IAD = ifelse(count == 0, NA, IQR(gap_days, na.rm = TRUE)),
    min_IAD = ifelse(count == 0, NA, min(gap_days, na.rm = TRUE)),
    max_IAD = ifelse(count == 0, NA, max(gap_days, na.rm = TRUE)),
    .groups = "drop"
  )
#view(statin_summary)

# Step 6: Group by scheme
statin_summary_scheme <- statins %>%
  group_by(scheme) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_IAD = ifelse(count == 0, NA, mean(gap_days, na.rm = TRUE)),
    median_IAD = ifelse(count == 0, NA, median(gap_days, na.rm = TRUE)),
    sd_IAD = ifelse(count < 2, NA, sd(gap_days, na.rm = TRUE)), # SD needs ≥2 obs
    iqr_IAD = ifelse(count == 0, NA, IQR(gap_days, na.rm = TRUE)),
    min_IAD = ifelse(count == 0, NA, min(gap_days, na.rm = TRUE)),
    max_IAD = ifelse(count == 0, NA, max(gap_days, na.rm = TRUE)),
    .groups = "drop"
  )
#View(statin_summary_scheme)

# Step 6: Group by sex
statin_summary_sex <- statins %>%
  group_by(sex) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_IAD = ifelse(count == 0, NA, mean(gap_days, na.rm = TRUE)),
    median_IAD = ifelse(count == 0, NA, median(gap_days, na.rm = TRUE)),
    sd_IAD = ifelse(count < 2, NA, sd(gap_days, na.rm = TRUE)), # SD needs ≥2 obs
    iqr_IAD = ifelse(count == 0, NA, IQR(gap_days, na.rm = TRUE)),
    min_IAD = ifelse(count == 0, NA, min(gap_days, na.rm = TRUE)),
    max_IAD = ifelse(count == 0, NA, max(gap_days, na.rm = TRUE)),
    .groups = "drop"
  )
#View(statin_summary_sex)

# Step 6: Group by age bracket 
statin_summary_age_bracket <- statins %>%
  group_by(age_bracket) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_IAD = ifelse(count == 0, NA, mean(gap_days, na.rm = TRUE)),
    median_IAD = ifelse(count == 0, NA, median(gap_days, na.rm = TRUE)),
    sd_IAD = ifelse(count < 2, NA, sd(gap_days, na.rm = TRUE)), # SD needs ≥2 obs
    iqr_IAD = ifelse(count == 0, NA, IQR(gap_days, na.rm = TRUE)),
    min_IAD = ifelse(count == 0, NA, min(gap_days, na.rm = TRUE)),
    max_IAD = ifelse(count == 0, NA, max(gap_days, na.rm = TRUE)),
    .groups = "drop"
  )
#View(statin_summary_age_bracket)

#group by year
statin_year <- statins %>%
  group_by(prescription_year) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_IAD = ifelse(count == 0, NA, mean(gap_days, na.rm = TRUE)),
    median_IAD = ifelse(count == 0, NA, median(gap_days, na.rm = TRUE)),
    sd_IAD = ifelse(count < 2, NA, sd(gap_days, na.rm = TRUE)), # SD needs ≥2 obs
    iqr_IAD = ifelse(count == 0, NA, IQR(gap_days, na.rm = TRUE)),
    min_IAD = ifelse(count == 0, NA, min(gap_days, na.rm = TRUE)),
    max_IAD = ifelse(count == 0, NA, max(gap_days, na.rm = TRUE)),
    .groups = "drop"
  )
#View(statin_summary_year)

#group by month
statin_month <- statins %>%
  group_by(prescription_month) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_IAD = ifelse(count == 0, NA, mean(gap_days, na.rm = TRUE)),
    median_IAD = ifelse(count == 0, NA, median(gap_days, na.rm = TRUE)),
    sd_IAD = ifelse(count < 2, NA, sd(gap_days, na.rm = TRUE)), # SD needs ≥2 obs
    iqr_IAD = ifelse(count == 0, NA, IQR(gap_days, na.rm = TRUE)),
    min_IAD = ifelse(count == 0, NA, min(gap_days, na.rm = TRUE)),
    max_IAD = ifelse(count == 0, NA, max(gap_days, na.rm = TRUE)),
    .groups = "drop"
  )

#View(statin_summary_month)

boxplot <- ggplot(statins %>% filter(!is.na(gap_days)), 
                  aes(x = age_bracket, y = gap_days, 
                      fill = factor(sex),    # Fill color by sex
                      color = factor(sex))) + # Border color by sex
  geom_boxplot(alpha = 0.7) +  # alpha controls transparency
  facet_wrap(~ scheme) +
  scale_y_continuous(breaks = seq(0, max(statins$gap_days, na.rm = TRUE), by = 60)) +
  labs(
    title = "Distribution of Gap Days Between Statin Prescriptions",
    x = "Age Bracket",
    y = "Days Between Prescriptions",
    fill = "Sex",  # Legend title for fill
    color = "Sex"  # Legend title for border color
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(boxplot)