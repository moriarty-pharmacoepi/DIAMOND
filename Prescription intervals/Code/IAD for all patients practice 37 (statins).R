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
 # facet_wrap(~ genericname) +  # Facet by statin ATC code/generic name
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(multigraph)

##---Statin Summary By ATC-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
statin_summary_by_atc <- statinGP37all %>%
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






##---Merging Datasets----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Step 1: Read in the data
demographics <- read.csv("/Users/padraicdonoghue/Library/CloudStorage/OneDrive-SharedLibraries-RoyalCollegeofSurgeonsinIreland/Frank Moriarty - RSS 2025/demographics_subsample.csv")
prescriptions <- read.csv("/Users/padraicdonoghue/Library/CloudStorage/OneDrive-SharedLibraries-RoyalCollegeofSurgeonsinIreland/Frank Moriarty - RSS 2025/prescriptions_subsample_170625.csv")
#view(prescriptions)
#view(demographics)
# Step 2: Merge the data on patient_id and practice_id
merged_data <- left_join(demographics, prescriptions, by = c("UniquePatientID", "UniquePracticeID"))

# Step 3: Optional - Save the merged data to a new CSV
write.csv(merged_data, "merged_output.csv", row.names = FALSE)
#view(merged_data)

##---Working with merged Data--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Step 2: Filter statins
statins_all <- merged_data %>%
  filter(str_detect(atc_final, "C10AA"))

# Step 3: Convert script_date to Date type
statins_all <- statins_all %>%
  mutate(script_date = as.Date(as.character(script_date), format = "%d%b%Y"))
#view (statins_all)

# Step 4: Create age brackets
statins_all <- statins_all %>%
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

# Step 5: Calculate gap_days by patient
statins_all <- statins_all %>%
  arrange(UniquePatientID, script_date) %>%
  group_by(UniquePatientID) %>%
  mutate(gap_days = as.numeric(script_date - lag(script_date))) %>%
  ungroup()

# Step 6: Group by sex,  scheme, age bracket
statin_summary <- statins_all %>%
  group_by(scheme, sex, age_bracket) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_IAD = mean(gap_days, na.rm = TRUE),
    median_IAD = median(gap_days, na.rm = TRUE),
    sd_IAD = sd(gap_days, na.rm = TRUE),
    iqr_IAD = IQR(gap_days, na.rm = TRUE),
    min_IAD = min(gap_days, na.rm = TRUE),
    max_IAD = max(gap_days, na.rm = TRUE),
    .groups = "drop"
  )
#View(statin_summary)

# Step 6: Group by scheme
statin_summary_scheme <- statins_all %>%
  group_by(scheme) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_IAD = mean(gap_days, na.rm = TRUE),
    median_IAD = median(gap_days, na.rm = TRUE),
    sd_IAD = sd(gap_days, na.rm = TRUE),
    iqr_IAD = IQR(gap_days, na.rm = TRUE),
    min_IAD = min(gap_days, na.rm = TRUE),
    max_IAD = max(gap_days, na.rm = TRUE),
    .groups = "drop"
  )
#View(statin_summary_scheme)

# Step 6: Group by sex
statin_summary_sex <- statins_all %>%
  group_by(sex) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_IAD = mean(gap_days, na.rm = TRUE),
    median_IAD = median(gap_days, na.rm = TRUE),
    sd_IAD = sd(gap_days, na.rm = TRUE),
    iqr_IAD = IQR(gap_days, na.rm = TRUE),
    min_IAD = min(gap_days, na.rm = TRUE),
    max_IAD = max(gap_days, na.rm = TRUE),
    .groups = "drop"
  )
#View(statin_summary_sex)

# Step 6: Group by age bracket 
statin_summary_age_bracket <- statins_all %>%
  group_by(age_bracket) %>%
  summarise(
    count = sum(!is.na(gap_days)),
    mean_IAD = mean(gap_days, na.rm = TRUE),
    median_IAD = median(gap_days, na.rm = TRUE),
    sd_IAD = sd(gap_days, na.rm = TRUE),
    iqr_IAD = IQR(gap_days, na.rm = TRUE),
    min_IAD = min(gap_days, na.rm = TRUE),
    max_IAD = max(gap_days, na.rm = TRUE),
    .groups = "drop"
  )
#View(statin_summary_age_bracket)


sdplot <- ggplot(statin_summary, aes(x = age_bracket, y = mean_IAD, color = factor(sex), group = sex)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_IAD - sd_IAD, ymax = mean_IAD + sd_IAD), width = 0.2) +
  facet_wrap(~ scheme) +
  scale_y_continuous(breaks = seq(0, max(statin_summary$mean_IAD + statin_summary$sd_IAD, na.rm = TRUE), by = 60)) +
  labs(
    title = "Mean Gap Days Between Statin Prescriptions ± SD",
    x = "Age Bracket",
    y = "Mean IAD",
    color = "Sex"
  ) +
  theme_minimal()

plot(sdplot)





