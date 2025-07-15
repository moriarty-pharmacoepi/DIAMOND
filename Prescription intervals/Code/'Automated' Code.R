##---In this code you must command+F for "CHOSEN ATC" and then replace this with the desired ATC code/Portion of ATC code---##


##---##Getting Started##-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)

##---##Isolating ATC Data---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

df <- read.csv("/Users/padraicdonoghue/Library/CloudStorage/OneDrive-SharedLibraries-RoyalCollegeofSurgeonsinIreland/Frank Moriarty - RSS 2025/prescriptions_subsample_170625.csv")
ATC <- df %>% 
  filter(str_detect(atc_final, "CHOSEN ATC"))

##---##Formatting Date---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ATC <- ATC %>% 
  mutate(script_date = as.Date(as.character(script_date), 
                               format = "%d%b%Y"))  

##---##Isolating one practice--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ATC <- ATC %>% 
  arrange(script_date) %>%
  mutate(gap_days = as.numeric(difftime(script_date, lag(script_date), units = "days")))

##---##Generating a column that includes the gap between previous and current RX-----------------------------------------------------------------------------------------------------------------------

ATC <- ATC %>%
  arrange(UniquePatientID, script_date) %>%  
  group_by(UniquePatientID) %>%
  mutate(gap_days = as.numeric(script_date - lag(script_date))) %>%
  ungroup()


##---Group for number of issues = 1 (30 days)---------------------------------------------
onemonth <- ATC %>% 
  filter(numberofissues==1)

graphone <- ggplot(onemonth, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black") +
  labs(
    title = "Density of Gaps Between 30 Day ATC Prescriptions (Practice 37) Over A 1 Year Period",
    x = "Days Since Last ATC Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()
plot(graphone)

##---Group for number of issues = 3 (90 days)---------------------------------------------

threemonth <- ATC %>% 
  filter(numberofissues==3)

graphtwo<- ggplot(threemonth, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black") +
  labs(
    title = "Density of Gaps Between 90 Day ATC Prescriptions (Practice 37) Over A 1 Year Period",
    x = "Days Since Last ATC Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()
plot(graphtwo)

##---Plotting------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

graphthree <- ggplot(ATC, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black") +
  labs(
    title = "Density of Gaps Between ATC Prescriptions (Practice 37) Over A 1 Year Period",
    x = "Days Since Last ATC Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()
plot(graphthree)

##---Generating Simple Stats on the Histogram----------------------------------------------------------------------------------------------------------------------------------------------------------

summary(ATC)

##---Nice code that generates a line of best fit that could be good for presentations------------------------------------------------------------------------------------------------------------------
graphfour <- ggplot(ATC, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black", alpha = 0.6) +
  geom_density(colour = "darkred", size = 1) +
  labs(
    title = "Density of Gaps Between ATC Prescriptions (Practice 37)",
    x = "Days Since Last ATC Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  theme_minimal()
plot(graphfour)

##---Generating Individual ATC histograms-----------------------------------------------------------------------------------------------------------------------------------------------------------

multigraph<- ggplot(ATC, aes(x = gap_days, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "black", alpha = 0.6) +
  labs(
    title = "Density of Gaps Between ATC Prescriptions (by ATC Code)",
    x = "Days Since Last ATC Prescription",
    y = "Density"
  ) +
  scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  coord_cartesian(xlim = c(0, 365)) +
  # facet_wrap(~ genericname) +  # Facet by ATC ATC code/generic name
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(multigraph)

##---ATC Summary By ATC-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary_by_atc <- ATC %>%
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
#view(summary_by_atc)






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
# Step 2: Filter ATCs
ATC <- merged_data %>%
  filter(str_detect(atc_final, "CHOSEN ATC"))

# Step 3: Convert script_date to Date type
ATC <- ATC %>%
  mutate(script_date = as.Date(as.character(script_date), format = "%d%b%Y"))
#view (ATCs_all)

# Step 4: Create age brackets
ATC <- ATC %>%
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
ATC <- ATC %>%
  arrange(UniquePatientID, script_date) %>%
  group_by(UniquePatientID) %>%
  mutate(gap_days = as.numeric(script_date - lag(script_date))) %>%
  ungroup()

# Step 6: Group by scheme, sex, age bracket — and summarise
ATC_summary <- ATC %>%
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

# View result
#View(ATC_summary)


sdplot <- ggplot(ATC_summary, aes(x = age_bracket, y = mean_IAD, color = factor(sex), group = sex)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_IAD - sd_IAD, ymax = mean_IAD + sd_IAD), width = 0.2) +
  facet_wrap(~ scheme) +
  scale_y_continuous(breaks = seq(0, max(ATC_summary$mean_IAD + ATC_summary$sd_IAD, na.rm = TRUE), by = 60)) +
  labs(
    title = "Mean Gap Days Between ATC Prescriptions ± SD",
    x = "Age Bracket",
    y = "Mean IAD",
    color = "Sex"
  ) +
  theme_minimal()

plot(sdplot)