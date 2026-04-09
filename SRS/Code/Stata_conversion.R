# =================================================================================================================================================================================
# Libraries
# =================================================================================================================================================================================
library(haven)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyverse)
library(readr)
library(ggplot2)
library(scales)
library(viridis)
library(here)
library(scales)
library(readxl)
library(broom)
library(gtsummary)
library(tidyr)
library(gt)

#script_path <- rstudioapi::getActiveDocumentContext()$path
#setwd(dirname(script_path))
#library(gtsummary)
#here::i_am("SRS project/DIAMOND/SRS/Code/Stata_conversion.r") 
# =================================================================================================================================================================================
# Load data
# =================================================================================================================================================================================
#library(here)  # if you use here()

library(dplyr)
library(here)

data_file_path <- here("C:/Users/frankmoriarty/OneDrive - Royal College of Surgeons in Ireland/PCRS data")

# File lists
list1_files  <- list.files(data_file_path, pattern = "^LIST1_[0-9]{4}\\.txt$", full.names = TRUE)
list1a_files <- list.files(data_file_path, pattern = "^LIST1A_[0-9]{4}\\.txt$", full.names = TRUE)
list2_files  <- list.files(data_file_path, pattern = "^LIST2_[0-9]{4}\\.txt$", full.names = TRUE)
list4_files  <- list.files(data_file_path, pattern = "^LIST4_[0-9]{4}\\.txt$", full.names = TRUE)
list4_2NEWATCS_files  <- list.files(data_file_path, pattern = "^LIST4_2NEWATCS_[0-9]{4}\\.txt$", full.names = TRUE)

all_files <- c(list1_files, list1a_files, list2_files, list4_files, list4_2NEWATCS_files)

# Placeholder for merged yearly data
data_list <- list()


lst <- vector("list", length(unique(sub(".*_([0-9]{4})\\.txt$", "\\1", basename(all_files)))))
# Assume analgesic_ind is already loaded

# Loop through each year found in filenames
for (yr in unique(sub(".*_([0-9]{4})\\.txt$", "\\1", basename(all_files)))) {
  # print(yr)
  yearly_dfs <- list()  # temporary list for this year's dataframes
  
  # LIST files for this year (combine all LIST types)
  for (file_vec in list(list1_files, list1a_files, list2_files, list4_files, list4_2NEWATCS_files)) {
    files <- file_vec[grepl(paste0("_", yr, "\\.txt$"), file_vec)]
    
    for (f in files) {
      df <- read.csv(f, header = TRUE)
      
      # Parse dateofdispensing (dd/mm/yyyy) and extract year
      df$year <- as.numeric(yr)
      
      df$year <- year(dmy(df$Date.of.dispensing))
      print(yr)     
      
      df <- df %>%
        rename(individualidentifiernumber = Individual.identifier.number) %>%
        rename(atccode = ATC.Code) %>%
        rename(age = Age) %>%
        rename(sex = Sex) %>%
        rename(dateofdispensing = Date.of.dispensing) %>%
        rename(gpidentifiernumber = GP.identifier.number) %>%
        rename(medicationname = Medication.name) %>%
        rename(code = Code) %>%
        rename(quantity = Quantity) %>%
        rename(cost = Cost) %>%
        rename(patientlho = Patient.LHO) 
      
      
      yearly_dfs <- c(yearly_dfs, list(df))
    }
  }
  
  # Combine all LISTs for this year by stacking rows
  analgesic_ind <- bind_rows(yearly_dfs)
  

  
  #print(analgesic_ind$year[1])
  
  analgesic_ind <- analgesic_ind %>%
    filter(year(as.Date(dateofdispensing)) != year) %>%
    filter(!grepl("^C10AA", atccode))
  
  
  # Preserve equivalent.
  subset_dates <- analgesic_ind %>%
    filter(!(
      (str_detect(atccode, "N02AJ06")
    )))

  analgesic_ind <- analgesic_ind %>%
    filter((
      (str_detect(atccode, "N02AJ06")
      )))
  
  subset_dates <- subset_dates %>%
    distinct(year, atccode, individualidentifiernumber, .keep_all = TRUE)
  
  
  analgesic_ind <- bind_rows(subset_dates, analgesic_ind)
    

  df <- analgesic_ind
  
  df <- df %>%
    rename(indID = individualidentifiernumber)
  
  # =================================================================================================================================================================================
  # Create 30 quantiles (xtile)
  # =================================================================================================================================================================================
  df <- df %>%
    mutate(ID_d = ntile(indID, 30))
  
  # =================================================================================================================================================================================
  # LHO to LHO_Area mapping
  # =================================================================================================================================================================================
  
  df <- df %>%
    mutate(
      LHO_area = case_when(
        patientlho == 1  ~ "Dun Laoghaire",
        patientlho == 2  ~ "Dublin South East",
        patientlho == 3  ~ "Dublin South City",
        patientlho == 4  ~ "Dublin South West",
        patientlho == 5  ~ "Dublin West",
        patientlho == 6  ~ "Kildare/West Wicklow",
        patientlho == 7  ~ "Wicklow",
        patientlho == 8  ~ "Laois/Offaly",
        patientlho == 9  ~ "Longford/Westmeath",
        patientlho == 10  ~ "Dublin North West",
        patientlho == 11  ~ "Dublin North Central",
        patientlho == 12  ~ "Dublin North",
        patientlho == 13  ~ "Cavan/Monaghan",
        patientlho == 14  ~ "Louth",
        patientlho == 15  ~ "Meath",
        patientlho == 16  ~ "Galway",
        patientlho == 17  ~ "Mayo",
        patientlho == 18  ~ "Roscommon",
        patientlho == 19  ~ "Donegal",
        patientlho == 20  ~ "Sligo/Leitrim/West Cavan",
        patientlho == 21  ~ "Clare",
        patientlho == 22  ~ "North Tipperary/East Limerick",
        patientlho == 23  ~ "Limerick",
        patientlho == 24  ~ "South Lee",
        patientlho == 25  ~ "North Lee",
        patientlho == 26  ~ "West Cork",
        patientlho == 27  ~ "Kerry",
        patientlho == 28  ~ "North Cork",
        patientlho == 29  ~ "Carlow/Kilkenny",
        patientlho == 30  ~ "Waterford",
        patientlho == 31  ~ "South Tipperary",
        patientlho == 32  ~ "Wexford",
        TRUE ~ NA_character_
      ),
      CHO_area = case_when(
        # CHO 1
        patientlho %in% c(13, 19, 20) ~ "1",
        
        # CHO 2
        patientlho %in% c(16, 17, 18) ~ "2",
        
        # CHO 3
        patientlho %in% c(21, 22, 23) ~ "3",
        
        # CHO 4
        patientlho %in% c(24, 25, 26, 27, 28) ~ "4",
        
        # CHO 5
        patientlho %in% c(29, 30, 31, 32) ~ "5",
        
        # CHO 6
        patientlho %in% c(1, 2, 7) ~ "6",
        
        # CHO 7
        patientlho %in% c(3, 4, 5, 6) ~ "7",
        
        # CHO 8
        patientlho %in% c(8, 9, 14, 15) ~ "8",
        
        # CHO 9
        patientlho %in% c(10, 11, 12) ~ "9",
        
        TRUE ~ NA_character_
      )
    )
  # =================================================================================================================================================================================
  # Drug categorisation
  # =================================================================================================================================================================================
  df <- df %>%
    mutate(
      opioid = str_sub(atccode,1,4)=="N02A" | atccode=="R05DA04",
      morphine = atccode %in% c("N02AA01","N02AA51"),
      hydromorphone = atccode=="N02AA03",
      oxycodone = atccode %in% c("N02AA05","N02AA55"),
      pethidine = atccode=="N02AB02",
      codeine = atccode %in% c("N02AJ06","R05DA04"),
      dihydrocodeine = atccode %in% c("N02AA08","N02AJ01"),
      fentanyl = atccode=="N02AB03",
      buprenorphine = atccode=="N02AE01",
      tramadol = atccode %in% c("N02AX02","N02AJ13","N02AJ14"),
      tapentadol = atccode=="N02AX06",
      meptazinol = atccode=="N02AX05"
    )
  
  df <- df %>%
    mutate(
      strongop = morphine | hydromorphone | oxycodone |
        pethidine | fentanyl | buprenorphine |
        tapentadol | tramadol,
      strongop = ifelse(!opioid, NA, strongop)
    )
  
  # =================================================================================================================================================================================
  # Systemic NSAIDs
  # =================================================================================================================================================================================
  df <- df %>%
    mutate(
      sysnsaid = str_sub(atccode,1,4)=="M01A" | atccode=="N02AJ14",
      coxib = str_sub(atccode,1,5)=="M01AH",
      nsnsaid = sysnsaid & !coxib
    )
  
  # =================================================================================================================================================================================
  # Paracetamol
  # =================================================================================================================================================================================
  df <- df %>%
    mutate(
      paracetamol = atccode %in% c("N02BE01","N02BE51","N02AJ01","N02AJ06","N02AJ13"),
      paraonly = atccode %in% c("N02BE01","N02BE51")
    )
  
  # =================================================================================================================================================================================
  # Strength extraction
  # =================================================================================================================================================================================
  df <- df %>%
    mutate(strength = as.numeric(str_extract(medicationname, "\\d+\\.?\\d*(?= Mg)")))
  
  # Fentanyl mcg conversion
  df <- df %>%
    mutate(
      strength = case_when(
        str_detect(medicationname,"50 Mcg") ~ 0.05,
        str_detect(medicationname,"100 Mcg") ~ 0.1,
        TRUE ~ strength
      )
    )
  
  # =================================================================================================================================================================================
  # DDD calculation
  # =================================================================================================================================================================================
  df <- df %>%
    mutate(DDD = NA_real_)
  
  df <- df %>%
    mutate(
      DDD = case_when(
        atccode=="N02BE01" ~ quantity*strength/3000,
        atccode=="N02AX02" ~ quantity*strength/300,
        atccode=="N02BF01" ~ quantity*strength/1800,
        TRUE ~ DDD
      )
    )
  
  # =================================================================================================================================================================================
  # Old OME calculation, new one below for codeine specifically
  # =================================================================================================================================================================================
  df <- df %>%
    mutate(
      ome = case_when(
        (codeine == 1 | dihydrocodeine == 1) & !is.na(quantity) & !is.na(strength) ~ quantity * strength * 0.1,
        hydromorphone == 1 & !is.na(quantity) & !is.na(strength) ~ quantity * strength * 5,
        oxycodone == 1 & !is.na(quantity) & !is.na(strength) ~ quantity * strength * 1.5,
        tramadol == 1 & !is.na(quantity) & !is.na(strength) ~ quantity * strength * 0.2,
        morphine == 1 & !is.na(quantity) & !is.na(strength) ~ quantity * strength * 1,
        TRUE ~ NA_real_
      )
    )
  # =================================================================================================================================================================================
  # Rolling 30-day OME (asrol equivalent)
  # =================================================================================================================================================================================
  setDT(df)
  setorder(df, indID, dateofdispensing)
  
  
  # =================================================================================================================================================================================
  # Keep final analytic sample
  # =================================================================================================================================================================================
  df <- df %>%
   filter(    year >= 2014, year <= 2022)
  
  #Ensure date is a real Date (dd/mm/yyyy)
  df <- df %>%
    mutate(
      dateofdispensing = as.Date(dateofdispensing, format = "%d/%m/%Y"),
      ome = as.numeric(ome)  # just in case it's character
    )
  # ================================================================================================================================================================================
  # Hard coding codeine content to drug names
  # =================================================================================================================================================================================
  codeine_products <-  df %>%
    filter(str_detect(atccode, "N02AJ0")) %>%
    arrange(medicationname) 
  
  df <- df %>%
    mutate(codeine_dose = case_when(
      (str_detect(medicationname, "Co-Codamol") |
         (str_detect(medicationname, "Kapake") & str_detect(medicationname, "Tabs 30")) |
         str_detect(medicationname, "Kapake Tabs 30") |
         str_detect(medicationname, "Solpadol") |
         str_detect(medicationname, "Tylex")) ~ 30,
      (str_detect(medicationname, "Solpadeine") |
         str_detect(medicationname, "Maxilief")) ~ 8,
      (str_detect(medicationname, "Kapake Tabs 15") |
         str_detect(medicationname, "Codipar") ~ 15),
      
      TRUE ~ NA_real_
    ))
  
  # =================================================================================================================================================================================
  # Calculating OME's for codeine specifically
  # =================================================================================================================================================================================
  df <- df %>%
    mutate(ome = ifelse(!is.na(codeine_dose), quantity * codeine_dose * 0.1, NA))
  
  monthly_ome_codeine <- df %>%
    filter(!is.na(ome), !is.na(dateofdispensing)) %>%
    mutate(month = floor_date(dateofdispensing, "month")) %>%
    group_by(month) %>%
    summarise(total_ome = sum(ome, na.rm = TRUE), 
              total_rx = n(),
              normrx = total_ome/total_rx)
  # =================================================================================================================================================================================
  # Grading codeine OME's as high or low
  # =================================================================================================================================================================================
  
  df <- df %>%
    mutate(
      codeine_ranking = case_when(
        codeine_dose <= 15 ~ "Low",
        codeine_dose > 15 ~ "High",
        TRUE ~ NA_character_
      )
    )
  
  lst[[yr]]<-df
  
}
df <- do.call(rbind, lst)

rm(lst, analgesic_ind, subset_dates, yearly_dfs, codeine_products)
gc()

# =================================================================================================================================================================================
# Generating Objective 2 Table (05/03/26)
# =================================================================================================================================================================================  

objective_two <- df %>%
  filter(
    codeine == TRUE,
    year(ymd(dateofdispensing)) == 2022,
    !is.na(codeine_ranking),
    codeine_ranking %in% c("High", "Low")
  ) %>%
  transmute(
    dateofdispensing = ymd(dateofdispensing),
    indID,
    age,
    sex,
    patientlho,
    LHO_area,
    CHO_area,
    medicationname,
    atccode,
    quantity,
    codeine_dose_mg = codeine_dose,
    high_codeine_dose = if_else(codeine_ranking == "High", 1L, 0L),
    codeine_ranking,
    codeine_ome = if_else(
      !is.na(codeine_dose_mg) & !is.na(quantity),
      quantity * codeine_dose_mg * 0.1,
      NA_real_
    )
  )




monthly_ome_codeine <-rbind(monthly_ome_codeine)



# =================================================================================================================================================================================
# Graphing Codeine OME's per month and subplotting based on HIGH or LOW dose preparations
# ================================================================================================================================================================================
monthly_total_ome_by_dose <- df %>%
  filter(!is.na(ome), !is.na(codeine_ranking), !is.na(dateofdispensing)) %>%
  mutate(month = as.Date(format(dateofdispensing, "%Y-%m-01"))) %>%
  group_by(month, codeine_ranking) %>%
  summarise(total_ome = sum(ome, na.rm = TRUE), .groups = "drop")



# =================================================================================================================================================================================
# Graphing Codeine OME's by sex
# =================================================================================================================================================================================
ome_by_sex <- df %>%
  filter(codeine == TRUE, !is.na(ome)) %>%
  group_by(sex, codeine_ranking) %>%
  summarise(avg_ome = mean(ome, na.rm = TRUE), .groups = "drop")


# =================================================================================================================================================================================
# Graphing Codeine OME's by medication name
# =================================================================================================================================================================================
ome_by_med <- df %>%
  filter(!is.na(ome)) %>%
  group_by(medicationname) %>%
  summarise(avg_ome = mean(ome, na.rm = TRUE)) %>%
  arrange(desc(avg_ome))


# =================================================================================================================================================================================
# Graphing Codeine OME's by GP practice
# =================================================================================================================================================================================
# Identify top 20 GPs overall
top_gps <- df %>%
  filter(!is.na(ome)) %>%
  group_by(gpidentifiernumber) %>%
  summarise(avg_ome = mean(ome, na.rm = TRUE)) %>%
  arrange(desc(avg_ome)) %>%
  slice_head(n = 20)

# Calculate averages for High vs Low dose within those GPs
ome_by_gp <- df %>%
  filter(!is.na(ome), gpidentifiernumber %in% top_gps$gpidentifiernumber) %>%
  group_by(gpidentifiernumber, codeine_ranking) %>%
  summarise(avg_ome = mean(ome, na.rm = TRUE), .groups = "drop")


# =================================================================================================================================================================================
# Boxplot of total OME proportions by sex in high vs low dose
# ================================================================================================================================================================================= 
# non-normalised
ome_by_sex_dose <- df %>%
  filter(!is.na(ome), !is.na(codeine_ranking), !is.na(sex)) %>%
  filter(year(dateofdispensing) == 2022) %>%
  group_by(codeine_ranking, sex) %>%
  summarise(total_ome = sum(ome, na.rm = TRUE), .groups = "drop")




# =================================================================================================================================================================================
# Boxplot and violin of total OME proportions by age in high vs low dose
# ================================================================================================================================================================================= 

boxplot_data <- df %>%
  filter(!is.na(ome), !is.na(age), !is.na(codeine_ranking), !is.na(sex)) %>% 
  filter(year(dateofdispensing) == 2022)


# =================================================================================================================================================================================
# Creating new data frame to perform chi squared test on sex vs codeine consumption
# ================================================================================================================================================================================= 
dose_by_sex <- df %>%
  filter(!is.na(codeine_ranking), sex %in% c("M","F"), codeine_ranking %in% c("High","Low")) %>%
  select(sex,codeine_ranking)%>%
  table()


test1<-chisq.test(dose_by_sex)  
print(test1)

# =================================================================================================================================================================================
# t-test
# ================================================================================================================================================================================= 
ttest_df <- df %>%
  filter(!is.na(codeine_ranking), sex %in% c("M","F"), codeine_ranking %in% c("High","Low")) %>%
  mutate(codeine_ranking = as.factor(codeine_ranking))%>%
  select(age,codeine_ranking)


test2<-t.test(age ~ codeine_ranking,data = ttest_df)
print(test2)

# =================================================================================================================================================================================
# Graphing average Codeine OME by age (10-year brackets from 18–80, then 5-year brackets from 80–110)
# =================================================================================================================================================================================

ome_by_ageband_codeine <- df %>%
  filter(
    codeine == TRUE,
    !is.na(age), !is.na(ome), !is.na(sex),
    age >= 18, age <= 110
  ) %>%
  mutate(
    age_band = cut(
      age,
      breaks = c(18, 30, 40, 50, 60, 70, 80, 85, 90, 95, 100, 105, 110),
      labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79",
                 "80-84", "85-89", "90-94", "95-99", "100-104", "105-109"),
      right = FALSE,
      include.lowest = TRUE
    )
  ) %>%
  group_by(sex, age_band) %>%
  summarise(
    avg_ome = mean(ome, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(age_band = factor(age_band, levels = levels(age_band)))


# =================================================================================================================================================================================
# Graphing average Codeine OME by time for males and females
# =================================================================================================================================================================================  
ome_over_time <- df %>%
  filter(
    codeine == TRUE,
    !is.na(dateofdispensing),
    !is.na(sex),
    !is.na(codeine_ranking),
    !is.na(ome),
    sex %in% c("M", "F"),
    codeine_ranking %in% c("High", "Low")
  ) %>%
  mutate(
    month = floor_date(ymd(dateofdispensing), unit = "month")
  ) %>%
  group_by(month, sex, codeine_ranking) %>%
  summarise(
    avg_ome = mean(ome, na.rm = TRUE),
    .groups = "drop"
  )



# =================================================================================================================================================================================
# Average quantity of tablets by sex and codeine dose
# =================================================================================================================================================================================  

avg_quantity_sex_dose <- df %>%
  filter(
    codeine == TRUE,
    !is.na(codeine_ranking),
    codeine_ranking %in% c("High", "Low")
  ) %>%
  mutate(
    high_codeine_dose = if_else(codeine_ranking == "High", 1L, 0L),
    sex = recode(sex, "M" = "Male", "F" = "Female")
  ) %>%
  group_by(sex, high_codeine_dose) %>%
  summarise(
    avg_quantity = mean(quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    high_codeine_dose = factor(
      high_codeine_dose,
      levels = c(0, 1),
      labels = c("Low dose (0)", "High dose (1)")
    )
  )



# =================================================================================================================================================================================
# Average quantity sex boxplot
# =================================================================================================================================================================================  
boxplot_sex_dose <- df %>%
  filter(
    codeine == TRUE,
    !is.na(codeine_ranking),
    codeine_ranking %in% c("High", "Low"),
    !is.na(sex),
    !is.na(quantity)
  ) %>%
  mutate(
    high_codeine_dose = if_else(codeine_ranking == "High", 1L, 0L),
    
    sex = recode(sex, "M" = "Male", "F" = "Female"),
    
    high_codeine_dose = factor(
      high_codeine_dose,
      levels = c(0, 1),
      labels = c("Low dose (0)", "High dose (1)")
    )
  )

# Plot
ggplot(boxplot_sex_dose, aes(x = sex, y = quantity, fill = high_codeine_dose)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.3) +
  labs(
    title = "Distribution of Codeine Quantity by Sex and Dose",
    x = "Sex",
    y = "Quantity",
    fill = "Codeine Dose"
  ) +
  theme_minimal(base_size = 13)

# =================================================================================================================================================================================
# Average quantity age
# =================================================================================================================================================================================  
avg_quantity_age_dose <- df %>%
  filter(
    codeine == TRUE,
    !is.na(codeine_ranking),
    codeine_ranking %in% c("High", "Low"),
    !is.na(age)
  ) %>%
  mutate(
    high_codeine_dose = if_else(codeine_ranking == "High", 1L, 0L),
    
    age_group = case_when(
      age >= 16 & age <= 24 ~ "16–24 Years",
      age >= 25 & age <= 34 ~ "25–34 Years",
      age >= 35 & age <= 44 ~ "35–44 Years",
      age >= 45 & age <= 54 ~ "45–54 Years",
      age >= 55 & age <= 64 ~ "55–64 Years",
      age >= 65 & age <= 69 ~ "65–69 Years",
      age >= 70 & age <= 74 ~ "70–74 Years",
      age >=75 ~ "75 and Over",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group, high_codeine_dose) %>%
  summarise(
    avg_quantity = mean(quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    age_group = factor(
      age_group,
      levels = c(
        "16–24 Years",
        "25–34 Years",
        "35–44 Years",
        "45–54 Years",
        "55–64 Years",
        "65–69 Years",
        "70–74 Years",
        "75 and Over"
      )
    ),
    high_codeine_dose = factor(
      high_codeine_dose,
      levels = c(0, 1),
      labels = c("Low dose (0)", "High dose (1)")
    )
  )



# =================================================================================================================================================================================
# Quantity by age boxplot
# =================================================================================================================================================================================  
boxplot_age_dose <- df %>%
  filter(
    codeine == TRUE,
    !is.na(codeine_ranking),
    codeine_ranking %in% c("High", "Low"),
    !is.na(age),
    !is.na(quantity)
  ) %>%
  mutate(
    high_codeine_dose = if_else(codeine_ranking == "High", 1L, 0L),
    
    age_group = case_when(
      age >= 16 & age <= 24 ~ "16–24 Years",
      age >= 25 & age <= 34 ~ "25–34 Years",
      age >= 35 & age <= 44 ~ "35–44 Years",
      age >= 45 & age <= 54 ~ "45–54 Years",
      age >= 55 & age <= 64 ~ "55–64 Years",
      age >= 65 & age <= 69 ~ "65–69 Years",
      age >= 70 & age <= 74 ~ "70–74 Years",
      age >= 75 ~ "75 and Over",
      TRUE ~ NA_character_
    ),
    
    high_codeine_dose = factor(
      high_codeine_dose,
      levels = c(0, 1),
      labels = c("Low dose (0)", "High dose (1)")
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  mutate(
    age_group = factor(
      age_group,
      levels = c(
        "16–24 Years",
        "25–34 Years",
        "35–44 Years",
        "45–54 Years",
        "55–64 Years",
        "65–69 Years",
        "70–74 Years",
        "75 and Over"
      )
    )
  )


# =================================================================================================================================================================================
# Hard-Coding oral NSAID's,topical NSAID's, sedatives, gabapentinoids and anti-migraines
# =================================================================================================================================================================================  
oral_nsaids_codes <- c(
  "M01AB01","M01AB05","M01AB55","M01AB16","M01AC06",
  "M01AE01","M01AE02","M01AE52","M01AE03","M01AE09",
  "M01AE17","M01AG01","M01AH01","M01AH05","M01AX01"
)

topical_analgesics_codes <- c(
  "M02AA05","M02AA06","M02AA07",
  "M02AA10","M02AA13","M02AA15",
  "M02AB01"
)

gabapentinoids_codes <- c(
  "N02BF01","N02BF02"
)

benzodiazepines_sedatives_codes <- c(
  "N05BA01","N05BA02","N05BA05", "N05BA06","N05BA08",
  "N05BA09","N05BA11","N05BA12","N05CD01","N05CD02",
  "N05CD03","N05CD05","N05CD06","N05CD07","N05CD08",
  "N05CF01","N05CF02","N03AE01","N06AA09"
)

anti_migraine_codes <- c(
  "N02CC01","N02CC02","N02CC03","N02CC04",
  "N02CC05","N02CC06","N02CC07","N02CX02"
)

Other_Opioids_codes <- c(
  "N02AA01","N02AA51","N02AA03","N02AA05","N02AA55",
  "N02AA08","N02AJ01","N02AB02","N02AB03","N02AE01",
  "N02AX02","N02AJ13","N02AJ14","N02AX05","N02AX06"
)

# ================================================================================================================================================================================
# CREATE PATIENT-LEVEL FLAGS FROM df FOR DISPENSINGS IN 2022
# ================================================================================================================================================================================

other_analgesic_flags_2022 <- df %>%
  mutate(
    dateofdispensing = ymd(dateofdispensing),
    atccode = as.character(atccode)
  ) %>%
  filter(year(dateofdispensing) == 2022) %>%
  group_by(indID) %>%
  summarise(
    Oral_NSAIDs = as.integer(any(atccode %in% oral_nsaids_codes)),
    Topical_analgesics = as.integer(any(atccode %in% topical_analgesics_codes)),
    Gabapentinoids = as.integer(any(atccode %in% gabapentinoids_codes)),
    Benzodiazepines_sedatives = as.integer(any(atccode %in% benzodiazepines_sedatives_codes)),
    Anti_migraines = as.integer(any(atccode %in% anti_migraine_codes)),
    Other_Opioids = as.integer(any(atccode %in% Other_Opioids_codes)),
    .groups = "drop"
  ) %>%
  mutate(
    other_analgesic_total = Oral_NSAIDs +
      Topical_analgesics +
      Gabapentinoids +
      Benzodiazepines_sedatives +
      Anti_migraines +
      Other_Opioids,
    other_analgesic_y_n = as.integer(other_analgesic_total > 0)
  )

# ================================================================================================================================================================================
# JOIN BACK TO objective_two AND FILL MISSING WITH 0
# ================================================================================================================================================================================

objective_two <- objective_two %>%
  select(
    -any_of(c(
      "Oral_NSAIDs",
      "Topical_analgesics",
      "Gabapentinoids",
      "Benzodiazepines_sedatives",
      "Anti_migraines",
      "Other_Opioids",
      "other_analgesic_total",
      "other_analgesic_y_n"
    ))
  ) %>%
  left_join(other_analgesic_flags_2022, by = "indID") %>%
  mutate(
    across(
      c(
        Oral_NSAIDs,
        Topical_analgesics,
        Gabapentinoids,
        Benzodiazepines_sedatives,
        Anti_migraines,
        Other_Opioids,
        other_analgesic_total,
        other_analgesic_y_n
      ),
      ~ tidyr::replace_na(., 0L)
    )
  )
# ================================================================================================================================================================================
# Making anticholinergic groups
# ================================================================================================================================================================================
ach1_codes <- c(
  "R06AD01", "A03AX08","A03AX58","C07AB03","C07FB03","C07CB03","C07CB53","C07BB03",
  "C07DB01","R06AB01","R06AB51","N06AX12","A08AA62","C09AA01","C09BA01","C03BA04",
  "C03BB04","C03EA06","A02BA01","A02BA51","M04AC01","C01AA05","B01AC07","C01BA03",
  "N02AB03","C03CA01", "C03CB01","C03EB01","N06AB08","N05AD01","C02DB02","C02LG02",
  "H02AB09","C01DA08","C01DA58","C05AE02","C01DA14","A07DA03","A07DA05","A07DA53",
  "C07AB02","C07FX03","C07FB13","C07FB02","C07FX05","C07CB02","C07BB02","C07BB52",
  "R05DA05","C08CA05","C07FB03","C08GA01","C08CA55","H02AB07","A07EA03","C01BA01",
  "C01BA51","C01BA71","A02BA02","A02BA07","R03DA04","R03DB04","R03DA54","R03DA74",
  "C03DB02","N06AX05","N05AX08","B01AA03"
)


ach2_codes <- c(
  "N04BB01","A03BA01","A03BA04","A03BB01","A03BB02",
  "A03BB03","A03BB04","A03BB05","A03BB06","A06AB30",
  "N03AF01","M03BX08","R06AX02","N05AA02","N05AH01",
  "N05AE02","N03AF02","N05AG02"
)

ach3_codes <- c(
  "N06AA09","N06AA17","A03CB03","N04AC01","R06AA08",
  "R06AB04","R06AB54","N05AA01","R06AA04","R06AA54",
  "N06AA04","N05AH02","G04BD10","N06AA01","A03AA07",
  "R06AA11","R06AA02","R06AA52","G04BD02","N05BB01",
  "N05BB51","A03BA03","A03CB31","N06AA02","N06AA03",
  "R06AC01","R03DA12","R06AE05","R06AE55","N06AA10",
  "N04AB02","M03BC01","M03BC51","G04BD04","N06AB05",
  "N05AB03","N04AA04","N05AA03","A03CA34","A04AD01",
  "A04AD51","N05CM05","N05AC02","G04BD07","N05AB06",
  "N04AA01","N05AH03","N05AH03","R06AD02","N06AA12","N06AA06"
)



all_ach_codes <- c(ach1_codes, ach2_codes, ach3_codes)

# =================================================================================================================================================================================
# Create patient-level ACB variables from df for 2022
# =================================================================================================================================================================================

ach_flags_2022 <- df %>%
  mutate(
    dateofdispensing = ymd(dateofdispensing),
    atccode = as.character(atccode)
  ) %>%
  filter(
    year(dateofdispensing) == 2022,
    atccode %in% all_ach_codes
  ) %>%
  distinct(indID, atccode) %>%   # counts each unique medicine once per patient
  group_by(indID) %>%
  summarise(
    ach_1_drugs = sum(atccode %in% ach1_codes),
    ach_2_drugs = sum(atccode %in% ach2_codes),
    ach_3_drugs = sum(atccode %in% ach3_codes),
    
    total_ach_meds = ach_1_drugs + ach_2_drugs + ach_3_drugs,
    ach_burden_med = as.integer(total_ach_meds > 0),
    
    total_acb_score = (1 * ach_1_drugs) +
      (2 * ach_2_drugs) +
      (3 * ach_3_drugs),
    
    .groups = "drop"
  )

# =================================================================================================================================================================================
# Join back to objective_two and fill missing with 0
# =================================================================================================================================================================================

objective_two <- objective_two %>%
  select(
    -any_of(c(
      "ach_1_drugs",
      "ach_2_drugs",
      "ach_3_drugs",
      "total_ach_meds",
      "ach_burden_med",
      "total_acb_score"
    ))
  ) %>%
  left_join(ach_flags_2022, by = "indID") %>%
  mutate(
    across(
      c(
        ach_1_drugs,
        ach_2_drugs,
        ach_3_drugs,
        total_ach_meds,
        ach_burden_med,
        total_acb_score
      ),
      ~ replace_na(., 0L)
    )
  )
# ================================================================================================================================================================================
# OME by CHO
# ================================================================================================================================================================================

ome_by_LHO_area <- df %>%
  filter(codeine == TRUE, !is.na(LHO_area), !is.na(ome)) %>%
  group_by(LHO_area) %>%
  summarise(total_ome = sum(ome, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_ome))



# ================================================================================================================================================================================
# OME by LHO_area proportional pi chart
# ================================================================================================================================================================================
ome_by_LHO_area_pi <- df %>%
  filter(codeine == TRUE, !is.na(LHO_area), !is.na(ome)) %>%
  group_by(LHO_area) %>%
  summarise(total_ome = sum(ome, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    prop_ome = total_ome / sum(total_ome),
    label = paste0(round(prop_ome * 100, 1), "%")
  )



# ================================================================================================================================================================================
# Pie carts of most commonly prescribed otehr analgesics across high and low dose users fo codeine
# ================================================================================================================================================================================

analgesic_cols <- c(
  "Oral_NSAIDs",
  "Topical_analgesics",
  "Gabapentinoids",
  "Benzodiazepines_sedatives",
  "Anti_migraines",
  "Other_Opioids"
)

pie_data <- objective_two %>%
  filter(!is.na(high_codeine_dose)) %>%
  distinct(indID, high_codeine_dose, .keep_all = TRUE) %>%
  mutate(
    codeine_group = if_else(high_codeine_dose == 1, "High-dose codeine", "Low-dose codeine")
  ) %>%
  select(indID, codeine_group, all_of(analgesic_cols)) %>%
  pivot_longer(
    cols = all_of(analgesic_cols),
    names_to = "analgesic_type",
    values_to = "present"
  ) %>%
  filter(present == 1) %>%
  group_by(codeine_group, analgesic_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(codeine_group) %>%
  mutate(
    prop = n / sum(n),
    label = paste0(round(prop * 100, 1), "%")
  ) %>%
  ungroup()



# ================================================================================================================================================================================
# proportional bar chart for high and low dose codine user ACH burden
# ================================================================================================================================================================================

ach_plot_data <- objective_two %>%
  filter(!is.na(high_codeine_dose), !is.na(total_acb_score)) %>%
  distinct(indID, high_codeine_dose, total_acb_score, .keep_all = TRUE) %>%
  mutate(
    codeine_group = if_else(high_codeine_dose == 1, "High-dose codeine", "Low-dose codeine"),
    ach_burden = case_when(
      total_acb_score == 0 ~ "ACH = 0",
      total_acb_score %in% c(1, 2) ~ "ACH = 1-2",
      total_acb_score >= 3 ~ "ACH = 3+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ach_burden))



# ================================================================================================================================================================================
# gp level trends
# ================================================================================================================================================================================

gp_metrics <- df %>%
  filter(!is.na(gpidentifiernumber)) %>%
  group_by(gpidentifiernumber) %>%
  summarise(
    total_analgesic = n(),
    total_codeine = sum(codeine == TRUE, na.rm = TRUE),
    high_codeine = sum(codeine == TRUE & codeine_ranking == "High", na.rm = TRUE),
    prop_high_within_codeine = if_else(total_codeine > 0, high_codeine / total_codeine, NA_real_),
    prop_codeine_overall = total_codeine / total_analgesic,
    .groups = "drop"
  )

# ================================================================================================================================================================================
# Pie chart 1: High-dose codeine over all codeine
# ================================================================================================================================================================================

pie_high_codeine <- tibble(
  category = c("High-dose codeine", "Other codeine"),
  value = c(
    mean(gp_metrics$prop_high_within_codeine, na.rm = TRUE),
    1 - mean(gp_metrics$prop_high_within_codeine, na.rm = TRUE)
  )
) %>%
  mutate(
    label = paste0(round(value * 100, 1), "%")
  )



# ================================================================================================================================================================================
# Pie chart 2: Any-dose codeine over all analgesics
# ================================================================================================================================================================================

pie_any_codeine <- tibble(
  category = c("Any-dose codeine", "Other analgesics"),
  value = c(
    mean(gp_metrics$prop_codeine_overall, na.rm = TRUE),
    1 - mean(gp_metrics$prop_codeine_overall, na.rm = TRUE)
  )
) %>%
  mutate(
    label = paste0(round(value * 100, 1), "%")
  )



# ================================================================================================================================================================================
# Create objective_three
# ================================================================================================================================================================================
objective_three <- df %>%
  filter(
    codeine == TRUE,
    !is.na(codeine_ranking),
    codeine_ranking %in% c("High", "Low")
  ) %>%
  transmute(
    dateofdispensing = ymd(dateofdispensing),
    indID,
    age,
    sex,
    patientlho,
    LHO_area,
    CHO_area,
    medicationname,
    atccode,
    quantity,
    codeine_dose_mg = codeine_dose,
    high_codeine_dose = if_else(codeine_ranking == "High", 1L, 0L),
    codeine_ranking,
    codeine_ome = if_else(
      !is.na(codeine_dose_mg) & !is.na(quantity),
      quantity * codeine_dose_mg * 0.1,
      NA_real_
    )
  )

# ================================================================================================================================================================================
# Create annual codeine sums per patient-year from df
# ================================================================================================================================================================================
annual_codeine_sums <- df %>%
  mutate(
    dateofdispensing = ymd(dateofdispensing),
    year = lubridate::year(dateofdispensing),
    codeine_ome = if_else(
      codeine == TRUE & !is.na(codeine_dose) & !is.na(quantity),
      quantity * codeine_dose * 0.1,
      0
    )
  ) %>%
  filter(!is.na(indID), !is.na(year), codeine == TRUE) %>%
  group_by(indID, year) %>%
  summarise(
    high_codeine_sum = sum(if_else(codeine_ranking == "High", codeine_ome, 0), na.rm = TRUE),
    low_codeine_sum = sum(if_else(codeine_ranking == "Low", codeine_ome, 0), na.rm = TRUE),
    total_codeine_annual = sum(codeine_ome, na.rm = TRUE),
    .groups = "drop"
  )

# ================================================================================================================================================================================
# Create annual counts of other analgesics/sedatives per patient-year from df
# ================================================================================================================================================================================
all_other_analgesics_sedatives_codes <- c(
  oral_nsaids_codes,
  topical_analgesics_codes,
  gabapentinoids_codes,
  benzodiazepines_sedatives_codes,
  anti_migraine_codes,
  Other_Opioids_codes
)

annual_other_analgesics <- df %>%
  mutate(
    dateofdispensing = ymd(dateofdispensing),
    year = lubridate::year(dateofdispensing),
    atccode = as.character(atccode)
  ) %>%
  filter(!is.na(indID), !is.na(year)) %>%
  group_by(indID, year) %>%
  summarise(
    all_other_analgesics_sedatives = sum(atccode %in% all_other_analgesics_sedatives_codes, na.rm = TRUE),
    Oral_NSAIDs = sum(atccode %in% oral_nsaids_codes, na.rm = TRUE),
    Topical_analgesics = sum(atccode %in% topical_analgesics_codes, na.rm = TRUE),
    Gabapentinoids = sum(atccode %in% gabapentinoids_codes, na.rm = TRUE),
    Benzodiazepines_sedatives = sum(atccode %in% benzodiazepines_sedatives_codes, na.rm = TRUE),
    Anti_migraines = sum(atccode %in% anti_migraine_codes, na.rm = TRUE),
    Other_Opioids = sum(atccode %in% Other_Opioids_codes, na.rm = TRUE),
    .groups = "drop"
  )

# ================================================================================================================================================================================
# Join annual variables back to objective_three
# ================================================================================================================================================================================
objective_three <- objective_three %>%
  mutate(
    dateofdispensing = ymd(dateofdispensing),
    year = lubridate::year(dateofdispensing)
  ) %>%
  left_join(annual_codeine_sums, by = c("indID", "year")) %>%
  left_join(annual_other_analgesics, by = c("indID", "year")) %>%
  mutate(
    high_codeine_sum = tidyr::replace_na(high_codeine_sum, 0),
    low_codeine_sum = tidyr::replace_na(low_codeine_sum, 0),
    total_codeine_annual = tidyr::replace_na(total_codeine_annual, 0),
    all_other_analgesics_sedatives = tidyr::replace_na(all_other_analgesics_sedatives, 0L),
    Oral_NSAIDs = tidyr::replace_na(Oral_NSAIDs, 0L),
    Topical_analgesics = tidyr::replace_na(Topical_analgesics, 0L),
    Gabapentinoids = tidyr::replace_na(Gabapentinoids, 0L),
    Benzodiazepines_sedatives = tidyr::replace_na(Benzodiazepines_sedatives, 0L),
    Anti_migraines = tidyr::replace_na(Anti_migraines, 0L),
    Other_Opioids = tidyr::replace_na(Other_Opioids, 0L)
  )

# ================================================================================================================================================================================
# Collapse all dispensings to ONE ROW PER PATIENT PER YEAR
# FIX: use first() for annual OME values so they are not summed repeatedly
# ================================================================================================================================================================================
objective_three_annual <- objective_three %>%
  mutate(
    dateofdispensing = ymd(dateofdispensing),
    year = lubridate::year(dateofdispensing)
  ) %>%
  group_by(indID, year) %>%
  summarise(
    age = first(age),
    sex = first(sex),
    patientlho = first(patientlho),
    LHO_area = first(LHO_area),
    CHO_area = first(CHO_area),
    
    high_codeine_sum = first(high_codeine_sum),
    low_codeine_sum = first(low_codeine_sum),
    total_codeine_annual = first(total_codeine_annual),
    
    all_other_analgesics_sedatives = first(all_other_analgesics_sedatives),
    Oral_NSAIDs = first(Oral_NSAIDs),
    Topical_analgesics = first(Topical_analgesics),
    Gabapentinoids = first(Gabapentinoids),
    Benzodiazepines_sedatives = first(Benzodiazepines_sedatives),
    Anti_migraines = first(Anti_migraines),
    Other_Opioids = first(Other_Opioids),
    
    n_dispensings_year = n(),
    .groups = "drop"
  )
objective_three_annual <- objective_three_annual %>%
  rename(
    GENDER = sex,
    CLIENT_NORM_AGE_GROUP_DESC = age,
    LHO_DESC_CHO = LHO_area,
  )

# ================================================================================================================================================================================
# Mimicing Excel Formats
# ================================================================================================================================================================================
objective_three_annual <- objective_three_annual %>%
  mutate(
    CLIENT_NORM_AGE_GROUP_DESC = case_when(
      CLIENT_NORM_AGE_GROUP_DESC >= 16 & CLIENT_NORM_AGE_GROUP_DESC <= 24 ~ "16-24 Years",
      CLIENT_NORM_AGE_GROUP_DESC >= 25 & CLIENT_NORM_AGE_GROUP_DESC <= 34 ~ "25-34 Years",
      CLIENT_NORM_AGE_GROUP_DESC >= 35 & CLIENT_NORM_AGE_GROUP_DESC <= 44 ~ "35-44 Years",
      CLIENT_NORM_AGE_GROUP_DESC >= 45 & CLIENT_NORM_AGE_GROUP_DESC <= 54 ~ "45-54 Years",
      CLIENT_NORM_AGE_GROUP_DESC >= 55 & CLIENT_NORM_AGE_GROUP_DESC <= 64 ~ "55-64 Years",
      CLIENT_NORM_AGE_GROUP_DESC >= 65 & CLIENT_NORM_AGE_GROUP_DESC <= 69 ~ "65-69 Years",
      CLIENT_NORM_AGE_GROUP_DESC >= 70 & CLIENT_NORM_AGE_GROUP_DESC <= 74 ~ "70-74 Years",
      CLIENT_NORM_AGE_GROUP_DESC >= 75 ~ "75 Years and Over",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(CLIENT_NORM_AGE_GROUP_DESC >= 16)

objective_three_annual <- objective_three_annual %>%
  mutate(
    GENDER = case_when(
      GENDER == "M" ~ "Male",
      GENDER == "F" ~ "Female",
      TRUE ~ GENDER
    )
  )

# ================================================================================================================================================================================
# Merging Excel
# ================================================================================================================================================================================
# Read Excel exactly as it is
#here::i_am("SRS project/Data/age_sex_lho_eligibility_2017-2021.xlsx")
merge_one_path <- here("C:/Users/frankmoriarty/OneDrive - Royal College of Surgeons in Ireland/PCRS data/age_sex_lho_eligibility_2017-2021.xlsx")
eligibility <- read_excel(merge_one_path)

eligibility <- eligibility %>%
  rename(Age_Classification = '"Age Classification"')

# Create April-only lookup from the Excel
eligibility_april_lookup <- eligibility %>%
  mutate(
    YEAR = as.integer(substr(as.character(MTH_YR), 1, 4)),
    MONTH = as.integer(substr(as.character(MTH_YR), 5, 6))
  ) %>%
  filter(
    MONTH == 4,
    !CLIENT_NORM_AGE_GROUP_DESC %in% c("Under 5 Years", "05-11 Years", "12-15 Years")
  ) %>%
  mutate(
    GENDER = case_when(
      toupper(trimws(GENDER)) == "M" ~ "Male",
      toupper(trimws(GENDER)) == "F" ~ "Female",
      TRUE ~ trimws(GENDER)
    ),
    CLIENT_NORM_AGE_GROUP_DESC = str_squish(CLIENT_NORM_AGE_GROUP_DESC),
    LHO_DESC_CHO = str_squish(LHO_DESC_CHO),
    LHO_DESC_CHO = case_when(
      LHO_DESC_CHO == "Cavan / Monaghan" ~ "Cavan/Monaghan",
      LHO_DESC_CHO == "Kildare / West Wicklow" ~ "Kildare/West Wicklow",
      LHO_DESC_CHO == "Laois / Offaly" ~ "Laois/Offaly",
      LHO_DESC_CHO == "Longford / Westmeath" ~ "Longford/Westmeath",
      LHO_DESC_CHO == "North Tipp./East Limerick" ~ "North Tipperary/East Limerick",
      LHO_DESC_CHO == "Sligo / Leitrim / West Cavan" ~ "Sligo/Leitrim/West Cavan",
      TRUE ~ LHO_DESC_CHO
    )
  ) %>%
  select(
    YEAR,
    GENDER,
    CLIENT_NORM_AGE_GROUP_DESC,
    LHO_DESC_CHO,
    Age_Classification
  )

# Match onto objective_three_annual and add new column
objective_three_annual <- objective_three_annual %>%
  mutate(
    year = as.integer(year),
    GENDER = case_when(
      toupper(trimws(GENDER)) == "M" ~ "Male",
      toupper(trimws(GENDER)) == "F" ~ "Female",
      TRUE ~ trimws(GENDER)
    ),
    CLIENT_NORM_AGE_GROUP_DESC = str_squish(CLIENT_NORM_AGE_GROUP_DESC),
    LHO_DESC_CHO = str_squish(LHO_DESC_CHO),
    LHO_DESC_CHO = case_when(
      LHO_DESC_CHO == "Cavan / Monaghan" ~ "Cavan/Monaghan",
      LHO_DESC_CHO == "Kildare / West Wicklow" ~ "Kildare/West Wicklow",
      LHO_DESC_CHO == "Laois / Offaly" ~ "Laois/Offaly",
      LHO_DESC_CHO == "Longford / Westmeath" ~ "Longford/Westmeath",
      LHO_DESC_CHO == "North Tipp./East Limerick" ~ "North Tipperary/East Limerick",
      LHO_DESC_CHO == "Sligo / Leitrim / West Cavan" ~ "Sligo/Leitrim/West Cavan",
      TRUE ~ LHO_DESC_CHO
    )
  ) %>%
  left_join(
    eligibility_april_lookup,
    by = c(
      "year" = "YEAR",
      "GENDER",
      "CLIENT_NORM_AGE_GROUP_DESC",
      "LHO_DESC_CHO"
    )
  )

# =========================================================================================================
# importing 2022 eligibility numbers
# =========================================================================================================
#here::i_am("SRS project/Data/Monthly GMS Medical Cards - Number of Eligible Persons.csv")
merge_one_path <- here("C:/Users/frankmoriarty/OneDrive - Royal College of Surgeons in Ireland/PCRS data/Monthly GMS Medical Cards - Number of Eligible Persons.csv")
gms_april_lookup <- read_csv(merge_one_path)
# Rename columns
gms_april_lookup <- gms_april_lookup %>%
  rename(
    Age_Classification = `...5`,
    CLIENT_NORM_AGE_GROUP_DESC = `Age Classification`,
    LHO_DESC_CHO = `Local Health Office (LHO)`,
    GENDER = Gender
  )

# ================================================================================================================================================================================
# Fill missing Age_Classification values in objective_three_annual
# using April 2022 CSV values matched on year + sex + age group + LHO
# ================================================================================================================================================================================

# 1. Prepare lookup from CSV
gms_april_lookup_match <- gms_april_lookup %>%
  filter(
    !CLIENT_NORM_AGE_GROUP_DESC %in% c("Under 5 Years", "05-11 Years", "12-15 Years")
  ) %>%
  mutate(
    YEAR = 2022,
    GENDER = case_when(
      toupper(trimws(GENDER)) == "M" ~ "Male",
      toupper(trimws(GENDER)) == "F" ~ "Female",
      TRUE ~ trimws(GENDER)
    ),
    CLIENT_NORM_AGE_GROUP_DESC = str_squish(CLIENT_NORM_AGE_GROUP_DESC),
    LHO_DESC_CHO = str_squish(LHO_DESC_CHO),
    LHO_DESC_CHO = case_when(
      LHO_DESC_CHO == "Cavan / Monaghan" ~ "Cavan/Monaghan",
      LHO_DESC_CHO == "Kildare / West Wicklow" ~ "Kildare/West Wicklow",
      LHO_DESC_CHO == "Laois / Offaly" ~ "Laois/Offaly",
      LHO_DESC_CHO == "Longford / Westmeath" ~ "Longford/Westmeath",
      LHO_DESC_CHO == "North Tipp./East Limerick" ~ "North Tipperary/East Limerick",
      LHO_DESC_CHO == "Sligo / Leitrim / West Cavan" ~ "Sligo/Leitrim/West Cavan",
      TRUE ~ LHO_DESC_CHO
    )
  ) %>%
  select(
    YEAR,
    GENDER,
    CLIENT_NORM_AGE_GROUP_DESC,
    LHO_DESC_CHO,
    Age_Classification
  ) %>%
  distinct()

# 2. Match onto your dataset and fill only missing values
objective_three_annual <- objective_three_annual %>%
  mutate(
    year = as.integer(year),
    GENDER = case_when(
      toupper(trimws(GENDER)) == "M" ~ "Male",
      toupper(trimws(GENDER)) == "F" ~ "Female",
      TRUE ~ trimws(GENDER)
    ),
    CLIENT_NORM_AGE_GROUP_DESC = str_squish(CLIENT_NORM_AGE_GROUP_DESC),
    LHO_DESC_CHO = str_squish(LHO_DESC_CHO),
    LHO_DESC_CHO = case_when(
      LHO_DESC_CHO == "Cavan / Monaghan" ~ "Cavan/Monaghan",
      LHO_DESC_CHO == "Kildare / West Wicklow" ~ "Kildare/West Wicklow",
      LHO_DESC_CHO == "Laois / Offaly" ~ "Laois/Offaly",
      LHO_DESC_CHO == "Longford / Westmeath" ~ "Longford/Westmeath",
      LHO_DESC_CHO == "North Tipp./East Limerick" ~ "North Tipperary/East Limerick",
      LHO_DESC_CHO == "Sligo / Leitrim / West Cavan" ~ "Sligo/Leitrim/West Cavan",
      TRUE ~ LHO_DESC_CHO
    )
  ) %>%
  left_join(
    gms_april_lookup_match,
    by = c(
      "year" = "YEAR",
      "GENDER",
      "CLIENT_NORM_AGE_GROUP_DESC",
      "LHO_DESC_CHO"
    )
  ) %>%
  mutate(
    Age_Classification = if_else(
      year == 2022,
      coalesce(Age_Classification.x, Age_Classification.y),
      Age_Classification.x
    )
  ) %>%
  select(-Age_Classification.x, -Age_Classification.y)
# =========================================================================================================
# GRAPH SET FOR OBJECTIVE 1 AND OBJECTIVE 2
# Based on existing df / objective_two structure
# Objective 1 = ALL YEARS
# Objective 2 = 2022 ONLY
# ========================================================================================================
# =========================================================================================================
# COMMON HELPERS
# =========================================================================================================

age_levels <- c(
  "16–24 Years",
  "25–34 Years",
  "35–44 Years",
  "45–54 Years",
  "55–64 Years",
  "65–69 Years",
  "70–74 Years",
  "75 and Over"
)

codeine_all_years <- df %>%
  filter(
    codeine == TRUE,
    !is.na(codeine_ranking),
    codeine_ranking %in% c("High", "Low")
  ) %>%
  mutate(
    month = floor_date(dateofdispensing, unit = "month"),
    year = year(dateofdispensing),
    sex_label = recode(sex, "M" = "Male", "F" = "Female"),
    age_group = case_when(
      age >= 16 & age <= 24 ~ "16–24 Years",
      age >= 25 & age <= 34 ~ "25–34 Years",
      age >= 35 & age <= 44 ~ "35–44 Years",
      age >= 45 & age <= 54 ~ "45–54 Years",
      age >= 55 & age <= 64 ~ "55–64 Years",
      age >= 65 & age <= 69 ~ "65–69 Years",
      age >= 70 & age <= 74 ~ "70–74 Years",
      age >= 75 ~ "75 and Over",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = age_levels),
    high_codeine_dose = if_else(codeine_ranking == "High", 1L, 0L),
    high_codeine_dose_f = factor(
      high_codeine_dose,
      levels = c(0, 1),
      labels = c("Low dose", "High dose")
    )
  )

codeine_2022 <- objective_two %>%
  mutate(
    sex_label = recode(sex, "M" = "Male", "F" = "Female"),
    age_group = case_when(
      age >= 16 & age <= 24 ~ "16–24 Years",
      age >= 25 & age <= 34 ~ "25–34 Years",
      age >= 35 & age <= 44 ~ "35–44 Years",
      age >= 45 & age <= 54 ~ "45–54 Years",
      age >= 55 & age <= 64 ~ "55–64 Years",
      age >= 65 & age <= 69 ~ "65–69 Years",
      age >= 70 & age <= 74 ~ "70–74 Years",
      age >= 75 ~ "75 and Over",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = age_levels),
    high_codeine_dose_f = factor(
      high_codeine_dose,
      levels = c(0, 1),
      labels = c("Low dose", "High dose")
    )
  )

# =========================================================================================================
# OBJECTIVE 1: ALL YEARS
# To determine trends in high vs low dose codeine products in terms of:
# dispensing, unit tablets, morphine equivalents and people
# Also trends by demographics
# =========================================================================================================

# ---------------------------------------------------------------------------------------------------------
# 1. Monthly dispensing count over time: High vs Low dose
# ---------------------------------------------------------------------------------------------------------
obj1_dispensing_over_time <- codeine_all_years %>%
  filter(!is.na(month)) %>%
  group_by(month, codeine_ranking) %>%
  summarise(
    n_prescriptions = n(),
    .groups = "drop"
  )



# ---------------------------------------------------------------------------------------------------------
# 2. Monthly unique people over time: High vs Low dose
# ---------------------------------------------------------------------------------------------------------
obj1_people_over_time <- codeine_all_years %>%
  filter(!is.na(month), !is.na(indID)) %>%
  group_by(month, codeine_ranking) %>%
  summarise(
    n_people = n_distinct(indID),
    .groups = "drop"
  )



# ---------------------------------------------------------------------------------------------------------
# 3. Monthly average quantity over time: High vs Low dose
# ---------------------------------------------------------------------------------------------------------
obj1_quantity_over_time <- codeine_all_years %>%
  filter(!is.na(month), !is.na(quantity)) %>%
  group_by(month, codeine_ranking) %>%
  summarise(
    avg_quantity = mean(quantity, na.rm = TRUE),
    .groups = "drop"
  )



# ---------------------------------------------------------------------------------------------------------
# 4. Monthly total quantity over time: High vs Low dose
# ---------------------------------------------------------------------------------------------------------
obj1_total_quantity_over_time <- codeine_all_years %>%
  filter(!is.na(month), !is.na(quantity)) %>%
  group_by(month, codeine_ranking) %>%
  summarise(
    total_quantity = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )



# ---------------------------------------------------------------------------------------------------------
# 5. Monthly average OME over time: High vs Low dose
# ---------------------------------------------------------------------------------------------------------
obj1_ome_over_time <- codeine_all_years %>%
  filter(!is.na(month), !is.na(ome)) %>%
  group_by(month, codeine_ranking) %>%
  summarise(
    avg_ome = mean(ome, na.rm = TRUE),
    .groups = "drop"
  )



# ---------------------------------------------------------------------------------------------------------
# 6. Monthly total OME over time: High vs Low dose
# ---------------------------------------------------------------------------------------------------------
obj1_total_ome_over_time <- codeine_all_years %>%
  filter(!is.na(month), !is.na(ome)) %>%
  group_by(month, codeine_ranking) %>%
  summarise(
    total_ome = sum(ome, na.rm = TRUE),
    .groups = "drop"
  )



# ---------------------------------------------------------------------------------------------------------
# 7. Proportion high-dose over time
# ---------------------------------------------------------------------------------------------------------
obj1_prop_high_over_time <- codeine_all_years %>%
  filter(!is.na(month)) %>%
  group_by(month) %>%
  summarise(
    prop_high = mean(codeine_ranking == "High", na.rm = TRUE),
    .groups = "drop"
  )



# ---------------------------------------------------------------------------------------------------------
# 8. Average quantity by sex and dose
# ---------------------------------------------------------------------------------------------------------
obj1_quantity_by_sex <- codeine_all_years %>%
  filter(!is.na(sex_label), !is.na(quantity), sex_label %in% c("Male", "Female")) %>%
  group_by(sex_label, high_codeine_dose_f) %>%
  summarise(
    avg_quantity = mean(quantity, na.rm = TRUE),
    .groups = "drop"
  )





# ---------------------------------------------------------------------------------------------------------
# 10. Average quantity by age group and dose
# ---------------------------------------------------------------------------------------------------------
obj1_quantity_by_age <- codeine_all_years %>%
  filter(!is.na(age_group), !is.na(quantity)) %>%
  group_by(age_group, high_codeine_dose_f) %>%
  summarise(
    avg_quantity = mean(quantity, na.rm = TRUE),
    .groups = "drop"
  )


# ---------------------------------------------------------------------------------------------------------
# 12. Average OME by sex and dose
# ---------------------------------------------------------------------------------------------------------
obj1_ome_by_sex <- codeine_all_years %>%
  filter(!is.na(sex_label), !is.na(ome), sex_label %in% c("Male", "Female")) %>%
  group_by(sex_label, high_codeine_dose_f) %>%
  summarise(
    avg_ome = mean(ome, na.rm = TRUE),
    .groups = "drop"
  )




# ---------------------------------------------------------------------------------------------------------
# 14. Average OME by age group and dose
# ---------------------------------------------------------------------------------------------------------
obj1_ome_by_age <- codeine_all_years %>%
  filter(!is.na(age_group), !is.na(ome)) %>%
  group_by(age_group, high_codeine_dose_f) %>%
  summarise(
    avg_ome = mean(ome, na.rm = TRUE),
    .groups = "drop"
  )



# ---------------------------------------------------------------------------------------------------------
# 16. Proportion high vs low dose by sex
# ---------------------------------------------------------------------------------------------------------
obj1_prop_by_sex <- codeine_all_years %>%
  filter(!is.na(sex_label), sex_label %in% c("Male", "Female")) %>%
  count(sex_label, codeine_ranking) %>%
  group_by(sex_label) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()



# ---------------------------------------------------------------------------------------------------------
# 17. Proportion high vs low dose by age group
# ---------------------------------------------------------------------------------------------------------
obj1_prop_by_age <- codeine_all_years %>%
  filter(!is.na(age_group)) %>%
  count(age_group, codeine_ranking) %>%
  group_by(age_group) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()



# =========================================================================================================
# OBJECTIVE 2: 2022 ONLY
# Compare characteristics of low vs high dose users in terms of demographics and co-prescribed medicines
# =========================================================================================================

# Use one row per patient for descriptive user-level plots
obj2_users_2022 <- codeine_2022 %>%
  arrange(indID, desc(high_codeine_dose), desc(codeine_ome)) %>%
  group_by(indID) %>%
  slice(1) %>%
  ungroup()

# ---------------------------------------------------------------------------------------------------------
# 18. Sex distribution by dose group (2022)
# ---------------------------------------------------------------------------------------------------------
obj2_sex_dist <- obj2_users_2022 %>%
  filter(!is.na(sex_label), sex_label %in% c("Male", "Female")) %>%
  count(high_codeine_dose_f, sex_label) %>%
  group_by(high_codeine_dose_f) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()



# ---------------------------------------------------------------------------------------------------------
# 24. Mean co-prescribed medicine flags by dose group (2022)
# ---------------------------------------------------------------------------------------------------------
co_med_cols <- c(
  "Oral_NSAIDs",
  "Topical_analgesics",
  "Gabapentinoids",
  "Benzodiazepines_sedatives",
  "Anti_migraines",
  "Other_Opioids"
)

obj2_comeds <- obj2_users_2022 %>%
  select(high_codeine_dose_f, all_of(co_med_cols)) %>%
  pivot_longer(
    cols = all_of(co_med_cols),
    names_to = "medicine_group",
    values_to = "present"
  ) %>%
  group_by(high_codeine_dose_f, medicine_group) %>%
  summarise(
    prop_present = mean(present == 1, na.rm = TRUE),
    .groups = "drop"
  )


# ---------------------------------------------------------------------------------------------------------
# 26. Any co-prescribed medicine: yes/no by dose group (2022)
# ---------------------------------------------------------------------------------------------------------
obj2_any_comed <- obj2_users_2022 %>%
  mutate(any_other_med = if_else(other_analgesic_y_n == 1, "Yes", "No")) %>%
  count(high_codeine_dose_f, any_other_med) %>%
  group_by(high_codeine_dose_f) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()



# ---------------------------------------------------------------------------------------------------------
# 27. ACB burden category by dose group (2022)
# ---------------------------------------------------------------------------------------------------------
obj2_acb <- obj2_users_2022 %>%
  mutate(
    acb_group = case_when(
      total_acb_score == 0 ~ "ACB = 0",
      total_acb_score %in% c(1, 2) ~ "ACB = 1–2",
      total_acb_score >= 3 ~ "ACB = 3+",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(acb_group)) %>%
  count(high_codeine_dose_f, acb_group) %>%
  group_by(high_codeine_dose_f) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()



# ---------------------------------------------------------------------------------------------------------
# 29. LHO distribution by dose group (2022)
# ---------------------------------------------------------------------------------------------------------
obj2_lho <- obj2_users_2022 %>%
  filter(!is.na(LHO_area)) %>%
  count(high_codeine_dose_f, LHO_area) %>%
  group_by(high_codeine_dose_f) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()



# ---------------------------------------------------------------------------------------------------------
# 30. CHO distribution by dose group (2022)
# ---------------------------------------------------------------------------------------------------------
obj2_cho <- obj2_users_2022 %>%
  filter(!is.na(CHO_area)) %>%
  count(high_codeine_dose_f, CHO_area) %>%
  group_by(high_codeine_dose_f) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()




# ---------------------------------------------------------------------------------------------------------
# Regression analysis 
# ---------------------------------------------------------------------------------------------------------
analysis <- objective_two %>%
  mutate(
    high_codeine_dose = factor(high_codeine_dose, levels = c(0, 1), labels = c("Low", "High")),
    sex = factor(sex, levels = c("M", "F"), labels = c("Male", "Female")),
    age_group = case_when(
      age >= 16 & age <= 24 ~ "16-24",
      age >= 25 & age <= 34 ~ "25-34",
      age >= 35 & age <= 44 ~ "35-44",
      age >= 45 & age <= 54 ~ "45-54",
      age >= 55 & age <= 64 ~ "55-64",
      age >= 65 & age <= 69 ~ "65-69",
      age >= 70 & age <= 74 ~ "70-74",
      age >= 75 ~ "75+"
    ) %>% factor(
      levels = c("16-24", "25-34", "35-44", "45-54", "55-64", "65-69", "70-74", "75+")
    ),
    ach_burden_med = factor(
      ifelse(is.na(ach_burden_med), 0, ach_burden_med),
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    Oral_NSAIDs = factor(
      ifelse(is.na(Oral_NSAIDs), 0, Oral_NSAIDs),
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    Topical_analgesics = factor(
      ifelse(is.na(Topical_analgesics), 0, Topical_analgesics),
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    Benzodiazepines_sedatives = factor(
      ifelse(is.na(Benzodiazepines_sedatives), 0, Benzodiazepines_sedatives),
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    Anti_migraines = factor(
      ifelse(is.na(Anti_migraines), 0, Anti_migraines),
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    Gabapentinoids = factor(
      ifelse(is.na(Gabapentinoids), 0, Gabapentinoids),
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    Other_Opioids = factor(
      ifelse(is.na(Other_Opioids), 0, Other_Opioids),
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

model <- glm(
  high_codeine_dose ~ age_group + sex + ach_burden_med +
    Oral_NSAIDs + Topical_analgesics + Benzodiazepines_sedatives +
    Anti_migraines + Gabapentinoids + Other_Opioids,
  data = analysis,
  family = binomial()
)

# Odds ratios table (raw)
or_results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
or_results

# Clean publication table
tbl_regression(model, exponentiate = TRUE)

plot_data <- or_results %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      "sexFemale" = "Female",
      "age_group25-34" = "Age 25-34",
      "age_group35-44" = "Age 35-44",
      "age_group45-54" = "Age 45-54",
      "age_group55-64" = "Age 55-64",
      "age_group65-69" = "Age 65-69",
      "age_group70-74" = "Age 70-74",
      "age_group75+" = "Age 75+",
      "ach_burden_medYes" = "ACB: Yes",
      "Oral_NSAIDsYes" = "Oral NSAIDs: Yes",
      "Topical_analgesicsYes" = "Topical analgesics: Yes",
      "Benzodiazepines_sedativesYes" = "Benzodiazepines/sedatives: Yes",
      "Anti_migrainesYes" = "Anti-migraines: Yes",
      "GabapentinoidsYes" = "Gabapentinoids: Yes",
      "Other_OpioidsYes" = "Other opioids: Yes"
    ),
    group = case_when(
      term == "Female" ~ "Sex",
      grepl("Age", term) ~ "Age",
      term == "ACB: Yes" ~ "ACB",
      term == "Oral NSAIDs: Yes" ~ "Oral NSAIDs",
      term == "Topical analgesics: Yes" ~ "Topical analgesics",
      term == "Benzodiazepines/sedatives: Yes" ~ "Benzodiazepines/sedatives",
      term == "Anti-migraines: Yes" ~ "Anti-migraines",
      term == "Gabapentinoids: Yes" ~ "Gabapentinoids",
      term == "Other opioids: Yes" ~ "Other opioids"
    ),
    term = factor(
      term,
      levels = rev(c(
        "Female",
        "Age 25-34",
        "Age 35-44",
        "Age 45-54",
        "Age 55-64",
        "Age 65-69",
        "Age 70-74",
        "Age 75+",
        "ACB: Yes",
        "Oral NSAIDs: Yes",
        "Topical analgesics: Yes",
        "Benzodiazepines/sedatives: Yes",
        "Anti-migraines: Yes",
        "Gabapentinoids: Yes",
        "Other opioids: Yes"
      ))
    ),
    or_label = sprintf("%.2f", estimate),
    ci_label = sprintf("%.2f–%.2f", conf.low, conf.high)
  )


# ---------------------------------------------------------------------------------------------------------
# Create yearly other analgesic flags
# ---------------------------------------------------------------------------------------------------------

other_analgesic_flags_all_years <- df %>%
  mutate(
    dateofdispensing = ymd(dateofdispensing),
    year = year(dateofdispensing),
    atccode = as.character(atccode)
  ) %>%
  filter(year >= 2014, year <= 2022) %>%
  group_by(indID, year) %>%
  summarise(
    other_analgesic_y_n = as.integer(any(atccode %in% c(
      oral_nsaids_codes,
      topical_analgesics_codes,
      gabapentinoids_codes,
      benzodiazepines_sedatives_codes,
      anti_migraine_codes,
      Other_Opioids_codes
    ))),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------------------------------------
# Create yearly ACB flags
# ---------------------------------------------------------------------------------------------------------

ach_flags_all_years <- df %>%
  mutate(
    dateofdispensing = ymd(dateofdispensing),
    year = year(dateofdispensing),
    atccode = as.character(atccode)
  ) %>%
  filter(
    year >= 2014, year <= 2022,
    atccode %in% all_ach_codes
  ) %>%
  distinct(indID, year, atccode) %>%
  group_by(indID, year) %>%
  summarise(
    total_ach_meds = n(),
    ach_burden_med = as.integer(total_ach_meds > 0),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------------------------------------
# Build dataset
# ---------------------------------------------------------------------------------------------------------

analysis_all_years <- df %>%
  mutate(
    dateofdispensing = ymd(dateofdispensing),
    year = year(dateofdispensing)
  ) %>%
  filter(
    codeine == TRUE,
    year >= 2014, year <= 2022,
    !is.na(codeine_ranking),
    codeine_ranking %in% c("High", "Low"),
    !is.na(age),
    !is.na(sex)
  ) %>%
  transmute(
    indID,
    year,
    age,
    sex,
    high_codeine_dose = if_else(codeine_ranking == "High", 1L, 0L)
  ) %>%
  left_join(other_analgesic_flags_all_years, by = c("indID", "year")) %>%
  left_join(ach_flags_all_years, by = c("indID", "year")) %>%
  mutate(
    other_analgesic_y_n = replace_na(other_analgesic_y_n, 0L),
    ach_burden_med = replace_na(ach_burden_med, 0L),
    sex = factor(sex, levels = c("M", "F"), labels = c("Male", "Female")),
    high_codeine_dose = factor(high_codeine_dose, levels = c(0, 1), labels = c("Low", "High")),
    ach_burden_med = factor(ach_burden_med, levels = c(0, 1), labels = c("No", "Yes")),
    other_analgesic_y_n = factor(other_analgesic_y_n, levels = c(0, 1), labels = c("No", "Yes")),
    age_10 = age / 10
  )

# ---------------------------------------------------------------------------------------------------------
# Run model per year
# ---------------------------------------------------------------------------------------------------------

results_by_year <- analysis_all_years %>%
  group_by(year) %>%
  group_split() %>%
  map_df(function(data_year) {
    
    current_year <- unique(data_year$year)
    
    model <- glm(
      high_codeine_dose ~ age_10 + sex + ach_burden_med + other_analgesic_y_n,
      data = data_year,
      family = binomial()
    )
    
    tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term %in% c("sexFemale", "age_10", "ach_burden_medYes", "other_analgesic_y_nYes")) %>%
      mutate(year = current_year)
  })

# ---------------------------------------------------------------------------------------------------------
# Prepare plotting data
# ---------------------------------------------------------------------------------------------------------

plot_data2 <- results_by_year %>%
  mutate(
    variable = recode(
      term,
      "sexFemale" = "Female",
      "age_10" = "Age (per 10 years)",
      "ach_burden_medYes" = "ACB: Yes",
      "other_analgesic_y_nYes" = "Other analgesics: Yes"
    ),
    year = factor(year, levels = 2014:2022),
    variable = factor(
      variable,
      levels = rev(c(
        "Female",
        "Age (per 10 years)",
        "ACB: Yes",
        "Other analgesics: Yes"
      ))
    )
  )

# ---------------------------------------------------------------------------------------------------------
# Plot (colour by year)
# ---------------------------------------------------------------------------------------------------------

pd <- position_dodge(width = 0.5)



#### Graphing 
# =================================================================================================================================================================================
# Graphing Codeine OME's per month
# =================================================================================================================================================================================
p6<- ggplot(monthly_ome_codeine, aes(x = month, y = normrx)) +
  geom_line() +
  labs(
    title = "Total Codeine OME Over Time",
    x = "Month",
    y = "Total OME"
  ) +
  theme_minimal()

print(p6)

ome_over_time <- ggplot(monthly_ome_codeine, aes(x = month, y = total_ome)) +
  geom_col(fill = "blue", width = 25) + # clean blue
  geom_smooth(color = "red", linewidth = 1.2) + #can also use geom_line for a direct overlay#
  scale_y_continuous(labels = comma) +       # remove scientific notation
  labs(
    title = "Average Codeine OME per Prescription Over Time",
    x = "Month",
    y = "Average OME per Prescription"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
print(ome_over_time)



ranking_graph <- ggplot(monthly_total_ome_by_dose,
                        aes(x = month, y = total_ome, color = codeine_ranking, group = codeine_ranking)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Total Codeine OME per Month: High vs Low Dose",
    x = "Month",
    y = "Total OME",
    color = "Codeine Dose"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
print(ranking_graph)


ome_by_sex_plot <- ggplot(ome_by_sex, aes(x = sex, y = avg_ome, fill = codeine_ranking)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(
    title = "Average Codeine OME per Prescription by Sex and Dose",
    x = "Sex",
    y = "Average OME",
    fill = "Codeine Dose"
  ) +
  theme_minimal(base_size = 14)

print(ome_by_sex_plot)

ome_by_product <- ggplot(ome_by_med, aes(x = reorder(medicationname, avg_ome), y = avg_ome)) +
  geom_col(fill = "blue", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Average Codeine OME per Prescription by Product",
    x = "Medication",
    y = "Average OME"
  ) +
  theme_minimal(base_size = 14)
print(ome_by_product)

ome_by_gp <- ggplot(ome_by_gp,
                    aes(x = reorder(gpidentifiernumber, avg_ome),
                        y = avg_ome,
                        fill = codeine_ranking)) +
  geom_col(position = "dodge", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Average Codeine OME per Prescription by GP (Top 20)",
    x = "GP Identifier",
    y = "Average OME",
    fill = "Codeine Dose"
  ) +
  theme_minimal(base_size = 14)

print(ome_by_gp)


# Stacked bar chart
ome_by_sex_barchart <- ggplot(ome_by_sex_dose, aes(x = codeine_ranking, y = total_ome, fill = sex)) +
  geom_col(width = 0.6) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Total Codeine OME by Dose Category and Sex in 2022",
    x = "Codeine Dose Category",
    y = "Total OME",
    fill = "Sex"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(ome_by_sex_barchart)


#normalised
# Stacked proportional bar chart
proportional_sex_bar <- ggplot(ome_by_sex_dose,
                               aes(x = codeine_ranking, y = total_ome, fill = sex)) +
  geom_col(position = "fill", width = 0.6) +
  labs(
    title = "Proportion of Codeine OME by Dose Category and Sex in 2022",
    x = "Codeine Dose Category",
    y = "Proportion of Total OME",
    fill = "Sex"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(proportional_sex_bar)

violin_box_plot <- ggplot(boxplot_data,
                          aes(x = codeine_ranking, y = age, fill = sex)) +
  geom_violin(alpha = 0.4, trim = FALSE, scale = "width",
              position = position_dodge(width = 1)) +
  geom_boxplot(width = 0.2, outlier.alpha = 0.3,
               position = position_dodge(width = 1)) +
  labs(
    title = "Distribution of Age by Codeine Dose and Sex in 2022",
    x = "Codeine Dose",
    y = "Age",
    fill = "Sex"
  ) +
  scale_fill_manual(values = c(
    "M" = "blue",
    "F" = "red"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(violin_box_plot)


ome_by_age_graph <- ggplot(
  ome_by_ageband_codeine,
  aes(x = age_band, y = avg_ome, fill = sex)
) +
  geom_col() +
  facet_wrap(~sex, ncol = 1) +
  scale_fill_manual(values = c("M" = "blue", "F" = "red")) +
  labs(
    title = "Average OME by Age Band (18–80 in 10-year brackets, 80–110 in 5-year brackets) — Codeine only",
    x = "Age band (years)",
    y = "Average OME",
    fill = "Sex"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(ome_by_age_graph)

# 
# p <- ggplot(
#   obj1_ome_over_time,
#   aes(
#     x = month,
#     y = avg_ome,
#     color = sex,
#     linetype = codeine_ranking,
#     group = interaction(sex, codeine_ranking)
#   )
# ) +
#   geom_smooth(se = FALSE, linewidth = 1.2) +
#   scale_color_manual(
#     values = c("M" = "blue", "F" = "red"),
#     labels = c("M" = "Male", "F" = "Female"),
#     name = "Sex"
#   ) +
#   scale_linetype_manual(
#     values = c("High" = "solid", "Low" = "longdash"),
#     labels = c("High" = "High dose",
#                "Low" = "Low dose"),
#     name = "Dose level"
#   ) +
#   guides(
#     linetype = guide_legend(override.aes = list(linewidth = 2)),
#     color = guide_legend(override.aes = list(linewidth = 2))
#   ) +
#   labs(
#     title = "Average OME Over Time (Codeine Prescriptions Only)",
#     x = "Month",
#     y = "Average OME"
#   ) +
#   theme_minimal()
# 
# print(p)


# Plot
avg_quantity_sex_graph <- ggplot(avg_quantity_sex_dose, aes(x = sex, y = avg_quantity, fill = high_codeine_dose)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Average Quantity of Codeine Prescriptions by Sex and Dose (All Years)",
    x = "Sex",
    y = "Average Quantity",
    fill = "Codeine Dose"
  ) +
  theme_minimal(base_size = 13)

print(avg_quantity_sex_graph)

# Plot
avg_quantity_age_graph <- ggplot(avg_quantity_age_dose, aes(x = age_group, y = avg_quantity, fill = high_codeine_dose)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Average Quantity of Codeine Prescriptions by Age Group and Dose",
    x = "Age Group",
    y = "Average Quantity",
    fill = "Codeine Dose"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(avg_quantity_age_graph)


age_boxplot_dose <- ggplot(boxplot_age_dose, aes(x = age_group, y = quantity, fill = high_codeine_dose)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.3) +
  labs(
    title = "Distribution of Codeine Quantity by Age Group and Dose",
    x = "Age Group",
    y = "Quantity",
    fill = "Codeine Dose"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(age_boxplot_dose)


ome_by_LHO_area_plot <- ggplot(ome_by_LHO_area, aes(x = reorder(LHO_area, total_ome), y = total_ome)) +
  geom_col(fill = "blue", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Total OME by LHO_area (Codeine only)",
    x = "LHO_area",
    y = "Total Codeine OME"
  ) +
  theme_minimal(base_size = 14)

print(ome_by_LHO_area_plot)


pie_chart <- ggplot(ome_by_LHO_area_pi, aes(x = "", y = prop_ome, fill = LHO_area)) +
  geom_col(width = 1, colour = "black", linewidth = 0.6) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  scale_fill_manual(values = rainbow(length(unique(ome_by_LHO_area_pi$LHO_area)))) +
  labs(
    title = "Proportion of Total OME by LHO area (Codeine only)",
    fill = "LHO"
  ) +
  theme_void()

print(pie_chart)

pie_other_analgesics <- ggplot(pie_data, aes(x = "", y = prop, fill = analgesic_type)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  facet_wrap(~codeine_group) +
  labs(
    title = "Most Commonly Prescribed Other Analgesics by Codeine Dose Group",
    fill = "Other Analgesic"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right"
  )

print(pie_other_analgesics)



pie_other_analgesics <- ggplot(pie_data, aes(x = "", y = prop, fill = analgesic_type)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  facet_wrap(~codeine_group) +
  labs(
    title = "Most Commonly Prescribed Other Analgesics by Codeine Dose Group",
    fill = "Other Analgesic"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right"
  )

print(pie_other_analgesics)

achb_porp <-ggplot(ach_plot_data, aes(x = codeine_group, fill = ach_burden)) +
  geom_bar(position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportional ACH Burden in High- vs Low-Dose Codeine Users",
    x = "Codeine dose group",
    y = "Proportion of users",
    fill = "ACH burden"
  ) +
  theme_minimal(base_size = 14)

print(achb_porp)

pie_one <- ggplot(pie_high_codeine, aes(x = "", y = value, fill = category)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5
  ) +
  labs(
    title = "Average High-Dose Codeine as a Proportion of All Codeine Prescribing Across GPs",
    fill = ""
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

print(pie_one)


pie_two <- ggplot(pie_any_codeine, aes(x = "", y = value, fill = category)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5
  ) +
  labs(
    title = "Average Any-Dose Codeine as a Proportion of All Analgesic Prescribing Across GPs",
    fill = ""
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )

print(pie_two)


g1_dispensing_over_time <- ggplot(
  obj1_dispensing_over_time,
  aes(x = month, y = n_prescriptions, color = codeine_ranking)
) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Monthly Number of Codeine Prescriptions Over Time",
    x = "Month",
    y = "Number of prescriptions",
    color = "Dose group"
  ) +  
  geom_line(alpha=0.3, linewidth = 1) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Monthly Number of Codeine Prescriptions Over Time",
    x = "Month",
    y = "Number of prescriptions",
    color = "Dose group"
  ) +  
  coord_cartesian(ylim=c(0, 50000))+
  theme_minimal()

print(g1_dispensing_over_time)

g2_people_over_time <- ggplot(
  obj1_people_over_time,
  aes(x = month, y = n_people, color = codeine_ranking)
) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Monthly Number of People Receiving Codeine Over Time",
    x = "Month",
    y = "Number of unique people",
    color = "Dose group"
  ) +
  geom_line(alpha=0.3, linewidth = 1) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Monthly Number of People Receiving Codeine Over Time",
    x = "Month",
    y = "Number of unique people",
    color = "Dose group"
  ) +
  coord_cartesian(ylim=c(0, 45000))+
  theme_minimal()

print(g2_people_over_time)

g3_quantity_over_time <- ggplot(
  obj1_quantity_over_time,
  aes(x = month, y = avg_quantity, color = codeine_ranking)
) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Average Quantity of Codeine Prescriptions Over Time",
    x = "Month",
    y = "Average quantity",
    color = "Dose group"
  ) +
  geom_line(alpha = 0.3, linewidth = 1) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Average Quantity of Codeine Prescriptions Over Time",
    x = "Month",
    y = "Average quantity",
    color = "Dose group"
  ) +
  coord_cartesian(ylim=c(0, 90))+
  theme_minimal()

print(g3_quantity_over_time)


g4_total_quantity_over_time <- ggplot(
  obj1_total_quantity_over_time,
  aes(x = month, y = total_quantity, color = codeine_ranking)
) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Total Quantity of Codeine Dispensed Over Time",
    x = "Month",
    y = "Total quantity",
    color = "Dose group"
  ) +
  geom_line(alpha=0.3, linewidth = 1) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Total Quantity of Codeine Dispensed Over Time",
    x = "Month",
    y = "Total quantity",
    color = "Dose group"
  ) +
  coord_cartesian(ylim=c(0, 4000000))+
  theme_minimal()

print(g4_total_quantity_over_time)

g5_ome_over_time <- ggplot(
  obj1_ome_over_time,
  aes(x = month, y = avg_ome, color = codeine_ranking)
) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Average Codeine OME Over Time",
    x = "Month",
    y = "Average OME",
    color = "Dose group"
  ) +
  geom_line(alpha=0.3, linewidth = 1) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Average Codeine OME Over Time",
    x = "Month",
    y = "Average OME",
    color = "Dose group"
  ) +
  coord_cartesian(ylim=c(0, 250))+
  theme_minimal()

print(g5_ome_over_time)

g6_total_ome_over_time <- ggplot(
  obj1_total_ome_over_time,
  aes(x = month, y = total_ome, color = codeine_ranking)
) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Total Codeine OME Over Time",
    x = "Month",
    y = "Total OME",
    color = "Dose group"
  ) + 
  geom_line(alpha=0.3, linewidth = 1) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Total Codeine OME Over Time",
    x = "Month",
    y = "Total OME",
    color = "Dose group"
  ) + 
  coord_cartesian(ylim=c(0, 8000000))+
  theme_minimal()

print(g6_total_ome_over_time)

g6_total_ome_over_time <- ggplot(
  obj1_total_ome_over_time,
  aes(x = month, y = total_ome, color = codeine_ranking)
) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Total Codeine OME Over Time",
    x = "Month",
    y = "Total OME",
    color = "Dose group"
  ) +
  theme_minimal()

print(g6_total_ome_over_time)

g7_prop_high_over_time <- ggplot(
  obj1_prop_high_over_time,
  aes(x = month, y = prop_high)
) +
  geom_smooth(se = FALSE, color = "darkred", linewidth = 1.2) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Proportion of Codeine Prescriptions That Are High Dose Over Time",
    x = "Month",
    y = "Proportion high dose"
  ) +
  geom_line(color = "darkred",alpha=0.4, linewidth = 1) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Proportion of Codeine Prescriptions That Are High Dose Over Time",
    x = "Month",
    y = "Proportion high dose"
  ) +
  coord_cartesian(ylim=c(0, 0.48))+
  theme_minimal()

print(g7_prop_high_over_time)

g8_quantity_by_sex <- ggplot(
  obj1_quantity_by_sex,
  aes(x = sex_label, y = avg_quantity, fill = high_codeine_dose_f)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Average Quantity by Sex and Codeine Dose",
    x = "Sex",
    y = "Average quantity",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g8_quantity_by_sex)

# ---------------------------------------------------------------------------------------------------------
# 9. Quantity distribution by sex and dose
# ---------------------------------------------------------------------------------------------------------
g9_quantity_boxplot_sex <- ggplot(
  codeine_all_years %>%
    filter(!is.na(sex_label), !is.na(quantity), sex_label %in% c("Male", "Female")),
  aes(x = sex_label, y = quantity, fill = high_codeine_dose_f)
) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.25) +
  labs(
    title = "Distribution of Quantity by Sex and Codeine Dose",
    x = "Sex",
    y = "Quantity",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g9_quantity_boxplot_sex)

g10_quantity_by_age <- ggplot(
  obj1_quantity_by_age,
  aes(x = age_group, y = avg_quantity, fill = high_codeine_dose_f)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Average Quantity by Age Group and Codeine Dose",
    x = "Age group",
    y = "Average quantity",
    fill = "Dose group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g10_quantity_by_age)

g10_quantity_by_age <- ggplot(
  obj1_quantity_by_age,
  aes(x = age_group, y = avg_quantity, fill = high_codeine_dose_f)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Average Quantity by Age Group and Codeine Dose",
    x = "Age group",
    y = "Average quantity",
    fill = "Dose group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g10_quantity_by_age)

g10_quantity_by_age <- ggplot(
  obj1_quantity_by_age,
  aes(x = age_group, y = avg_quantity, fill = high_codeine_dose_f)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Average Quantity by Age Group and Codeine Dose",
    x = "Age group",
    y = "Average quantity",
    fill = "Dose group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g10_quantity_by_age)

# ---------------------------------------------------------------------------------------------------------
# 11. Quantity distribution by age group and dose
# ---------------------------------------------------------------------------------------------------------
g11_quantity_boxplot_age <- ggplot(
  codeine_all_years %>%
    filter(!is.na(age_group), !is.na(quantity)),
  aes(x = age_group, y = quantity, fill = high_codeine_dose_f)
) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.25) +
  labs(
    title = "Distribution of Quantity by Age Group and Codeine Dose",
    x = "Age group",
    y = "Quantity",
    fill = "Dose group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g11_quantity_boxplot_age)


g12_ome_by_sex <- ggplot(
  obj1_ome_by_sex,
  aes(x = sex_label, y = avg_ome, fill = high_codeine_dose_f)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Average OME by Sex and Codeine Dose",
    x = "Sex",
    y = "Average OME",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g12_ome_by_sex)

# ---------------------------------------------------------------------------------------------------------
# 13. OME distribution by sex and dose
# ---------------------------------------------------------------------------------------------------------
g13_ome_boxplot_sex <- ggplot(
  codeine_all_years %>%
    filter(!is.na(sex_label), !is.na(ome), sex_label %in% c("Male", "Female")),
  aes(x = sex_label, y = ome, fill = high_codeine_dose_f)
) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.25) +
  labs(
    title = "Distribution of OME by Sex and Codeine Dose",
    x = "Sex",
    y = "OME",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g13_ome_boxplot_sex)


g14_ome_by_age <- ggplot(
  obj1_ome_by_age,
  aes(x = age_group, y = avg_ome, fill = high_codeine_dose_f)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  labs(
    title = "Average OME by Age Group and Codeine Dose",
    x = "Age group",
    y = "Average OME",
    fill = "Dose group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g14_ome_by_age)

# ---------------------------------------------------------------------------------------------------------
# 15. OME distribution by age group and dose
# ---------------------------------------------------------------------------------------------------------
g15_ome_boxplot_age <- ggplot(
  codeine_all_years %>%
    filter(!is.na(age_group), !is.na(ome)),
  aes(x = age_group, y = ome, fill = high_codeine_dose_f)
) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.25) +
  labs(
    title = "Distribution of OME by Age Group and Codeine Dose",
    x = "Age group",
    y = "OME",
    fill = "Dose group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g15_ome_boxplot_age)

g16_prop_by_sex <- ggplot(
  obj1_prop_by_sex,
  aes(x = sex_label, y = prop, fill = codeine_ranking)
) +
  geom_col(position = "fill", width = 0.6) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Proportion of High- and Low-Dose Codeine by Sex",
    x = "Sex",
    y = "Proportion",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g16_prop_by_sex)


g17_prop_by_age <- ggplot(
  obj1_prop_by_age,
  aes(x = age_group, y = prop, fill = codeine_ranking)
) +
  geom_col(position = "fill", width = 0.7) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("Low" = "blue", "High" = "red")) +
  labs(
    title = "Proportion of High- and Low-Dose Codeine by Age Group",
    x = "Age group",
    y = "Proportion",
    fill = "Dose group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g17_prop_by_age)

g18_sex_distribution_2022 <- ggplot(
  obj2_sex_dist,
  aes(x = high_codeine_dose_f, y = prop, fill = sex_label)
) +
  geom_col(position = "fill", width = 0.6) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Sex Distribution by Codeine Dose Group in 2022",
    x = "Dose group",
    y = "Proportion",
    fill = "Sex"
  ) +
  theme_minimal()

print(g18_sex_distribution_2022)


# ---------------------------------------------------------------------------------------------------------
# 19. Age distribution by dose group (2022)
# ---------------------------------------------------------------------------------------------------------
g19_age_distribution_2022 <- ggplot(
  obj2_users_2022 %>% filter(!is.na(age)),
  aes(x = high_codeine_dose_f, y = age, fill = high_codeine_dose_f)
) +
  geom_boxplot(outlier.alpha = 0.25) +
  labs(
    title = "Age Distribution by Codeine Dose Group in 2022",
    x = "Dose group",
    y = "Age",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g19_age_distribution_2022)

# ---------------------------------------------------------------------------------------------------------
# 20. Age distribution by dose group and sex (2022)
# ---------------------------------------------------------------------------------------------------------
g20_age_violin_2022 <- ggplot(
  obj2_users_2022 %>% filter(!is.na(age), !is.na(sex_label), sex_label %in% c("Male", "Female")),
  aes(x = high_codeine_dose_f, y = age, fill = sex_label)
) +
  geom_violin(alpha = 0.35, trim = FALSE, position = position_dodge(width = 0.9)) +
  geom_boxplot(width = 0.2, outlier.alpha = 0.2, position = position_dodge(width = 0.9)) +
  labs(
    title = "Age Distribution by Dose Group and Sex in 2022",
    x = "Dose group",
    y = "Age",
    fill = "Sex"
  ) +
  theme_minimal()

print(g20_age_violin_2022)



# ---------------------------------------------------------------------------------------------------------
# 21. Age-group composition by dose group (2022)
# ---------------------------------------------------------------------------------------------------------
obj2_age_comp <- obj2_users_2022 %>%
  filter(!is.na(age_group)) %>%
  count(high_codeine_dose_f, age_group) %>%
  group_by(high_codeine_dose_f) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

g21_age_group_composition_2022 <- ggplot(
  obj2_age_comp,
  aes(x = high_codeine_dose_f, y = prop, fill = age_group)
) +
  geom_col(position = "fill", width = 0.65) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Age-Group Composition by Codeine Dose Group in 2022",
    x = "Dose group",
    y = "Proportion",
    fill = "Age group"
  ) +
  theme_minimal()

print(g21_age_group_composition_2022)

# ---------------------------------------------------------------------------------------------------------
# 22. Quantity distribution by dose group (2022)
# ---------------------------------------------------------------------------------------------------------
g22_quantity_boxplot_2022 <- ggplot(
  codeine_2022 %>% filter(!is.na(quantity)),
  aes(x = high_codeine_dose_f, y = quantity, fill = high_codeine_dose_f)
) +
  geom_boxplot(outlier.alpha = 0.25) +
  labs(
    title = "Quantity Distribution by Codeine Dose Group in 2022",
    x = "Dose group",
    y = "Quantity",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g22_quantity_boxplot_2022)

# ---------------------------------------------------------------------------------------------------------
# 23. Codeine-specific OME distribution by dose group (2022)
# ---------------------------------------------------------------------------------------------------------
g23_codeine_ome_boxplot_2022 <- ggplot(
  codeine_2022 %>% filter(!is.na(codeine_ome)),
  aes(x = high_codeine_dose_f, y = codeine_ome, fill = high_codeine_dose_f)
) +
  geom_boxplot(outlier.alpha = 0.25) +
  labs(
    title = "Codeine OME Distribution by Dose Group in 2022",
    x = "Dose group",
    y = "Codeine OME",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g23_codeine_ome_boxplot_2022)


g24_comed_bar_2022 <- ggplot(
  obj2_comeds,
  aes(x = fct_reorder(medicine_group, prop_present), y = prop_present, fill = high_codeine_dose_f)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Co-Prescribed Medicine Groups by Codeine Dose Group in 2022",
    x = "Medicine group",
    y = "Proportion of users",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g24_comed_bar_2022)

# ---------------------------------------------------------------------------------------------------------
# 25. Number of co-prescribed medicine groups per user (2022)
# ---------------------------------------------------------------------------------------------------------
g25_other_analgesic_total_2022 <- ggplot(
  obj2_users_2022 %>% filter(!is.na(other_analgesic_total)),
  aes(x = high_codeine_dose_f, y = other_analgesic_total, fill = high_codeine_dose_f)
) +
  geom_boxplot(outlier.alpha = 0.25) +
  labs(
    title = "Number of Co-Prescribed Medicine Groups by Codeine Dose Group in 2022",
    x = "Dose group",
    y = "Number of co-prescribed medicine groups",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g25_other_analgesic_total_2022)

g26_any_comed_2022 <- ggplot(
  obj2_any_comed,
  aes(x = high_codeine_dose_f, y = prop, fill = any_other_med)
) +
  geom_col(position = "fill", width = 0.6) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Any Co-Prescribed Medicine by Codeine Dose Group in 2022",
    x = "Dose group",
    y = "Proportion",
    fill = "Any co-prescribed medicine"
  ) +
  theme_minimal()

print(g26_any_comed_2022)

g27_acb_2022 <- ggplot(
  obj2_acb,
  aes(x = high_codeine_dose_f, y = prop, fill = acb_group)
) +
  geom_col(position = "fill", width = 0.6) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Anticholinergic Burden by Codeine Dose Group in 2022",
    x = "Dose group",
    y = "Proportion",
    fill = "ACB category"
  ) +
  theme_minimal()

print(g27_acb_2022)

# ---------------------------------------------------------------------------------------------------------
# 28. Total ACB score distribution by dose group (2022)
# ---------------------------------------------------------------------------------------------------------
g28_acb_score_boxplot_2022 <- ggplot(
  obj2_users_2022 %>% filter(!is.na(total_acb_score)),
  aes(x = high_codeine_dose_f, y = total_acb_score, fill = high_codeine_dose_f)
) +
  geom_boxplot(outlier.alpha = 0.25) +
  labs(
    title = "Total ACB Score by Codeine Dose Group in 2022",
    x = "Dose group",
    y = "Total ACB score",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g28_acb_score_boxplot_2022)

g29_lho_distribution_2022 <- ggplot(
  obj2_lho,
  aes(x = fct_reorder(LHO_area, prop), y = prop, fill = high_codeine_dose_f)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(
    title = "LHO Distribution by Codeine Dose Group in 2022",
    x = "LHO area",
    y = "Proportion of users",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g29_lho_distribution_2022)

g30_cho_distribution_2022 <- ggplot(
  obj2_cho,
  aes(x = CHO_area, y = prop, fill = high_codeine_dose_f)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "CHO Distribution by Codeine Dose Group in 2022",
    x = "CHO area",
    y = "Proportion of users",
    fill = "Dose group"
  ) +
  theme_minimal()

print(g30_cho_distribution_2022)


or_plot1 <- ggplot(plot_data, aes(x = estimate, y = term, colour = group)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.6, colour = "black") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 1) +
  geom_point(size = 3) +
#  annotation_logticks(sides = "b") +
#  scale_x_log10() +
  scale_colour_manual(values = c(
    "Sex" = "#1b9e77",
    "Age" = "#7570b3",
    "ACB" = "#d95f02",
    "Oral NSAIDs" = "#66a61e",
    "Topical analgesics" = "#e7298a",
    "Benzodiazepines/sedatives" = "#e6ab02",
    "Anti-migraines" = "#a6761d",
    "Gabapentinoids" = "#a6769d",
    "Other opioids" = "#666666"
  )) +
  labs(
    title = "Adjusted Odds of High-Dose Codeine Prescribing (2022)",
    x = "Odds Ratio",
    y = NULL,
    colour = "Variable"
  ) +
  coord_cartesian(xlim = c(0.7, 1.6)) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(or_plot1)

cc <- scales::seq_gradient_pal("#e6ab02", "#7F0000", "Lab")(seq(0,1,length.out=9))
forest_plot_clean <- ggplot(
  plot_data2,
  aes(
    x = estimate,
    y = variable,
    colour = year,
    group = year
  )
) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.7) +

  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    linewidth = 1,
    position = pd
    ) +

  geom_point(
    size = 3,
    position = pd
  ) +
  
  scale_colour_manual(values=cc  
  ) +
  
  coord_cartesian(xlim = c(0.8, 1.5)) +
  
  labs(
    title = "Adjusted Odds of High-Dose Codeine Prescribing by Year",
    subtitle = "Age shown as odds ratio per 10-year increase",
    x = "Odds Ratio",
    y = "Factor",
    colour = "Year"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) 

print(forest_plot_clean)








# ================================================================================================================
# CHOROPLETH MAP: PROPORTION OF HIGH-DOSE CODEINE DISPENSING OVER ALL DISPENSING
# ================================================================================================================

library(sf)
library(RColorBrewer)

# ------------------------------------------------------------------------------------------------
# STEP 1: CREATE 2022 LHO SUMMARY TABLE
# ------------------------------------------------------------------------------------------------

objective_three_lho_table_2022 <- df %>%
  filter(year(dateofdispensing) == 2022) %>%
  group_by(LHO_area, CHO_area) %>%
  summarise(
    total_dispensings = n(),
    codeine_dispensings = sum(codeine == TRUE, na.rm = TRUE),
    high_codeine_dispensings = sum(codeine == TRUE & codeine_ranking == "High", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_high_of_all_codeine = if_else(
      codeine_dispensings > 0,
      100 * high_codeine_dispensings / codeine_dispensings,
      NA_real_
    )
  )

# ------------------------------------------------------------------------------------------------
# STEP 2: READ AND CLEAN GEOJSON
# ------------------------------------------------------------------------------------------------

lho_geo <- st_read("C:/Users/frankmoriarty/OneDrive - Royal College of Surgeons in Ireland/PCRS data/LHO georef.geojson", quiet = TRUE)

lho_geo <- lho_geo %>%
  mutate(
    LHO_area = case_when(
      LHO == "Tipperary South" ~ "South Tipperary",
      LHO == "Tipperary North/East Limerick" ~ "North Tipperary/East Limerick",
      TRUE ~ as.character(LHO)
    )
  ) %>%
  filter(!is.na(LHO_area))

# Dissolve to one geometry per LHO
lho_geo_clean <- lho_geo %>%
  group_by(LHO_area) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# ------------------------------------------------------------------------------------------------
# STEP 3: JOIN DATA TO MAP
# ------------------------------------------------------------------------------------------------

lho_map_data <- lho_geo_clean %>%
  left_join(objective_three_lho_table_2022, by = "LHO_area")

# Optional check (very useful)
print(
  lho_map_data %>%
    st_drop_geometry() %>%
    select(LHO_area, codeine_dispensings, high_codeine_dispensings, pct_high_of_all_codeine) %>%
    arrange(desc(pct_high_of_all_codeine))
)

# ------------------------------------------------------------------------------------------------
# STEP 4: GRAPH GALLERY STYLE MAP (BASE R)
# ------------------------------------------------------------------------------------------------

# Create colour palette
my_colors <- brewer.pal(9, "Reds")
my_colors <- colorRampPalette(my_colors)(30)

# Bin the data
class_of_lho <- cut(lho_map_data$pct_high_of_all_codeine, 30)

# Assign colours
map_colors <- my_colors[as.numeric(class_of_lho)]

# Plot
plot(
  st_geometry(lho_map_data),
  col = map_colors,
  border = "white",
  lwd = 0.7,
  main = "High-Dose Codeine as % of All Codeine Dispensing (2022)\nLHO Level"
)

# ------------------------------------------------------------------------------------------------
# STEP 5: GGPLOT VERSION (RECOMMENDED FOR REPORTS)
# ------------------------------------------------------------------------------------------------

high_codeine_map <- ggplot(lho_map_data) +
  geom_sf(aes(fill = pct_high_of_all_codeine), colour = "white", linewidth = 0.3) +
  scale_fill_distiller(
    palette = "Reds",
    direction = 1,
 #   limits = c(0, 100),   # 🔥 FORCE 0–100 scale
    na.value = "grey90",
    name = "% High-dose\nof codeine (2022)"
  )+
  labs(
    title = "Proportion of High-Dose Codeine Dispensing (2022)",
    subtitle = "As % of all codeine dispensing (High + Low)\nLHO level"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

print(high_codeine_map)

# ================================================================================================================
# 100-SQUARE GP TILE PLOT (2022) — MATCHED CHOROPLETH COLOUR SCHEME
# Updated bands: 0–24, 25–49, 50–74, 75–100
# ================================================================================================================

gp_codeine_2022 <- df %>%
  filter(
    year(dateofdispensing) == 2022,
    !is.na(gpidentifiernumber),
    codeine == TRUE,
    !is.na(codeine_ranking),
    codeine_ranking %in% c("High", "Low")
  ) %>%
  group_by(gpidentifiernumber) %>%
  summarise(
    total_codeine = n(),
    high_codeine = sum(codeine_ranking == "High", na.rm = TRUE),
    prop_high = 100 * high_codeine / total_codeine,
    .groups = "drop"
  ) %>%
  arrange(prop_high) %>%
  mutate(
    percentile_group = ntile(prop_high, 100)
  ) %>%
  group_by(percentile_group) %>%
  summarise(
    mean_prop_high = mean(prop_high, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prop_band = case_when(
      mean_prop_high >= 0  & mean_prop_high <= 24  ~ "0–24%",
      mean_prop_high >= 25 & mean_prop_high <= 49  ~ "25–49%",
      mean_prop_high >= 50 & mean_prop_high <= 74  ~ "50–74%",
      mean_prop_high >= 75 & mean_prop_high <= 100 ~ "75–100%",
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------------------------------------------
# CREATE 10x10 GRID
# ------------------------------------------------------------------------------------------------

tile_data <- gp_codeine_2022 %>%
  arrange(percentile_group) %>%
  mutate(
    tile_id = row_number(),
    x = ((tile_id - 1) %% 10) + 1,
    y = 10 - ((tile_id - 1) %/% 10)
  )

# ------------------------------------------------------------------------------------------------
# PLOT (MATCHED TO CHOROPLETH COLOUR SCALE)
# ------------------------------------------------------------------------------------------------

gp_square_plot <- ggplot(tile_data, aes(x = x, y = y, fill = mean_prop_high)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  coord_equal() +
  scale_fill_gradientn(
    colours = c(
      "#FBE4D8",  # light
      "#F4A582",
      "#F46D43",
      "#D73027",
      "#7F0000"   # dark
    ),
    limits = c(0, 100),
    name = "% High-dose\nof codeine"
  ) +
  labs(
    title = "Distribution of High-Dose Codeine Prescribing Across GPs (2022)",
    subtitle = "100 squares = 100 percentile groups (low → high)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

print(gp_square_plot)


# ================================================================================================================
# SCATTER PLOT — TRUE PRACTICE SIZE vs HIGH-DOSE CODEINE (%)
# ================================================================================================================

gp_scatter_2022 <- df %>%
  filter(
    year(dateofdispensing) == 2022,
    codeine == TRUE,
    !is.na(gpidentifiernumber),
    !is.na(codeine_ranking),
    codeine_ranking %in% c("High", "Low"),
    !is.na(indID)
  ) %>%
  group_by(gpidentifiernumber) %>%
  summarise(
    total_codeine_users = n_distinct(indID),
    high_users = n_distinct(indID[codeine_ranking == "High"]),
    pct_high = 100 * high_users / total_codeine_users,
    .groups = "drop"
  ) %>%
  filter(total_codeine_users > 0)
gp_scatter_plot <- ggplot(gp_scatter_2022,
                          aes(x = total_codeine_users, y = pct_high)) +
  geom_jitter(
    width = 0.3, 
    height = 1.5, 
    alpha = 0.5, 
    size = 1.5,
    colour = "black"
  ) +
  # geom_smooth(
  #   method = "lm",
  #   se = FALSE,
  #   colour = "red",
  #   linewidth = 1
  # ) +
  labs(
    title = "High-Dose Codeine Use by GP Practice (2022)",
    subtitle = "Each point represents one GP practice (jittered to reduce overlap)",
    x = "Number of Codeine Users",
    y = "% of Users Receiving High-Dose Codeine"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(gp_scatter_plot)




















rm(ach_plot_data, analysis_all_years, codeine_2022, objective_three, objective_three_annual, other_analgesic_flags_all_years,annual_codeine_sums, annual_other_analgesics, ach_flags_all_years, ach_flags_2022, boxplot_sex_dose, boxplot_age_dose, codeine_all_years, boxplot_data, codeine_products, objective_two, other_analgesic_flags_2022)
gc()
# ================================================================================================================
# OBJECTIVE 3: Explore geographic variation in high-dose codeine use
# ALL YEARS COMBINED - NO YEAR FILTER
# AREA-LEVEL TOTALS AND PERCENTAGES
# ================================================================================================================

# ================================================================================================================
# STEP 1: PREP DATA
# ================================================================================================================

objective_three_amount <- df %>%
  filter(!is.na(LHO_area)) %>%
   subset(select = c(LHO_area,CHO_area, indID, codeine,codeine_ranking)) %>%
mutate(
    is_codeine = if_else(codeine == TRUE, 1L, 0L),
    is_high_codeine = if_else(codeine == TRUE & codeine_ranking == "High", 1L, 0L)
  )

# ================================================================================================================
# STEP 2: LHO-LEVEL RESULTS TABLE (VIEWER)
# ================================================================================================================

objective_three_lho_table <- objective_three_amount %>%
  group_by(LHO_area, CHO_area) %>%
  summarise(
    total_dispensings = n(),
    codeine_dispensings = sum(is_codeine, na.rm = TRUE),
    high_codeine_dispensings = sum(is_high_codeine, na.rm = TRUE),
    pct_codeine_of_all = 100 * codeine_dispensings / total_dispensings,
    pct_high_of_codeine = 100 * high_codeine_dispensings / codeine_dispensings,
    .groups = "drop"
  ) %>%
  mutate(
    pct_codeine_of_all = round(pct_codeine_of_all, 1),
    pct_high_of_codeine = round(pct_high_of_codeine, 1)
  ) %>%
  arrange(desc(pct_high_of_codeine))

# DISPLAY IN VIEWER
objective_three_lho_table %>%
  gt() %>%
  tab_header(
    title = "Geographic Variation in Codeine Prescribing",
    subtitle = "LHO Level (All Years Combined)"
  ) %>%
  cols_label(
    LHO_area = "LHO",
    CHO_area = "CHO",
    total_dispensings = "Total dispensings",
    codeine_dispensings = "Codeine dispensings",
    pct_codeine_of_all = "% Codeine of all dispensing",
    pct_high_of_codeine = "% High-dose of codeine"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 1
  )

# ================================================================================================================
# STEP 3: CHO-LEVEL RESULTS TABLE (VIEWER)
# ================================================================================================================

objective_three_cho_table <- objective_three_amount %>%
  filter(!is.na(CHO_area)) %>%
  group_by(CHO_area) %>%
  summarise(
    total_dispensings = n(),
    codeine_dispensings = sum(is_codeine, na.rm = TRUE),
    high_codeine_dispensings = sum(is_high_codeine, na.rm = TRUE),
    pct_codeine_of_all = 100 * codeine_dispensings / total_dispensings,
    pct_high_of_codeine = 100 * high_codeine_dispensings / codeine_dispensings,
    .groups = "drop"
  ) %>%
  mutate(
    pct_codeine_of_all = round(pct_codeine_of_all, 1),
    pct_high_of_codeine = round(pct_high_of_codeine, 1)
  ) %>%
  arrange(desc(pct_high_of_codeine))
print(objective_three_lho_table)

# DISPLAY IN VIEWER
objective_three_cho_table %>%
  gt() %>%
  tab_header(
    title = "Geographic Variation in Codeine Prescribing",
    subtitle = "CHO Level (All Years Combined)"
  ) %>%
  cols_label(
    CHO_area = "CHO",
    total_dispensings = "Total dispensings",
    codeine_dispensings = "Codeine dispensings",
    pct_codeine_of_all = "% Codeine of all dispensing",
    pct_high_of_codeine = "% High-dose of codeine"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 1
  )
print(objective_three_cho_table)


