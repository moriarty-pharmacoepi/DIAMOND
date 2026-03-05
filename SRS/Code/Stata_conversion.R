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

# =================================================================================================================================================================================
# Load data
# =================================================================================================================================================================================
data_file_path<- "C:/Users/ryanmuddiman/Downloads/sample_data.csv"

analgesic_ind <- read_csv(data_file_path) 

# Preserve equivalent.
subset_dates <- analgesic_ind %>%
  filter(
    (str_detect(atccode, "N06A") & atccode != "N06AA09") |
      str_detect(atccode, "N05") |
      str_detect(atccode, "N03")
  )

analgesic_ind_dates <- analgesic_ind

subset_dates <- bind_rows(subset_dates, analgesic_ind_dates) %>%
  distinct(dateofdispensing, individualidentifiernumber, .keep_all = TRUE)

#write_csv(subset_dates,
#          "~/Desktop/test.csv")

# Restore equivalent
analgesic_ind <- subset_dates %>%
  filter(!(
    (str_detect(atccode, "N06A") & atccode != "N06AA09") |
      str_detect(atccode, "N05") |
      str_detect(atccode, "N03")
  ))

analgesic_ind_dates <- analgesic_ind
analgesic_ind_A <- analgesic_ind_dates

df <- analgesic_ind

df <- df %>%
  rename(indID = individualidentifiernumber)

# =================================================================================================================================================================================
# Create 30 quantiles (xtile)
# =================================================================================================================================================================================
df <- df %>%
  mutate(ID_d = ntile(indID, 30))

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
##---looks good up to here, next line reduces data set to 12 observations----------------------------------------------------------------------------------------------------------
# =================================================================================================================================================================================
# Rolling 30-day OME (asrol equivalent)
# =================================================================================================================================================================================
setDT(df)
setorder(df, indID, dateofdispensing)

df[, sum30_ome := frollsum(ome, n = 30, align = "right"), by = indID]
df[, avg30ome := sum30_ome/30]
df[, high_risk_ome := avg30ome > 90 & opioid == TRUE]

# =================================================================================================================================================================================
# Keep final analytic sample
# =================================================================================================================================================================================
df <- df %>%
  filter(between(dateofdispensing, 19724, 23010))


# =================================================================================================================================================================================
##PADRAIC generating graphs(outdated now i reckon)
# =================================================================================================================================================================================
# 1) Ensure date is a real Date (dd/mm/yyyy)
df <- df %>%
  mutate(
    dateofdispensing = as.Date(dateofdispensing, format = "%d/%m/%Y"),
    ome = as.numeric(ome)  # just in case it's character
  )

# 2) Monthly total OME (exclude NA ome and NA dates)
monthly_ome <- df %>%
  filter(!is.na(.data$ome), !is.na(.data$dateofdispensing)) %>%
  mutate(month = floor_date(.data$dateofdispensing, unit = "month")) %>%
  group_by(month) %>%
  summarise(
    total_ome = sum(.data$ome, na.rm = TRUE),
    .groups = "drop"
  )

# 3) Plot: OME quantity on Y axis
p4<-ggplot(monthly_ome, aes(x = month, y = total_ome)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_number()) +
  theme_minimal()
print(p4)

# ================================================================================================================================================================================
# Hard coding codeine content to drug names
# =================================================================================================================================================================================
   codeine_products <-  df %>%
                      filter(str_detect(atccode, "N02AJ0")) %>%
                        arrange(medicationname) 
 #view(codeine_products)
 
 df <- df %>%
   mutate(codeine_dose = case_when(
     medicationname == "Tylex Caps" ~ 30,
     medicationname == "Solpadeine Caps" ~ 8,
     medicationname == "Solpadol Eff Tabs" ~ 30,
     medicationname == "Solpadeine Sol. Tabs" ~ 8,
     medicationname == "Solpadol Caplets" ~ 30,
     medicationname == "Codipar Caps 15/500 Mg" ~ 15,
     medicationname == "Kapake Tabs 30/500 Mg" ~ 30,
     medicationname == "Maxilief Eff Tabs" ~ 8,
     medicationname == "Kapake Tabs 15/500 Mg" ~ 15,
     medicationname == "Kapake (Imbat Ltd.) Tabs 30/500 Mg" ~ 30,
     medicationname == "Codipar  Eff Tabs 15/500 Mg" ~ 15,
     medicationname == "Tylex Effervescent  Tabs 30/500 Mg" ~ 30,
     
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
 
 # =================================================================================================================================================================================
 # Graphing Codeine OME's per month and subplotting based on HIGH or LOW dose preparations
 # ================================================================================================================================================================================
 monthly_total_ome_by_dose <- df %>%
   filter(!is.na(ome), !is.na(codeine_ranking), !is.na(dateofdispensing)) %>%
   mutate(month = as.Date(format(dateofdispensing, "%Y-%m-01"))) %>%
   group_by(month, codeine_ranking) %>%
   summarise(total_ome = sum(ome, na.rm = TRUE), .groups = "drop")
 
 
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
 # =================================================================================================================================================================================
 # Graphing Codeine OME's by sex
 # =================================================================================================================================================================================
 ome_by_sex <- df %>%
   filter(!is.na(ome)) %>%
   group_by(sex) %>%
   summarise(avg_ome = mean(ome, na.rm = TRUE))
 
 ome_by_sex <- ggplot(ome_by_sex, aes(x = sex, y = avg_ome)) +
   geom_col(fill = "blue", alpha = 0.8) +
   labs(
     title = "Average Codeine OME per Prescription by Sex",
     x = "Sex",
     y = "Average OME"
   ) +
   theme_minimal(base_size = 14)
 print(ome_by_sex)
 # =================================================================================================================================================================================
 # Graphing Codeine OME's by medication name
 # =================================================================================================================================================================================
 ome_by_med <- df %>%
   filter(!is.na(ome)) %>%
   group_by(medicationname) %>%
   summarise(avg_ome = mean(ome, na.rm = TRUE)) %>%
   arrange(desc(avg_ome))
 
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
 # =================================================================================================================================================================================
 # Graphing Codeine OME's by GP practice
 # =================================================================================================================================================================================
 ome_by_gp <- df %>%
   filter(!is.na(ome)) %>%
   group_by(gpidentifiernumber) %>%
   summarise(avg_ome = mean(ome, na.rm = TRUE)) %>%
   arrange(desc(avg_ome))%>%
   slice_head(n = 20)
 
 ome_by_gp <- ggplot(ome_by_gp, aes(x = reorder(gpidentifiernumber, avg_ome), y = avg_ome)) +
   geom_col(fill = "red", alpha = 0.8) +
   coord_flip() +
   labs(
     title = "Average Codeine OME per Prescription by GP (Top 20)",
     x = "GP Identifier",
     y = "Average OME"
   ) +
   theme_minimal(base_size = 14)
 print(ome_by_gp)
 # =================================================================================================================================================================================
 # Boxplot of total OME proportions by sex in high vs low dose
 # ================================================================================================================================================================================= 
 # non-normalised
  ome_by_sex_dose <- df %>%
    filter(!is.na(ome), !is.na(codeine_ranking), !is.na(sex)) %>%
    group_by(codeine_ranking, sex) %>%
    summarise(total_ome = sum(ome, na.rm = TRUE), .groups = "drop")
 
 # Stacked bar chart
 ome_by_sex_barchart <- ggplot(ome_by_sex_dose, aes(x = codeine_ranking, y = total_ome, fill = sex)) +
   geom_col(width = 0.6) +
   scale_y_continuous(labels = comma) +
   labs(
     title = "Total Codeine OME by Dose Category and Sex",
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
     title = "Proportion of Codeine OME by Dose Category and Sex",
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
 
 # =================================================================================================================================================================================
 # Boxplot and violin of total OME proportions by age in high vs low dose
 # ================================================================================================================================================================================= 
 
 boxplot_data <- df %>%
   filter(!is.na(ome), !is.na(age), !is.na(codeine_ranking))
 
 violin_box_plot <- ggplot(boxplot_data,
                           aes(x = codeine_ranking, y = age, fill = codeine_ranking)) +
   geom_violin(alpha = 0.4, trim = FALSE, scale = "width") + 
   geom_boxplot(width = 0.2, outlier.alpha = 0.3) + 
   scale_y_continuous(breaks = seq(0, max(boxplot_data$ome, na.rm = TRUE), by = 10)) +
   labs(
     title = "Distribution of Codeine OME by Age and Dose Category",
     x = "OME",
     y = "Age",
     fill = "Codeine Dose"
   ) +
   scale_fill_manual(values = c(
     "Low" = "blue",
     "High" = "red"
   )) +
   theme_minimal(base_size = 14) +
   theme(
     plot.title = element_text(face = "bold", size = 16),
     axis.text.x = element_text(angle = 45, hjust = 1),
     panel.grid.minor = element_blank()
   )
 
 print(violin_box_plot)
 
 
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
 # Graphing average Codeine OME by age (10 year brackets)
 # =================================================================================================================================================================================  
 
 ome_by_ageband_10_codeine <- df %>%
   filter(
     codeine == TRUE,
     !is.na(age), !is.na(ome),
     age >= 0, age <= 107
   ) %>%
   mutate(
     age_band = cut(
       age,
       breaks = seq(0, 110, by = 10),   # 0–9, 10–19, ... 100–109
       right = FALSE,
       include.lowest = TRUE
     )
   ) %>%
   group_by(age_band) %>%
   summarise(
     avg_ome = mean(ome, na.rm = TRUE),
     n = n(),
     .groups = "drop"
   ) %>%
   mutate(age_band = factor(age_band, levels = unique(age_band)))  # keep order
 
 ome_by_age_graph <- ggplot(ome_by_ageband_10_codeine, aes(x = age_band, y = avg_ome, fill = avg_ome)) +
   geom_col() +
   scale_fill_gradient(low = "lightblue", high = "blue") +
   labs(
     title = "Average OME by Age Band (10-year brackets) — Codeine only",
     x = "Age band (years)",
     y = "Average OME",
     fill = "Avg OME"
   ) +
   theme_minimal(base_size = 12) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 print(ome_by_age_graph)
 
 # =================================================================================================================================================================================
 # Graphing average Codeine OME by age for males and females (10 year brackets)
 # =================================================================================================================================================================================  
 # Summarise avg OME per 10-year window (midpoints) for codeine-only, split by sex
 ome_age_sex <- df %>%
   filter(codeine == TRUE, !is.na(age), !is.na(ome), age >= 0, age <= 107, sex %in% c("M","F")) %>%
   mutate(
     age_band = cut(age, breaks = seq(0, 110, by = 10), right = FALSE, include.lowest = TRUE),
     age_mid  = as.numeric(sub("\\[(\\d+),.*", "\\1", as.character(age_band))) + 5
   ) %>%
   group_by(sex, age_mid) %>%
   summarise(avg_ome = mean(ome, na.rm = TRUE), .groups = "drop")
 
 # Plot with legend: Male = blue, Female = red
 p <- ggplot(ome_age_sex, aes(x = age_mid, y = avg_ome, color = sex, group = sex)) +
   geom_point(size = 2) +
   geom_line(linewidth = 1) +
   scale_color_manual(
     values = c("M" = "blue", "F" = "red"),
     labels = c("M" = "Male", "F" = "Female"),
     name = "Sex"
   ) +
   scale_x_continuous(breaks = seq(5, 105, by = 10)) +
   labs(
     title = "Average OME by Age Band (10-year) — Codeine only",
     x = "Age (10-year band midpoint)",
     y = "Average OME"
   ) +
   theme_minimal()
 
 print(p)