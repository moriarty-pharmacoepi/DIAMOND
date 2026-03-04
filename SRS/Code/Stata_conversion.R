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

# =================================================================================================================================================================================
# Load data
# =================================================================================================================================================================================
data_file_path<- "C:/Users/ryanmuddiman/Royal College of Surgeons in Ireland/Frank Moriarty - RSS 2025/SRS/sample_data.csv"

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
  
 ggplot(monthly_ome_codeine, aes(x = month, y = total_ome)) +
   geom_col(fill = "#4C78A8", width = 25) + # clean blue
   geom_smooth(color = "#E63946", linewidth = 1.2) + #can also use geom_line for a direct overlay#
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
 
 # =================================================================================================================================================================================
 # Graphing Codeine OME's by sex
 # =================================================================================================================================================================================
 ome_by_sex <- df %>%
   filter(!is.na(ome)) %>%
   group_by(sex) %>%
   summarise(avg_ome = mean(ome, na.rm = TRUE))
 
 ggplot(ome_by_sex, aes(x = sex, y = avg_ome)) +
   geom_col(fill = "#2A9D8F", alpha = 0.8) +
   labs(
     title = "Average Codeine OME per Prescription by Sex",
     x = "Sex",
     y = "Average OME"
   ) +
   theme_minimal(base_size = 14)
 # =================================================================================================================================================================================
 # Graphing Codeine OME's by medication name
 # =================================================================================================================================================================================
 ome_by_med <- df %>%
   filter(!is.na(ome)) %>%
   group_by(medicationname) %>%
   summarise(avg_ome = mean(ome, na.rm = TRUE)) %>%
   arrange(desc(avg_ome))
 
 ggplot(ome_by_med, aes(x = reorder(medicationname, avg_ome), y = avg_ome)) +
   geom_col(fill = "#4C78A8", alpha = 0.8) +
   coord_flip() +
   labs(
     title = "Average Codeine OME per Prescription by Product",
     x = "Medication",
     y = "Average OME"
   ) +
   theme_minimal(base_size = 14)
 # =================================================================================================================================================================================
 # Graphing Codeine OME's by GP practice
 # =================================================================================================================================================================================
 ome_by_gp <- df %>%
   filter(!is.na(ome)) %>%
   group_by(gpidentifiernumber) %>%
   summarise(avg_ome = mean(ome, na.rm = TRUE)) %>%
   arrange(desc(avg_ome))%>%
   slice_head(n = 80)
 
 ggplot(ome_by_gp, aes(x = reorder(gpidentifiernumber, avg_ome), y = avg_ome)) +
   geom_col(fill = "#E63946", alpha = 0.8) +
   coord_flip() +
   labs(
     title = "Average Codeine OME per Prescription by GP (Top 20)",
     x = "GP Identifier",
     y = "Average OME"
   ) +
   theme_minimal(base_size = 14)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 