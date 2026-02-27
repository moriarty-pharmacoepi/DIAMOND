# ================================
# Libraries
# ================================
library(haven)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(tidyverse)
library(readr)
library(ggplot2)

# ================================
# Load data
# ================================
analgesic_ind <- read_csv("~/Desktop/test.csv") 

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

write_csv(subset_dates,
          "~/Desktop/test.csv")

# Restore equivalent
analgesic_ind <- analgesic_ind %>%
  filter(!(
    (str_detect(atccode, "N06A") & atccode != "N06AA09") |
      str_detect(atccode, "N05") |
      str_detect(atccode, "N03")
  ))

analgesic_ind_dates <- read_csv("~/Desktop/test.csv")
analgesic_ind_A <- read_csv("~/Desktop/test.csv")

df <- bind_rows(analgesic_ind,
                analgesic_ind_dates,
                analgesic_ind_A)

df <- df %>%
  rename(indID = individualidentifiernumber)

# ================================
# Create 30 quantiles (xtile)
# ================================
df <- df %>%
  mutate(ID_d = ntile(indID, 30))

# ================================
# Drug categorisation
# ================================
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

# ================================
# Systemic NSAIDs
# ================================
df <- df %>%
  mutate(
    sysnsaid = str_sub(atccode,1,4)=="M01A" | atccode=="N02AJ14",
    coxib = str_sub(atccode,1,5)=="M01AH",
    nsnsaid = sysnsaid & !coxib
  )

# ================================
# Paracetamol
# ================================
df <- df %>%
  mutate(
    paracetamol = atccode %in% c("N02BE01","N02BE51","N02AJ01","N02AJ06","N02AJ13"),
    paraonly = atccode %in% c("N02BE01","N02BE51")
  )

# ================================
# Strength extraction
# ================================
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

# ================================
# DDD calculation
# ================================
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

# OME calculation
# ================================
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
##======================looks good up to here, next line reduces data set to 12 observations=========================================================================================
# ================================
# Rolling 30-day OME (asrol equivalent)
# ================================
setDT(df)
setorder(df, indID, dateofdispensing)

df[, sum30_ome := frollsum(ome, n = 30, align = "right"), by = indID]
df[, avg30ome := sum30_ome/30]
df[, high_risk_ome := avg30ome > 90 & opioid == TRUE]

# ================================
# Keep final analytic sample
# ================================
df <- df %>%
  filter(between(dateofdispensing, 19724, 23010)) %>%
  filter(DDD == 1)

#write_csv(df,
          "C:/Users/padraicdonoghue/Desktop/test.csv")




##-------------- PADRAIC generating graphs 
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
ggplot(monthly_ome, aes(x = month, y = total_ome)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_number()) +
  theme_minimal()


