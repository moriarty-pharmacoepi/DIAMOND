library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)

custom_atc <- read.csv("/Users/padraicdonoghue/Library/CloudStorage/OneDrive-SharedLibraries-RoyalCollegeofSurgeonsinIreland/Frank Moriarty - RSS 2025/prescriptions_full_sample_140725.csv")

custom_atc <- custom_atc %>% 
  mutate(atc_final = case_when(
    atc_final == "B01AA03" ~ "B01AX", #warfarin
    atc_final == "B01AB04" ~ "B01AX", #dalteparin
    atc_final == "B01AB05" ~ "B01AX", #enoxaparin
    atc_final == "B01AB10" ~ "B01AX", #tinzaparin
    atc_final == "B01AE07" ~ "B01AX", #dabigatran
    atc_final == "B01AF01" ~ "B01AX", #rivaroxaban
    atc_final == "B01AF02" ~ "B01AX", #apixaban
    atc_final == "B01AF03" ~ "B01AX", #edoxaban
    atc_final == "B01AB01" ~ "B01AX", #heparin
    atc_final == "N02BE51" ~ "N02BEX", #paracetamol combos
    atc_final == "N02BE01" ~ "N02BEX", #paracetamol
    atc_final == "C08CA01" ~ "C08X", #amlodipine
    atc_final == "C08CA02" ~ "C08X", #felodipine
    atc_final == "C08CA05" ~ "C08X", #nifedipine
    atc_final == "C08CA013" ~ "C08X", #lercanidipine
    atc_final == "C08GA02" ~ "C08X", #amlodipine+diuretics
    atc_final == "C09AA02" ~ "C09XA", #enalapril
    atc_final == "C09AA04" ~ "C09XA", #perindopril
    atc_final == "C09AA05" ~ "C09XA", #ramipril
    atc_final == "C09BA04" ~ "C09XA", #perindopril+diuretics
    atc_final == "B01AC06" ~ "B01ACX", #aspirin
    atc_final == "B01AC04" ~ "B01ACX", #clopidogrel
    atc_final == "B01AC22" ~ "B01ACX", #prasugrel
    atc_final == "B01AC24" ~ "B01ACX", #ticagrelor
    
    TRUE ~ atc_final
  ))