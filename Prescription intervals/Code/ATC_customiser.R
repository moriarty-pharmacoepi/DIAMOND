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
    atc_final == "B01AC24" ~ "A12AX", #calcium+vitD
    atc_final == "A12AA" ~ "A12AX", #calcium_salts
    atc_final == "A11CC03" ~ "A12AX", #alfacalcidol
    atc_final == "A11CC04" ~ "A12AX", #calcitriol
    atc_final == "A11CC05" ~ "A12AX", #colecalciferol
    atc_final == "N05BA01" ~ "N0X5", #diazepam
    atc_final == "N05BA06" ~ "N0X5", #lorazepam
    atc_final == "N05BA08" ~ "N0X5", #bromazepam
    atc_final == "N05BA12" ~ "N0X5", #alprazolam
    atc_final == "N05CD07" ~ "N0X5", #temazepam
    atc_final == "N05CD08" ~ "N0X5", #midazolam
    atc_final == "N05CF01" ~ "N0X5", #zopiclone
    atc_final == "N05CF02" ~ "N0X5", #zolpidem
    atc_final == "N05BA02" ~ "N0X5", #chlordiazepoxide
    atc_final == "N05BA04" ~ "N0X5", #clobazam
    atc_final == "N05BA09" ~ "N0X5", #oxazepam
    atc_final == "N03AE01" ~ "N0X5", #clonazepam
    atc_final == "N02AA01" ~ "N02X", #morphine
    atc_final == "N02AA05" ~ "N02X", #oxycodone
    atc_final == "N02AA08" ~ "N02X", #dihydrocodeine
    atc_final == "N02AA055" ~ "N02X", #oxycodone+naloxone
    atc_final == "N02AA56" ~ "N02X", #oxycodone+naltrexone
    atc_final == "N02AB03" ~ "N02X", #fentanyl
    atc_final == "N02AE01" ~ "N02X", #buprenorphine
    atc_final == "N02AJ06" ~ "N02X", #codeine+paracetamol
    atc_final == "N02AJ13" ~ "N02X", #tramadol and paracetamol
    atc_final == "N02AX02" ~ "N02X", #tramadol
    atc_final == "R05DA04" ~ "N02X", #codeine
    atc_final == "N02AA59" ~ "N02X", #codeine_combos
    atc_final == "R03DA03" ~ "N02X", #hydrocodone
    atc_final == "R05DA04" ~ "N02X", #codeine_cough
    atc_final == "M01AB05" ~ "M01AX", #diclofenac
    atc_final == "M01AE01" ~ "M01AX", #ibuprofen
    atc_final == "M01AE02" ~ "M01AX", #naproxen
    atc_final == "M01AE52" ~ "M01AX", #vimovo
    atc_final == "M01AG01" ~ "M01AX", #ponstan
    atc_final == "M0AH01" ~ "M01AX", #celecoxib
    atc_final == "M01AH02" ~ "M01AX", #etoricoxib
    atc_final == "M01AE51" ~ "M01AX", #ibuprofen+combos
    atc_final == "N06AB03" ~ "N06AZ", #fluoxetine
    atc_final == "N06AB04" ~ "N06AZ", #citalopram
    atc_final == "N06AB05" ~ "N06AZ", #paroxetine
    atc_final == "N06AB06" ~ "N06AZ", #sertraline
    atc_final == "N06AB10" ~ "N06AZ", #escitalopram
    atc_final == "N06AX16" ~ "N06AZ", #venlafaxine
    atc_final == "N06AX21" ~ "N06AZ", #duloxetine
    atc_final == "C09CA01" ~ "C09X", #losartan
    atc_final == "C09CA03" ~ "C09X", #valsartan
    atc_final == "C09CA04" ~ "C09X", #irbesartan
    atc_final == "C09CA06" ~ "C09X", #candesartan
    atc_final == "C09CA07" ~ "C09X", #telmisartan
    atc_final == "C09CA08" ~ "C09X", #olmesartan
    atc_final == "C09DA01" ~ "C09X", #losartan&diuretics
    atc_final == "C09DA03" ~ "C09X", #valsartan&diuretics
    atc_final == "C09DA04" ~ "C09X", #irbesartan&diuretics
    atc_final == "C09DA07" ~ "C09X", #telmisartan&diuretics
    atc_final == "R03AC" ~ "R03AX", 
    atc_final == "R03BB" ~ "R03AX", 
    atc_final == "R03AK06" ~ "R03AX", #salmeterol&fluticasone
    atc_final == "R03AK07" ~ "R03AX", #formeterol&budesonide
    atc_final == "R03AK08" ~ "R03AX", #formeterol&beclomethasone
    atc_final == "R03AK011" ~ "R03AX", #formeterol&fluticasone
    atc_final == "N03AX09" ~ "N03AX", #lamotrigine
    atc_final == "N03AX11" ~ "N03AX", #topiramate
    atc_final == "N03AX14" ~ "N03AX", #levetiracetam
    atc_final == "N03AX18" ~ "N03AX", #lacosamide
    atc_final == "N03AX23" ~ "N03AX", #brivaracetam
    atc_final == "N03AB02" ~ "N03AX", #phenytoin
    atc_final == "N03AG01" ~ "N03AX", #valproate
    atc_final == "A01BA02" ~ "A01X", #metformin
    atc_final == "A01BD16" ~ "A01X", #metformin&cangliflozin
    atc_final == "A01BD15" ~ "A01X", #metformin&dapagliflozin
    atc_final == "A01BD20" ~ "A01X", #metformin&empagliflozin
    atc_final == "A01BD11" ~ "A01X", #metformin&lingaliptin
    atc_final == "A01BD10" ~ "A01X", #metformin&saxagaliptin
    atc_final == "A01BD07" ~ "A01X", #metformin&sitagliptin
    atc_final == "M05BA04" ~ "M05BX", #alendronic_acid
    atc_final == "M05BA06" ~ "M05BX", #ibrandronic_acid
    atc_final == "M05BA07" ~ "M05BX", #risendronic_acid
    atc_final == "M05BA08" ~ "M05BX", #zolendronic_acid
    atc_final == "M05BB" ~ "M05BX", #bisphosphonate_combos
    TRUE ~ atc_final
  ))

View(custom_atc)