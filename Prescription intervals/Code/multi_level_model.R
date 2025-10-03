
library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)
library(data.table)
library(dtplyr)
library(lme4)

script_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(script_path))

Sys.setlocale("LC_TIME", "C") 
##---##Isolating Statin Data---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


prescriptions <- read.csv("prescriptions_full_sample_140725.csv")

demographics <- read.csv("demographics_full_sample_260825.csv")

demographics$UniquePatientID <- as.character(demographics$UniquePatientID)
demographics$UniquePracticeID <- as.character(demographics$UniquePracticeID)

full_df <- inner_join(prescriptions,demographics,by = c("UniquePatientID","UniquePracticeID"))

full_df<- full_df %>% 
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




##---##Formatting Date---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


full_df<-  lazy_dt(full_df)

df <- full_df %>% 
  mutate(script_date = as.Date(script_date, 
                               format = "%d%b%Y"))  

df_gaps <- df %>%
  arrange(UniquePatientID, script_date) %>%  # Critical for correct ordering
  group_by(UniquePatientID,atc_final) %>%             # Essential for patient-specific gaps
  mutate(
    gap_days = as.numeric(difftime(lead(script_date), script_date, units = "days"))
  ) %>%
  ungroup()

# Remove NA due to missing gap at last prescription (none after end date)
df_gaps<- df_gaps%>%
  drop_na(gap_days)%>%
  filter(script_date < "2026-01-01",
         gap_days<18*30)

df_gaps <- df_gaps %>%
  arrange(UniquePatientID, script_date) %>%
  group_by(UniquePatientID, script_date, atc_final) %>%
  mutate(
    numberofissues = max(numberofissues, na.rm = TRUE),
    gap_days = min(gap_days, na.rm = TRUE)
  ) %>%
  slice(1) %>%  # keep only one row per group (first row after arrange)
  ungroup() %>%
  filter(gap_days != 0)%>%
  mutate(numberofissues = as.numeric(numberofissues))



selected_drugs <- c(
  "C10AA", "A02BC", "N02BEX", "C07AB", "B01AX", "C08X", "C09XA", "B01ACX",
  "H03AA0", "A12AX", "N0X5", "N02X", "A06A", "M01AX", "N06AZ", "N06AX",
  "C09X", "R03AC", "R03AX", "N03AX"
)

atc_labels <- c(
  "C10AA" = "Statins",
  "A02BC" = "Proton pump inhibitors",
  "N02BEX" = "Paracetamol",
  "C07AB" = "Beta blockers",
  "B01AX" = "Oral anticoagulants",
  "C08X" = "DHP calcium channel blockers +/- diuretics",
  "C09XA" = "ACE inhibitors +/- diuretics",
  "B01ACX" = "Antiplatelets (incl. low dose aspirin)",
  "H03AA0" = "Thyroid hormones and derivatives",
  "A12AX" = "Calcium +/- vitamin D",
  "N0X5" = "Benzodiazepines and associated drugs",
  "N02X" = "Opioids",
  "A06A" = "Laxatives",
  "M01AX" = "NSAIDs",
  "N06AZ" = "SSRIs and SNRIs",
  "N06AX" = "Other antidepressants",
  "C09X" = "Angiotensin receptor blockers +/- diuretics",
  "R03AC" = "Inhaled adrenergics",
  "R03AX" = "Inhaled adrenergics with corticosteroids and/or anticholinergics",
  "N03AX" = "Gabapentinoids"
)





# Multi-level model -------------------------------------------------------
###############
library(broom.mixed)   # tidy() for mixed models
library(knitr)         # kable()
library(kableExtra)  


all_results <- list()
for (i in seq_along(selected_drugs)) {
  drug_name <- selected_drugs[i]
  print(i)
  
df_subset <- df_gaps %>%
  filter(str_detect(atc_final, drug_name),
  )%>%
  as.data.frame()

# removing NA covariates
df_subset <- df_subset %>%
  filter(!is.na(age) & !is.na(numberofissues))%>%
  mutate(patienttype = as.factor(patienttype),
         atc_final = as.factor(atc_final),
         patienttype_grouped = case_when(
           grepl("GMS", patienttype) ~ "GMS",
           grepl("DVC", patienttype) ~ "DVC",
           TRUE ~ "Other"
         ))

# Outlier removal using z-score
vars_to_check <- c("age", "gap_days", "numberofissues")
df_cleaned <- df_subset %>%
  filter(
    if_all(all_of(vars_to_check), ~ abs(scale(.)) <= 3)
  )

# centering
df_cleaned$age <- as.numeric(scale(df_cleaned$age, scale = FALSE))

# intercept only model
null_model <- lmer(gap_days ~ (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned,REML = FALSE)
#summary(null_model)

# Level 1 slopes
level1_model <- lmer(gap_days ~ numberofissues +  (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned,REML = FALSE)
#summary(level1_model)

# Level 1+2 slopes
level2_model <- lmer(gap_days ~ numberofissues + age + sex + patienttype_grouped + (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned,REML = FALSE)
#summary(level2_model)


#tbl_null <- as.data.frame(broom.mixed::tidy(null_model, effects = "fixed", conf.int = TRUE))

#tbl_1 <- as.data.frame(broom.mixed::tidy(level1_model, effects = "fixed", conf.int = TRUE))

#tbl_2 <- as.data.frame(broom.mixed::tidy(level2_model, effects = "fixed", conf.int = TRUE))

all_results[[drug_name]]<-list(
  drug = drug_name,
  level0 = null_model,
  level1 = level1_model,
  level2 = level2_model
)

}


saveRDS(all_results, "all_drug_models.rds")
# Load later
#all_results <- readRDS("all_drug_models.rds")]

extract_tbls <- function(model_list) {
  lapply(names(model_list), function(drug) {
    res <- model_list[[drug]]
    
    tbl_level0 <- broom.mixed::tidy(res$level0, effects = "fixed", conf.int = TRUE) %>% as.data.frame()
    tbl_level1 <- broom.mixed::tidy(res$level1, effects = "fixed", conf.int = TRUE) %>% as.data.frame()
    tbl_level2 <- broom.mixed::tidy(res$level2, effects = "fixed", conf.int = TRUE) %>% as.data.frame()
    
    list(
      drug = drug,
      tbl_level0 = tbl_level0,
      tbl_level1 = tbl_level1,
      tbl_level2 = tbl_level2
    )
  }) %>% setNames(names(model_list))
}





all_results_tbls <- extract_tbls(all_results)



tbl_all <- lapply(names(all_results_tbls), function(drug) {
  all_results_tbls[[drug]]$tbl_level2 %>%
    mutate(drug = drug)
}) %>% bind_rows()

tbl_all <- tbl_all %>%
  mutate(drug_label = recode(drug, !!!atc_labels))


fixed_effects <- ggplot(tbl_all, aes(y = drug_label, x = estimate, color = term)) +
geom_point(position = position_dodge(width = 0.5), size = 1) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.5),linewidth=1) +
  xlab("Estimate") +
  ylab("Drug") +
  theme_bw() +
  scale_color_bmj() 
ggsave(filename = "fixed_effects.png", plot = fixed_effects, width = 10, height = 5, dpi = 300)






# Combine selected_drugs into a single regex pattern
pattern <- paste0("^(", paste(selected_drugs, collapse = "|"), ")")

# Filter rows that match any of the selected drugs
df_all_drugs <- df_gaps %>%
  filter(str_detect(atc_final, pattern)) %>%
  mutate(
    atc_final_prefix = str_extract(atc_final, pattern)  # e.g. "C10AA" or "N0X5"
  ) %>%
  as.data.frame()

df_all_drugs <- df_all_drugs %>%
  mutate(atc_final_prefix = as.factor(atc_final_prefix))

df_all_drugs <- df_all_drugs %>%
  mutate(drug_label = recode(atc_final_prefix, !!!atc_labels))


library(ggridges)


gap_plot<-ggplot(df_all_drugs, aes(x = gap_days, y = drug_label, fill = drug_label)) +
  geom_density_ridges(scale=2,bandwidth=3)+
  xlim(0,300)+
 # scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off") + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  theme(legend.position = "none")

ggsave(filename = "gap times.png", plot = gap_plot, width = 10, height = 5, dpi = 300)

df_statins <- df_all_drugs %>% filter(drug_label == "Statins")



ggplot(df_statins, aes(x = gap_days)) +
  geom_histogram(aes(y = after_stat(density)),  # Normalize histogram
                 binwidth = 5,
                 closed = "left",
                 fill = "steelblue",
                 colour = "black")+

  xlim(0,400)

