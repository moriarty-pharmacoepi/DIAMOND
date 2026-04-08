
library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)
library(data.table)
library(dtplyr)
library(lme4)
library(patchwork)
library(ggridges)
library(forcats)
library(ggsci)
library(RColorBrewer)
library(broom.mixed)   # tidy() for mixed models
library(knitr)         # kable()
library(kableExtra)  
library(tableone)
library(performance)
library(see)
library(cowplot)
library(kableExtra)
library(gt)
library(gridExtra)
library(grid)


set.seed(45)

analytics_names <- c("multi-level", "WTD")
analytics <- data.frame(methods = analytics_names, on = c(1,1))


script_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(script_path))

Sys.setlocale("LC_TIME", "C") 

source("wtd_func_nfrds.r")

##---##Isolating Statin Data---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


prescriptions <- read.csv("prescriptions_full_sample_140725.csv")

demographics <- read.csv("demographics_full_sample_260825.csv")

demographics <- demographics%>%sample_frac(1)

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



##---Pre-processing---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
  ungroup()%>%
# Remove NA due to missing gap at last prescription (none after end date)
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
  filter(row_number() == 1) %>%  # keep only one row per group (first row after arrange)
  ungroup() %>%
  filter(gap_days != 0)%>%
  mutate(numberofissues = as.numeric(numberofissues))%>%
  filter(!is.na(age) & !is.na(numberofissues))



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
all_results <- list()

df_for_wtd<- data.frame()

# Outlier removal using z-score
vars_to_check <- c("age", "gap_days", "numberofissues")


if (analytics$on[analytics$methods == "multi-level"] == 1){
for (i in seq_along(selected_drugs)) {
  drug_name <- selected_drugs[i]
  print(i)
  
df_subset <- df_gaps %>%
  filter(str_detect(atc_final, drug_name),
  )%>%
  as.data.frame()

# removing NA covariates
df_subset <- df_subset %>%
  mutate(patienttype = as.factor(patienttype),
         atc_final = as.factor(atc_final),
         patienttype_grouped = case_when(
           grepl("GMS", patienttype) ~ "GMS",
           grepl("DVC", patienttype) ~ "DVC",
           TRUE ~ "Other"
         ))%>%
  filter(sex != 3)



df_cleaned <- df_subset %>%
  filter(
    if_all(all_of(vars_to_check), ~ abs(scale(.)) <= 3)
  )

# centering
df_cleaned$age <- as.numeric(scale(df_cleaned$age, center = TRUE, scale = FALSE))
df_cleaned$UniquePracticeID <- as.factor(df_cleaned$UniquePracticeID)
df_cleaned$UniquePatientID  <- as.factor(df_cleaned$UniquePatientID)
df_cleaned$sex              <- as.factor(df_cleaned$sex)
df_cleaned$patienttype_grouped <- as.factor(df_cleaned$patienttype_grouped)

df_for_wtd <- rbind(df_for_wtd,df_cleaned)


# intercept only model
null_model <- lmer(gap_days ~ (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned,REML = TRUE)
#summary(null_model)

# Level 1 slopes
level1_model <- lmer(gap_days ~ numberofissues +  (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned,REML = TRUE)
#summary(level1_model)

# Level 1+2 slopes
level2_model <- lmer(gap_days ~ numberofissues + age + sex + patienttype_grouped + (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned,REML = TRUE)
summary(level2_model)



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



#saveRDS(all_results, "all_drug_models.rds")

}
# Load later
all_results <- readRDS("all_drug_models.rds")


# Create directory to save combined plot



all_patients <- bind_rows(lapply(names(all_results), function(drug_name){
  model <- all_results[[drug_name]]$level2
  if(is.null(model)) return(NULL)
  
  ranef(model)$UniquePatientID %>%
    as.data.frame() %>%
    rownames_to_column("ID") %>%
    rename(Intercept = `(Intercept)`) %>%
    mutate(drug = recode(drug_name,!!!atc_labels))
}), .id = NULL)

# Combine all drugs for Practices
all_practices <- bind_rows(lapply(names(all_results), function(drug_name){
  model <- all_results[[drug_name]]$level2
  if(is.null(model)) return(NULL)
  
  ranef(model)$UniquePracticeID %>%
    as.data.frame() %>%
    rownames_to_column("ID") %>%
    rename(Intercept = `(Intercept)`) %>%
    mutate(drug = recode(drug_name,!!!atc_labels))
}), .id = NULL)

# Make sure drug factor levels are the same in both datasets
# Apply the same factor levels to both datasets




##

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


# Combine selected_drugs into a single regex pattern
pattern <- paste0("^(", paste(selected_drugs, collapse = "|"), ")")


null_models <- lapply(all_results, function(x) x$level0)
level2_models <- lapply(all_results, function(x) x$level2)

vpc_null <- lapply(null_models, function(m) icc(m, tolerance = 0, check_singularity = FALSE,by_group = TRUE))
vpc_level2 <- lapply(level2_models,function(m) icc(m, tolerance = 0, check_singularity = FALSE,by_group = TRUE))

combine_vpc <- function(vpc_list, model_name) {
  bind_rows(
    lapply(names(vpc_list), function(drug) {
      df <- vpc_list[[drug]]
      df$drug <- drug
      df$model <- model_name
      df
    })
  )
}

vpc_all <- bind_rows(
  combine_vpc(vpc_null, "null"),
  combine_vpc(vpc_level2, "level2")
)



# Example: create tidy VPC table for level2 models
vpc_plot_data_level_2 <- lapply(names(level2_models), function(drug) {
  m <- level2_models[[drug]]
  
  # Extract variance components
  vc <- as.data.frame(VarCorr(m)) %>%
    rename(Group = grp, Variance = vcov)
  
  vc$drug <- drug
  vc
}) %>%
  bind_rows() %>%
  mutate(Group = factor(Group, levels = c("UniquePracticeID", "UniquePatientID", "Residual")))

vpc_plot_data_level_2 <- vpc_plot_data_level_2 %>%
  group_by(drug) %>%
  mutate(Proportion = Variance) %>%
  ungroup()%>%
  mutate(drug_label = recode(drug, !!!atc_labels))

vpc_plot_data_level_0 <- lapply(names(null_models), function(drug) {
  m <- null_models[[drug]]
  
  # Extract variance components
  vc <- as.data.frame(VarCorr(m)) %>%
    rename(Group = grp, Variance = vcov)
  
  vc$drug <- drug
  vc
}) %>%
  bind_rows() %>%
  mutate(Group = factor(Group, levels = c("UniquePracticeID", "UniquePatientID", "Residual")))

vpc_plot_data_level_0 <- vpc_plot_data_level_0 %>%
  group_by(drug) %>%
  mutate(Proportion = Variance) %>%
  ungroup()%>%
  mutate(drug_label = recode(drug, !!!atc_labels))


vpc_plot_data_combined <- bind_rows(
  vpc_plot_data_level_2 %>% mutate(model = "Full model"),
  vpc_plot_data_level_0 %>% mutate(model = "Intercept only")
)





#check_model(all_results$B01AX$level2,
#            check = c("posterior"),
#            verbose = TRUE)



# Generating data for pWTD ------------------------------------------------
df_all_drugs <- data.frame()

for (i in seq_along(selected_drugs)) {
  atc_pattern <- selected_drugs[i]
  
  df_subset_drugs <- df_gaps %>%
    filter(str_detect(atc_final, atc_pattern)) %>%
    mutate(atc_grouping = atc_pattern)%>%  # assign the grouping here
    mutate(patienttype = as.factor(patienttype),
           atc_final = as.factor(atc_final),
           patienttype_grouped = case_when(
             grepl("GMS", patienttype) ~ "GMS",
             grepl("DVC", patienttype) ~ "DVC",
             TRUE ~ "Other"
           ))%>%
    as.data.frame()%>%
    filter(sex != 3)%>%
    filter(
      if_all(all_of(vars_to_check), ~ abs(scale(.)) <= 3)
    )
  df_all_drugs <- rbind(df_subset_drugs,df_all_drugs)  # store in list
}






df_all_drugs <- df_all_drugs %>%
  mutate(atc_grouping = as.factor(atc_grouping))

df_all_drugs <- df_all_drugs %>%
  mutate(drug_label = recode(atc_grouping, !!!atc_labels))

df_all_drugs$drug_label <- fct_rev(fct_infreq(df_all_drugs$drug_label))


#saveRDS(df_all_drugs , "df_all_drugs.rds")

# Patient level table 1 

library(dplyr)
library(forcats)
library(gtsummary)
library(gt)

#========================
# 1. Patient-level data
#========================
df_tab1_patient <- df_all_drugs %>%
  group_by(UniquePatientID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    sex = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female"
    ),
    drug_label = fct_infreq(drug_label)
  )

tbl_patient <- 
  df_tab1_patient %>%
  select(age, sex, patienttype_grouped) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_n() %>%
  modify_header(stat_0 ~ "**Count**") %>%
  bold_labels()

#========================
# 2. Prescription-level data
#========================
df_tab1_prescription <- df_all_drugs %>%
  mutate(
    sex = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female"
    ),
    drug_label = fct_infreq(drug_label)
  )

tbl_prescription <- 
  df_tab1_prescription %>%
  select(numberofissues, drug_label) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_n() %>%
  modify_header(stat_0 ~ "**Count**") %>%
  bold_labels()

#========================
# 3. Stack tables with subheadings
#========================
tbl_combined <- tbl_stack(
  list(tbl_patient, tbl_prescription),
  group_header = c(
    "**Patient characteristics**",
    "**Prescription characteristics**"
  )
) %>%
  modify_caption("**Table 1. Patient and Prescription Characteristics**")

#========================
# 4. Export to Word (.docx)
#========================
tbl_combined %>%
  as_gt() %>%
  gt::gtsave("Table_1_combined.docx")

#========================
# 5. View in R
#========================
tbl_combined



# Load later
df_all_drugs  <- readRDS("df_all_drugs.rds")

df_all_drugs <- df_all_drugs %>%
  mutate(group = fct_reorder(drug_label, gap_days, .fun = mean, na.rm = TRUE)) %>%
  arrange(group)


ordered_drugs <- df_all_drugs$group

tbl_all$drug_label <- factor(tbl_all$drug_label, levels = levels(ordered_drugs))


drug_levels <- levels(tbl_all$drug_label)

# Create alternating background info
bg_df <- data.frame(
  drug_label = factor(drug_levels, levels = drug_levels),
  ymin = seq_along(drug_levels) - 0.5,  # bottom of rectangle
  ymax = seq_along(drug_levels) + 0.5,  # top of rectangle
  fill = rep(c("grey95", "white"), length.out = length(drug_levels))  # alternating
)

fixed_effects <- ggplot() +
  # 1. Background rectangles first
  geom_rect(data = bg_df, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
            inherit.aes = FALSE) +
  # 2. Coefficients points and lines on top
  geom_linerange(data = tbl_all, 
                 aes(xmin = conf.low, xmax = conf.high, y = drug_label, color = term),
                 position = position_dodge(width = 0.5),
                 linewidth = 0.5, height = 0) +
  geom_point(data = tbl_all, 
             aes(x = estimate, y = drug_label, color = term),
             position = position_dodge(width = 0.5), size = 0.6) +
  scale_fill_identity() +  # use exact colors from bg_df$fill
  scale_color_bmj(labels = c('Intercept', 'Age','Number of issues','GMS:DVC', 'Other:DVC','Female')) +
  xlab("Estimate (days)") +
  ylab("") +
  theme_bw(base_size = 14) +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "top",
        legend.title = element_blank(),
        legend.direction = "vertical")


ggsave(filename = "fixed_effects.png", plot = fixed_effects, width = 10, height = 5, dpi = 300)

df_all_drugs <- df_all_drugs %>%
  mutate(atc_letter = substr(atc_grouping, 1, 1))

atc_labels <- c(
  A = "A – Alimentary tract and metabolism",
  B = "B – Blood and blood forming organs",
  C = "C – Cardiovascular system",
  D = "D – Dermatologicals",
  G = "G – Genito-urinary system and sex hormones",
  H = "H – Systemic hormonal preparations",
  J = "J – Anti-infectives for systemic use",
  L = "L – Antineoplastic and immunomodulating agents",
  M = "M – Musculo-skeletal system",
  N = "N – Nervous system",
  P = "P – Antiparasitic products",
  R = "R – Respiratory system",
  S = "S – Sensory organs",
  V = "V – Various"
)
atc_colors <- c(
  A = "#1b9e77",
  B = "#d95f02",
  C = "#7570b3",
  D = "#e7298a",
  G = "#66a61e",
  H = "#e6ab02",
  J = "#a6761d",
  L = "#666666",
  M = "#1f78b4",
  N = "#b2df8a",
  P = "#fb9a99",
  R = "#fdbf6f",
  S = "#cab2d6",
  V = "#ff7f00"
)



gap_plot <- ggplot(
  df_all_drugs,
  aes(x = gap_days, y = group, fill = atc_letter)
) +
  geom_density_ridges(scale = 1,bandwidth=2) +
  xlim(0, 200) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_bw(base_size = 14) +
  theme(axis.text.y = element_text(size = 10),
        axis.ticks.y = element_line(size = 0.4),
        legend.position = "top",   # x, y inside panel
        legend.background = element_blank(),
        legend.key.size = unit(0.4, "cm"),
        legend.direction = "vertical")+
  coord_cartesian(clip = "off") + 
  labs(
    x = "Interval (days)",         # <-- x-axis label
    y = ""                 # <-- y-axis label
  )+
  scale_fill_brewer(
    name = expression(ATC~1^{st}~Level),
    labels = atc_labels,
    palette = "Set3" 
  )
print(gap_plot)
#ggsave(filename = "gap times.png", plot = gap_plot, width = 10, height = 5, dpi = 300)


fixed_effects_clean <- fixed_effects +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Combine side by side, sharing y-axis
combined_plot <- gap_plot + fixed_effects_clean + plot_layout(ncol = 2, widths = c(1, 1))+plot_annotation(tag_levels = "A")
combined_plot<-combined_plot+theme(
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background  = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA)
)
print(combined_plot)

ggsave("combined_plot2.png",
       plot = combined_plot,
       width = 12, height = 7,
       dpi = 300,
       bg = "transparent")


vpc_plot_data_combined$drug_label <- factor(vpc_plot_data_combined$drug_label,
                                            levels = levels(ordered_drugs))

vpc_plot_data_combined$model <- factor(vpc_plot_data_combined$model,)

var_plot <- ggplot(vpc_plot_data_combined, aes(x = Proportion, y = drug_label, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2",labels=c('GP practice', 'Patient','Residual'))  +
  facet_wrap(~factor(model,levels = c('Intercept only','Full model')), ncol = 2) +   # two panels side by side
  labs(
    title = "Variance Partition per Drug",
    x = expression(Variance~(days^2)),
    y = "",
    fill = "Level"
  ) +
  #scale_fill_discrete(labels=c('GP practice', 'Patient','Residual'))+
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )
print(var_plot)
ggsave(filename = "var_plot_linear.png", plot = var_plot, width = 12, height = 5, dpi = 300)




vpc_plot_data_combined$drug_label <- factor(vpc_plot_data_combined$drug_label,
                                            levels = rev(levels(ordered_drugs)))


# Prepare wide data
tbl_variance_nested <- vpc_plot_data_combined %>%
  select(Group, drug_label, model, Variance) %>%
  group_by(Group, drug_label, model) %>%
  summarise(Variance = mean(Variance, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = c(model, Group),
    values_from = Variance,
    names_sep = "_"
  ) %>%
  arrange(drug_label)

# Identify columns
intercept_cols <- grep("^Intercept only", names(tbl_variance_nested), value = TRUE) %>% sort()
full_model_cols <- grep("^Full model", names(tbl_variance_nested), value = TRUE) %>% sort()

# Reorder columns: Intercept only first, then Full model
tbl_variance_nested <- tbl_variance_nested %>%
  select(drug_label, all_of(intercept_cols), all_of(full_model_cols))

# Rename subcolumns (assuming order: PracticeID, PatientID)
subcolumn_labels <- c("Residual", "GP level", "Patient level")

# Create gt table
tbl_variance_gt <- tbl_variance_nested %>%
  gt() %>%
  # Spanners for models
  tab_spanner(label = "Intercept only", columns = all_of(intercept_cols)) %>%
  tab_spanner(label = "Full model", columns = all_of(full_model_cols)) %>%
  # Rename subcolumns
  cols_label(
    drug_label = "Drug",
    !!intercept_cols[3] := subcolumn_labels[3],
    !!intercept_cols[1] := subcolumn_labels[1],
    !!intercept_cols[2] := subcolumn_labels[2],
    !!full_model_cols[1] := subcolumn_labels[1],
    !!full_model_cols[2] := subcolumn_labels[2],
    !!full_model_cols[3] := subcolumn_labels[3],
  ) %>%
  fmt_number(columns = everything(), decimals = 1) %>%
  tab_header(title = "Variance by Drug, Model, and Group")

tbl_variance_gt

gtsave(tbl_variance_gt, "Variance_by_Drug_Model_Group.docx")







df_all_drugs$drug_label <- (fct_infreq(df_all_drugs$drug_label))

cdf_plot <- ggplot(
  df_all_drugs,
  aes(x = gap_days, color = group)
) +
  stat_ecdf(
    geom = "step",
    linewidth = 0.5
  ) +
  scale_x_log10(breaks = 10^(0:3),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.y = element_text(size = 10)
  ) + 
  guides(color = guide_legend(reverse = TRUE))+
  annotation_logticks(sides = "b")+
  labs(
    x = "Interval (days)",
    y = "Cumulative probability",
    color = "Drug"
  )
print(cdf_plot)
ggsave("cdf_plot.png", plot =cdf_plot, width = 10, height = 5, dpi = 300)



library(dplyr)
library(scales)

drugs <- unique(df_all_drugs$drug_label)
n_drugs <- length(drugs)

# evenly spaced x across overall x-range


cdf_plot <- ggplot(df_all_drugs, aes(x = gap_days, color = drug_label)) +
  stat_ecdf(geom = "step", linewidth = 1) +
  #scale_x_continuous(expand = c(0,0)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_bw(base_size = 14) +
  theme()+
  labs(
    x = "Interval (days)",
    y = "Cumulative probability",
    color = "Drug class"
  )+
  annotation_logticks(sides = "b")
print(cdf_plot)
#ggsave("cdf_plot.png", plot =cdf_plot, width = 12, height = 5, dpi = 300)








gap_plot_strat <- ggplot(
  df_all_drugs %>% filter(numberofissues %in% 1:2),
  aes(x = gap_days, y = group, fill = group)
) +
  geom_density_ridges(scale = 1, bandwidth = 2) +
  xlim(0, 200) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_bw(base_size = 14) +
  labs(
    x = "Interval (days)",
    y = ""
  ) +
  facet_wrap(
    ~ numberofissues,
    nrow = 1,
    scales = "fixed",
    strip.position = "top",
    labeller = as_labeller(c('1'='number of issues = 1', '2'='number of issues = 2'))
  ) +
  theme(
    strip.text = element_text(size = 12),
    # Remove y-axis text/ticks for all but the first facet
    panel.spacing = unit(1, "lines"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 12))
print(gap_plot_strat)
ggsave("strat_plot.png", plot = gap_plot_strat, width = 12, height = 5, dpi = 300)




##---Scheme boxplot------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_statins <- df_all_drugs %>% filter(drug_label == "Statins")

df_statins <- df_statins %>%
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


ggplot(df_statins, aes(x = gap_days)) +
  geom_histogram(aes(y = after_stat(density)),  # Normalize histogram
                 binwidth = 5,
                 closed = "left",
                 fill = "steelblue",
                 colour = "black")+
  xlim(0,400)



df_statins <- df_statins %>%
  mutate(sex_label = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female",
    TRUE ~ "Unknown"
  ))

sex_boxplot <- ggplot(df_statins %>% filter(sex_label %in% c("Male", "Female")), 
                      aes(x = sex_label, y = gap_days)) +
  geom_boxplot(aes(fill = sex_label), 
               alpha = 0.7,
               outlier.shape = NA) +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 18, 
               size = 3, 
               color = "red") +
  scale_fill_manual(values = c("Male" = "purple", "Female" = "orange")) +
  scale_y_continuous(limits = c(0, 365),
                     breaks = seq(0, 365, by = 30)) +
  labs(title = "Statin Prescription Intervals by Sex",
       subtitle = "Red diamonds show group averages",
       x = "Sex",
       y = "Days Until Next Prescription",
       caption = paste("Total prescriptions:",
                       nrow(df_statins),
                       "| Male:",
                       sum(df_statins$sex == 1, na.rm = TRUE),
                       "| Female:",
                       sum(df_statins$sex == 2, na.rm = TRUE))) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

plot(sex_boxplot)


scheme_boxplot <- ggplot(df_statins, 
                         aes(x = patienttype_grouped, y = gap_days)) +
  geom_boxplot(aes(fill = patienttype_grouped), 
               alpha = 1,
               outlier.shape = NA) +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 18, 
               size = 3, 
               color = "red") +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(limits = c(0, 365),
                     breaks = seq(0, 365, by = 30)) +
  labs(title = "Statin Prescription Intervals by\n Insurance Scheme",
       subtitle = "Red diamonds show group averages",
       x = "Healthcare Scheme",
       y = "Days Until Next Prescription") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

plot(scheme_boxplot)

age_boxplot <- ggplot(df_statins%>%drop_na(), 
                      aes(x = age_bracket, y = gap_days)) +
  geom_boxplot(aes(fill = age_bracket), 
               alpha = 1,
               outlier.shape = NA) +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 18, 
               size = 3, 
               color = "red") +
  scale_fill_viridis_d(option = "magma", 
                       begin = 0.2, 
                       end = 0.8) +  # Age-appropriate color gradient
  scale_y_continuous(limits = c(0, 365),
                     breaks = seq(0, 365, by = 30)) +
  labs(title = "Statin Prescription Intervals by\n Age Group",
       subtitle = "Red diamonds show group averages",
       x = "Age group",
       y = "Days Until Next Prescription") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

plot(age_boxplot)



p <- (plot_spacer() + sex_boxplot) /plot_spacer()/
  (age_boxplot + scheme_boxplot) +
  plot_layout(heights = c(1,0.2, 1))  # top row = 1 unit, bottom row = 2 units
print(p)

#(filename = "descriptive_plots.png", plot = p, width = 12, height = 10, dpi = 300)

statins_table<-tbl_all %>% 
  filter(drug_label=="Statins")%>%
  select(Term = term, Estimate = estimate, 
         conf.low,conf.high) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  # Combine CI into one column
  mutate(CI = paste0("(", conf.low, ", ", conf.high, ")"))%>%
  select(Term, Estimate, CI)

# Display nicely
kable(statins_table, caption = "Fixed Effects for Statins", align = "c")


# Create the table grob
table_grob <- tableGrob(statins_table, rows = NULL)

# Save as PNG
#png("statins_table.png", width = 800, height = 400)
grid.draw(table_grob)
dev.off()


###

# Step 1: Count prescriptions per practice and drug
df_counts <- df_all_drugs %>%
  group_by(UniquePracticeID, drug_label) %>%
  summarise(count = n(), .groups = "drop")

# Step 2: Normalize per practice
df_normalized <- df_counts %>%
  group_by(UniquePracticeID) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Step 3: Optional - order practices/drugs for readability
df_normalized <- df_normalized %>%
  mutate(
    UniquePracticeID = factor(UniquePracticeID, levels = unique(UniquePracticeID[order(-prop)])),
    drug_label = factor(drug_label, levels = unique(drug_label[order(-prop)]))
  )

# Step 4: Plot heatmap
ggplot(df_normalized, aes(y = drug_label, x = UniquePracticeID, fill = prop)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue")+
  labs(
    title = "Normalized Prescription Heatmap (Proportion per Practice)",
    y = "Drug Label",
    x = "Practice ID",
    fill = "Proportion"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    axis.text.y = element_text(size = 10)  # shrink y labels if too many practices
  )




# Model selection - Parametric waiting time distribution ------------------------------------
set.seed(40)
if (analytics$on[analytics$methods == "WTD"] == 1) {
  
  save_dir <- "drug_results"  # folder to save individual rds
  if (!dir.exists(save_dir)) dir.create(save_dir)
  
  for (j in seq_along(selected_drugs)) {
  #for (j in 8) {
    print(paste("Processing drug", j, "of", length(selected_drugs)))
    
    drug_name <- selected_drugs[j]
    
    # Filter and sort once per drug
    df_gaps_single <- df_all_drugs %>%
      filter(str_detect(atc_final, drug_name)) %>%
      arrange(UniquePatientID, script_date)
    
    study_start <- min(df_gaps_single$script_date, na.rm = TRUE)
    study_end <- max(df_gaps_single$script_date, na.rm = TRUE)
    
    window <- 365
    n <- 10 # number of repetitions
    
    data_prep <- function() {
      first_times_list <- vector("list", n)
      for (i in seq_len(n)) {
        random_date <- sample(seq(study_start, study_start + window, by = "day"), 1)
        df_with_randoms <- df_gaps_single %>%
          filter(script_date > random_date & script_date < (random_date + window)) %>%
          group_by(UniquePatientID) %>%
          summarise(first_script = first(script_date), .groups = "drop")
        first_times_list[[i]] <- df_with_randoms %>%
          mutate(days_to_first = as.integer(first_script - random_date))
      }
      bind_rows(first_times_list)
    }
    
    Gmax <- 5
    iterations <- 50
    best_tries <- 5
    
    results <- data.frame(
      G = 1:Gmax,
      logLik = NA_real_,
      AIC = NA_real_,
      BIC = NA_real_,
      CV_logLik = NA_real_
    )
    
    first_rx <- data_prep()
    
    for (G in seq_len(Gmax)) {
      best_fit <- NULL
      best_logLik <- -Inf
      for (try_idx in seq_len(best_tries)) {
        fit <- tryCatch({
          WTD_fit(
            first_rx$days_to_first,
            first_rx$UniquePatientID,
            window = window,
            G = G,
            plots = TRUE,
            iter = iterations
          )
        }, error = function(e) NULL)
        
        if (is.null(fit)) {
          first_rx <- data_prep()
          next
        }
        
        if (!is.null(fit) && fit$logLik > best_logLik) {
          best_fit <- fit
          best_logLik <- fit$logLik
        }
      }
      
      if (!is.null(best_fit)) {
        results$BIC[results$G == G] <- best_fit$BIC
        results$AIC[results$G == G] <- best_fit$AIC
        results$logLik[results$G == G] <- best_fit$logLik
      } else {
        warning(paste("All 5 tries failed for drug", drug_name, "G =", G))
      }
    }
    
    results$Drug <- drug_name
    
    # save individual RDS per drug
    rds_file <- file.path(save_dir, paste0(drug_name, "_results.rds"))
    #saveRDS(results, rds_file)
    cat(paste("Saved results for", drug_name, "to", rds_file, "\n"))
  }
  
}



cross_val_results <- list()
for (j in seq_along(selected_drugs)) {
  
  drug_name <- selected_drugs[j]
  filename<-paste0("drug_results/",drug_name, "_results.rds")
  cross_val_results[[drug_name]] <- readRDS(filename)
  }


# Combine results
cross_val_results_df <- bind_rows(cross_val_results)

# Add labels and ordering
ordered_drugs <- df_all_drugs$group

cross_val_results_df <- cross_val_results_df %>%
  mutate(drug_label = recode(Drug, !!!atc_labels)) %>%
  mutate(drug_label = factor(drug_label, levels = rev(levels(ordered_drugs))))


#saveRDS(cross_val_results_df, "cross_val_results_df_new.rds")

cross_val_results_df <- readRDS("cross_val_results_df.rds")

# Compute minimum BIC per drug
min_criteria_df <- cross_val_results_df %>%
  group_by(drug_label) %>%
  summarise(
    min_BIC = G[which.min(BIC)],
    BIC_val = min(BIC),
    min_AIC = G[which.min(AIC)],
    AIC_val = min(AIC)
  ) %>%
  pivot_longer(
    cols = c(BIC_val, AIC_val),
    names_to = "criterion",
    values_to = "value"
  ) %>%
  mutate(G = ifelse(criterion == "BIC_val", min_BIC, min_AIC),
         criterion = recode(criterion, 
                            "BIC_val" = "BIC (min)", 
                            "AIC_val" = "AIC (min)"))

# Plot BIC and AIC together
p <- ggplot(cross_val_results_df) +
  geom_line(aes(x = G, y = BIC, color = "BIC"), size = 1) +
  geom_line(aes(x = G, y = AIC, color = "AIC"), size = 1) +
  geom_point(
    data = min_criteria_df,
    aes(x = G, y = value, color = criterion),
    size = 2
  ) +
  facet_wrap(~ drug_label, scales = "free", labeller = label_wrap_gen(width = 23)) +
  labs(
    y = "Information Criterion",
    x = "Number of components",
    title = "IC vs. number of components",
    color = "Criterion"
  ) +
  scale_color_jama() +
  theme_bw() +
  theme(
    strip.text = element_text(size = 8),
    legend.position = "right",     # vertical legend
    legend.justification = "center",  # align to top of plot area
    legend.box.margin = margin(0,0,0,0),
    legend.margin = margin(0,0,0,0)
  )
print(p)

ggsave(
  filename = "bic_aic_plot_vertical_legend.png",
  plot = p,
  width = 8,
  height = 8,
  dpi = 300
)





# Fitting pWTD ------------------------------------------------------------

min_bic_df <- cross_val_results_df %>% group_by(drug_label) %>% slice_min(BIC, n = 1)
iterations <- 50

n<-10
window<-365
B = 20


fits <- vector("list", length(selected_drugs))
names(fits) <- selected_drugs


library(doSNOW)
library(foreach)
library(doRNG)
library(dplyr)
library(stringr)
library(progress)
library(parallel)
num_cores <- detectCores()
cl <- makeCluster(num_cores-1)
clusterExport(cl, c(
  "df_split",
  "selected_drugs",
  "min_bic_df",
  "window",
  "B",
  "iterations",
  "n"
))
#setwd("~/R Scripts")
#source("functions.r")
registerDoSNOW(cl)
pb <- progress_bar$new(
  format = "  Progress [:bar] :percent ETA: :eta Elapsed: :elapsed",
  total = 20 , clear = FALSE, width = 60
)
# Define progress function
progress <- function(n) pb$tick()
opts <- list(progress = progress)

df_split <- split(df_all_drugs, df_all_drugs$atc_grouping)

# Parallel foreach with reproducible RNG
fits_list <- foreach(
  j = seq_along(selected_drugs),
  .packages = c("dplyr", "stringr"),
  .options.snow = opts,
  .options.RNG = 1234
) %do% {
  source("wtd_func_nfrds.r")
  drug_name <- selected_drugs[j]
  save_dir <- "drug_results"
  # Filter and sort once per drug
  df_gaps_single <- df_split[[drug_name]]
  
  if (nrow(df_gaps_single) == 0) {
    warning(paste("Drug not found:", drug_name))
    return(NULL)
  }
  
  G <- min_bic_df %>%
    filter(Drug == drug_name) %>%
    pull(G)
  
  study_start <- min(df_gaps_single$script_date, na.rm = TRUE)
  study_end   <- max(df_gaps_single$script_date, na.rm = TRUE)
  
  # Data preparation function
  data_prep <- function() {
    first_times_list <- vector("list", n)
    
    for (i in seq_len(n)) {
      random_date <- sample(seq(study_start, study_start + window, by = "day"), 1)
      
      df_with_randoms <- df_gaps_single %>%
        filter(script_date > random_date &
                 script_date < (random_date + window)) %>%
        group_by(UniquePatientID) %>%
        summarise(first_script = first(script_date),
                  .groups = "drop")
      
      first_times_list[[i]] <- df_with_randoms %>%
        mutate(days_to_first = as.integer(first_script - random_date))
    }
    
    bind_rows(first_times_list)
  }
  
  # Retry until successful fit
  first_rx <- data_prep()
  
  fit <- tryCatch({
    WTD_fit(
      first_rx$days_to_first,
      first_rx$UniquePatientID,
      window = window,
      G = G,
      B = B,
      plots = TRUE,
      iter = iterations
    )
  }, error = function(e)
    NULL)
  
  while (is.null(fit)) {
    cat("Failed, refitting for", drug_name, "\n")
    first_rx <- data_prep()
    fit <- tryCatch({
      WTD_fit(
        first_rx$days_to_first,
        first_rx$UniquePatientID,
        window = window,
        G = G,
        B = B,
        plots = FALSE,
        iter = iterations
      )
    }, error = function(e)
      NULL)
  }
  
  fit[["drug"]] <- drug_name
  
  rds_file <- file.path(save_dir, paste0(drug_name, "_fit_new.rds"))
  saveRDS(fit, rds_file)
  cat(paste("Saved results for", drug_name, "to", rds_file, "\n"))
  
  fit
}

# Close progress bar and cluster
#close(pb)
#stopCluster(cl)

# Convert to named list
fits <- setNames(fits_list, selected_drugs)


fits <- list()
for (j in seq_along(selected_drugs)) {

  drug_name <- selected_drugs[j]
  filename<-paste0("drug_results/",drug_name, "_fit_new.rds")
  fits[[drug_name]] <- readRDS(filename)
}



#saveRDS(fits,"fits.rds")

#fits <- readRDS("fits.rds")

extract_fit_info <- function(fit) {
  data.frame(
    G = fit$G,
    w = fit$w,
    delta = fit$delta,
    BIC = fit$BIC,
    AIC = fit$AIC,
    means = paste(fit$means, collapse = ","),
    mu = paste(fit$mu, collapse = ","),
    log_sigma = paste(fit$log_sigma, collapse = ","),
    p_raw = paste(fit$p_raw, collapse = ","),
    drug_name = fit$drug
  )
}
# Apply the function to each fit in the list
fit_df <- bind_rows(lapply(fits, extract_fit_info))

fit_df <- fit_df %>%
  mutate(drug_label = recode(drug_name, !!!atc_labels)) %>%
  mutate(drug_label = factor(drug_label, levels = (levels(ordered_drugs))))

# View result


fit_long <- fit_df %>%
  rowwise() %>%
  mutate(
    mu_list = list(as.numeric(strsplit(means, ",")[[1]]))
  ) %>%
  unnest(mu_list)


bg_df <- data.frame(
  drug_label = factor(drug_levels, levels = drug_levels),
  ymin = seq_along(drug_levels) - 0.5,  # bottom of rectangle
  ymax = seq_along(drug_levels) + 0.5,  # top of rectangle
  fill = rep(c("grey95", "white"), length.out = length(drug_levels))  # alternating
)


dot_plot <- ggplot() +
  # 1. Background rectangles
  geom_rect(
    data = bg_df,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
    inherit.aes = FALSE
  ) +
  # 2. Points
  geom_point(
    data = fit_long,
    aes(x = mu_list, y = factor(drug_label, levels = drug_levels)),
    position = position_jitter(height = 0.2),
    size = 3,
    color = "steelblue"
  ) +
  
  labs(x = "Mean (mu)", y = "Drug") +
  
  scale_fill_identity() +   # <- critical
  theme_bw()
print(dot_plot)




library(dplyr)
library(purrr)

qq_df <- map_dfr(selected_drugs, function(drug_plot) {
  
  # 1. Subset the observed data for this drug
  gaps_for_emp <- df_all_drugs %>%
    filter(atc_grouping == drug_plot)
  
  drug_label <- gaps_for_emp$drug_label[1]
  # 2. Extract fitted data and bootstraps
  fit <- fits[[drug_plot]]
  fit_data <- fit$plot_data        # point estimate densities
  IAD_se <- fit$iad_se   # B x t_vals
  t_vals <- fit_data$t
    # Convert to CDF
    
  probs <- seq(0.01, 0.99, by = 0.01)
  
  # 6. Interpolate fitted quantiles and their confidence bands
  boot_quantiles <- apply(fit$boot_CDF_mat, 1, function(cdf_row) {
    approx(
      x = cdf_row,
      y = t_vals,
      xout = probs,
      ties = "ordered"
    )$y
  })

  fit_quantiles <- rowMeans(boot_quantiles)
  
  fitted_lower <- apply(boot_quantiles, 1, quantile, 0.025)
  fitted_upper <- apply(boot_quantiles, 1, quantile, 0.975)
   # 5. Compute empirical quantiles
 
  emp_quantiles <- quantile(gaps_for_emp$gap_days, probs = probs)
  
  # 7. Return tidy data frame for this drug
  data.frame(
    empirical = emp_quantiles,
    fitted = fit_quantiles,
    fitted_lower = fitted_lower,
    fitted_upper = fitted_upper,
    prob = probs,
    drug = drug_label
  )
  
})


qq_df<-qq_df%>%
mutate(drug = factor(drug, levels = rev(levels(ordered_drugs))))

p2<-ggplot(qq_df, aes(x = fitted, y = empirical)) +
  geom_rug(sides = "b", alpha = 0.4) +  # bottom rug for fitted values
  geom_rug(aes(x = NULL, y = empirical), sides = "l", alpha = 0.4) +
  geom_point(size = 1,alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed",linewidth = 0.7) +
  facet_wrap(~ drug,scales = "free",labeller = label_wrap_gen(width = 23)) +
  theme(strip.text = element_text(size = 8))+
  labs(
    title = "QQ Plots - Empirical vs Custom Fitted Models",
    x = "Theoretical (Fitted)",
    y = "Empirical"
  ) +
  theme_bw()
print(p2)



p3 <- ggplot(
qq_df,
  aes(x = fitted, y = empirical)
) +
  geom_rug(sides = "b", alpha = 0.4) +
  geom_rug(aes(y = empirical), sides = "l", alpha = 0.4) +
  geom_line(
    data = qq_df,
    aes(x = fitted_lower, y = empirical),color = "red"
  ) +
  geom_line(
    data = qq_df,
    aes(x = fitted_upper, y = empirical),color = "blue"
  ) +
  geom_point(size = 1, alpha = 0.6) +
  geom_abline(
    slope = 1, intercept = 0,
    color = "red", linetype = "dashed", linewidth = 0.7
  ) +
  labs(
    title = "QQ Plot - Statins",
    x = "Theoretical (Fitted)",
    y = "Empirical"
  ) +
  theme_bw()

print(p3)



aligned <- align_plots(p, p2, align = "v", axis = "l")
top_aligned <- aligned[[1]]
bottom_aligned <- aligned[[2]]

# Combine vertically
panels <- cowplot::plot_grid(
  top_aligned,
  bottom_aligned,
  ncol = 1,
  align = "v",
  labels = c("A", "B"),
  label_size = 18,
  label_fontface = "plain"
)
ggsave("wtd_fits_plot.png",panels, width = 12, height = 16, dpi = 300)




quantile_table <- qq_df %>%
  filter(prob %in% c(0.5, 0.8)) %>%
  pivot_wider(
    id_cols = drug,
    names_from = prob,
    values_from = c(empirical, fitted_lower,fitted, fitted_upper),
    names_glue = "{.value}_{prob}"
  ) %>%
  select(drug, 
         empirical_0.5, fitted_lower_0.5,fitted_0.5, fitted_upper_0.5,
         empirical_0.8, fitted_lower_0.8,fitted_0.8, fitted_upper_0.8)


library(tidyr)

quantile_table_word <- quantile_table %>%
  mutate(
    fitted_0.5 = paste0(round(fitted_0.5, 1), " (", 
                        round(fitted_lower_0.5, 1), "–", round(fitted_upper_0.5, 1), ")"),
    fitted_0.8 = paste0(round(fitted_0.8, 1), " (", 
                        round(fitted_lower_0.8, 1), "–", round(fitted_upper_0.8, 1), ")")
  ) %>%
  select(drug, empirical_0.5, fitted_0.5, empirical_0.8, fitted_0.8)%>%
  arrange(drug)

library(dplyr)
library(flextable)
library(officer)
ft <- flextable(quantile_table_word)

# Step 3: Add nested headers
ft <- set_header_labels(
  ft,
  empirical_0.5 = "Empirical",
  fitted_0.5 = "Fitted (95% CI)",
  empirical_0.8 = "Empirical",
  fitted_0.8 = "Fitted (95% CI)"
)

ft <- add_header_row(
  ft,
  values = c("Quantile", "0.5", "0.8"),
  colwidths = c(1, 2, 2)  # 1 column for drug, 2 columns per quantile
)

# Step 4: Adjust formatting
ft <- autofit(ft)
ft <- bold(ft, part = "header")

# Step 5: Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "quantile_table.docx")



# Sensitivity analysis (Multi-level model) --------------------------------

# Multi-level model -------------------------------------------------------
###############
all_results_sensi <- list()



# Outlier removal using z-score
vars_to_check <- c("age", "gap_days", "numberofissues")

  for (i in seq_along(selected_drugs)) {
    drug_name <- selected_drugs[i]
    print(i)
    
    df_subset_sensi  <- df_gaps %>%
      filter(str_detect(atc_final, drug_name),
      )%>%
      as.data.frame()
    
    # removing NA covariates
    df_subset_sensi <- df_subset_sensi %>%
      mutate(patienttype = as.factor(patienttype),
             atc_final = as.factor(atc_final),
             patienttype_grouped = case_when(
               grepl("GMS", patienttype) ~ "GMS",
               grepl("DVC", patienttype) ~ "DVC",
               TRUE ~ "Other"
             ))%>%
      filter(sex != 3)
    
    
    
    df_cleaned_sensi <- df_subset_sensi
    
    # centering
    df_cleaned_sensi$age <- as.numeric(scale(df_cleaned_sensi$age, center = TRUE, scale = FALSE))
    df_cleaned_sensi$UniquePracticeID <- as.factor(df_cleaned_sensi$UniquePracticeID)
    df_cleaned_sensi$UniquePatientID  <- as.factor(df_cleaned_sensi$UniquePatientID)
    df_cleaned_sensi$sex              <- as.factor(df_cleaned_sensi$sex)
    df_cleaned_sensi$patienttype_grouped <- as.factor(df_cleaned_sensi$patienttype_grouped)
    
    
    # intercept only model
    null_model <- lmer(gap_days ~ (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned_sensi,REML = TRUE)
    #summary(null_model)
    
    # Level 1 slopes
    level1_model <- lmer(gap_days ~ numberofissues +  (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned_sensi,REML = TRUE)
    #summary(level1_model)
    
    # Level 1+2 slopes
    level2_model <- lmer(gap_days ~ numberofissues + age + sex + patienttype_grouped + (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned_sensi,REML = TRUE)
    summary(level2_model)
    
    
  
    
    all_results_sensi[[drug_name]]<-list(
      drug = drug_name,
      level0 = null_model,
      level1 = level1_model,
      level2 = level2_model
    )
    
  }
  
  
saveRDS(all_results_sensi,"all_results_sensi.rds")


all_results_tbls_sensi <- extract_tbls(all_results_sensi)



tbl_all_sensi <- lapply(names(all_results_tbls_sensi), function(drug) {
  all_results_tbls_sensi[[drug]]$tbl_level2 %>%
    mutate(drug = drug)
}) %>% bind_rows()

tbl_all_sensi <- tbl_all_sensi %>%
  mutate(drug_label = recode(drug, !!!atc_labels))


# Combine selected_drugs into a single regex pattern
pattern <- paste0("^(", paste(selected_drugs, collapse = "|"), ")")


null_models_sensi <- lapply(all_results_sensi, function(x) x$level0)
level2_models_sensi <- lapply(all_results_sensi, function(x) x$level2)

vpc_null_sensi <- lapply(null_models_sensi, function(m) icc(m, tolerance = 0, check_singularity = FALSE,by_group = TRUE))
vpc_level2_sensi <- lapply(level2_models_sensi,function(m) icc(m, tolerance = 0, check_singularity = FALSE,by_group = TRUE))


vpc_all_sensi <- bind_rows(
  combine_vpc(vpc_null_sensi, "null"),
  combine_vpc(vpc_level2_sensi, "level2")
)



# Example: create tidy VPC table for level2 models
vpc_plot_data_level_2_sensi <- lapply(names(level2_models_sensi), function(drug) {
  m <- level2_models_sensi[[drug]]
  
  # Extract variance components
  vc <- as.data.frame(VarCorr(m)) %>%
    rename(Group = grp, Variance = vcov)
  
  vc$drug <- drug
  vc
}) %>%
  bind_rows() %>%
  mutate(Group = factor(Group, levels = c("UniquePracticeID", "UniquePatientID", "Residual")))

vpc_plot_data_level_2_sensi <- vpc_plot_data_level_2_sensi %>%
  group_by(drug) %>%
  mutate(Proportion = Variance) %>%
  ungroup()%>%
  mutate(drug_label = recode(drug, !!!atc_labels))

vpc_plot_data_level_0_sensi <- lapply(names(null_models_sensi), function(drug) {
  m <- null_models_sensi[[drug]]
  
  # Extract variance components
  vc <- as.data.frame(VarCorr(m)) %>%
    rename(Group = grp, Variance = vcov)
  
  vc$drug <- drug
  vc
}) %>%
  bind_rows() %>%
  mutate(Group = factor(Group, levels = c("UniquePracticeID", "UniquePatientID", "Residual")))

vpc_plot_data_level_0_sensi <- vpc_plot_data_level_0_sensi %>%
  group_by(drug) %>%
  mutate(Proportion = Variance) %>%
  ungroup()%>%
  mutate(drug_label = recode(drug, !!!atc_labels))


vpc_plot_data_combined_sensi <- bind_rows(
  vpc_plot_data_level_2_sensi %>% mutate(model = "Full model"),
  vpc_plot_data_level_0_sensi %>% mutate(model = "Intercept only")
)

# Ensure the drug factor order is consistent across plots
vpc_plot_data_combined_sensi$drug_label <- factor(vpc_plot_data_combined_sensi$drug_label,
                                            levels = levels(ordered_drugs))

vpc_plot_data_combined_sensi$model <- factor(vpc_plot_data_combined_sensi$model,)

var_plot_sensi <- ggplot(vpc_plot_data_combined_sensi, aes(x = Proportion, y = drug_label, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2",labels=c('GP practice', 'Patient','Residual'))  +
  facet_wrap(~factor(model,levels = c('Intercept only','Full model')), ncol = 2) +   # two panels side by side
  labs(
    title = "Variance Partition per Drug",
    x = expression(Variance~(days^2)),
    y = "",
    fill = "Level"
  ) +
  #scale_fill_discrete(labels=c('GP practice', 'Patient','Residual'))+
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )
print(var_plot_sensi)
ggsave(filename = "var_plot_linear_sensi.png", plot = var_plot_sensi, width = 12, height = 5, dpi = 300)





# Load later
df_all_drugs  <- readRDS("df_all_drugs.rds")






tbl_all_sensi$drug_label <- factor(tbl_all_sensi$drug_label, levels = levels(ordered_drugs))


drug_levels <- levels(tbl_all_sensi$drug_label)


fixed_effects_sensi <- ggplot() +
  # 1. Background rectangles first
  geom_rect(data = bg_df, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
            inherit.aes = FALSE) +
  # 2. Coefficients points and lines on top
  geom_linerange(data = tbl_all_sensi, 
                 aes(xmin = conf.low, xmax = conf.high, y = drug_label, color = term),
                 position = position_dodge(width = 0.5),
                 linewidth = 0.5, height = 0) +
  geom_point(data = tbl_all_sensi, 
             aes(x = estimate, y = drug_label, color = term),
             position = position_dodge(width = 0.5), size = 0.6) +
  scale_fill_identity() +  # use exact colors from bg_df$fill
  scale_color_bmj(labels = c('Intercept', 'Age','Number of issues','GMS:DVC', 'Other:DVC','Female')) +
  xlab("Estimate (days)") +
  ylab("") +
  theme_bw(base_size = 14) +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "right",
        legend.title = element_blank())

print(fixed_effects_sensi)
ggsave(filename = "fixed_effects_sensi.png", plot = fixed_effects_sensi, width = 10, height = 5, dpi = 300)




df_all_drugs_sensi <- data.frame()

for (i in seq_along(selected_drugs)) {
  atc_pattern <- selected_drugs[i]
  
  df_subset_drugs <- df_gaps %>%
    filter(str_detect(atc_final, atc_pattern)) %>%
    mutate(atc_grouping = atc_pattern)%>%  # assign the grouping here
    mutate(patienttype = as.factor(patienttype),
           atc_final = as.factor(atc_final),
           patienttype_grouped = case_when(
             grepl("GMS", patienttype) ~ "GMS",
             grepl("DVC", patienttype) ~ "DVC",
             TRUE ~ "Other"
           ))%>%
    as.data.frame()%>%
    filter(sex != 3)
  df_all_drugs_sensi <- rbind(df_subset_drugs,df_all_drugs_sensi)  # store in list
}



#ordered_drugs <- fct_rev(fct_infreq(df_all_drugs_sensi$drug_label))



all_patients$drug <- factor(all_patients$drug, levels = (levels(ordered_drugs)))
all_practices$drug <- factor(all_practices$drug, levels = (levels(ordered_drugs)))

# Boxplot for Patients
p_patients <- ggplot(all_patients, aes(y = drug, x = Intercept)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.3, outlier.size = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Patient Random Intercepts", y = NULL, x = "Interval (Days)") +
  theme_bw() +
  theme(axis.text.y = element_blank()) # hide duplicate labels

# Boxplot for Practices
p_practices <- ggplot(all_practices, aes(y = drug, x = Intercept)) +
  geom_boxplot(fill = "steelblue", alpha = 0.5, outlier.size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Practice Random Intercepts", y = NULL, x = "Interval (Days)") +
  theme_bw()

# Combine left/right
final_plot <-  (p_practices | p_patients)+plot_annotation(tag_levels = "A") 

# Print
print(final_plot)

# Save
ggsave("patients_practices_by_drug.png", 
       plot = final_plot, width = 12, height = 5)


df_all_drugs_sensi <- df_all_drugs_sensi %>%
  mutate(atc_grouping = as.factor(atc_grouping))

df_all_drugs_sensi <- df_all_drugs_sensi %>%
  mutate(drug_label = recode(atc_grouping, !!!atc_labels))

df_all_drugs_sensi$drug_label <- fct_rev(fct_infreq(df_all_drugs_sensi$drug_label))






gap_plot <- ggplot(
  df_all_drugs_sensi,
  aes(x = gap_days, y = drug_label, fill = drug_label)
) +
  geom_density_ridges(scale = 1,bandwidth=2) +
  xlim(0, 200) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_bw() +
  #coord_cartesian(clip = "off") + 
  theme_bw(base_size = 14) +
  theme(legend.position = "none",axis.text.y = element_text(size = 10),
        axis.ticks.y = element_line(size = 0.4))+
  labs(
    x = "Interval (days)",         # <-- x-axis label
    y = ""                 # <-- y-axis label
  )
#print(gap_plot)
#ggsave(filename = "gap times.png", plot = gap_plot, width = 10, height = 5, dpi = 300)


fixed_effects_clean <- fixed_effects_sensi  +
  theme(
    axis.title.y = element_blank(),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Combine side by side, sharing y-axis
combined_plot <- gap_plot + fixed_effects_clean + plot_layout(ncol = 2, widths = c(1, 1))+plot_annotation(tag_levels = "A")

print(combined_plot)

#ggsave("fixed_effects_sensi.png", plot = fixed_effects_sensi, width = 10, height = 5, dpi = 300)



# Sensitivity-analysis truncating at 270 for MLM --------------------------

# Multi-level model -------------------------------------------------------
###############

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
  ungroup()%>%
  # Remove NA due to missing gap at last prescription (none after end date)
  drop_na(gap_days)%>%
  filter(script_date < "2026-01-01",
         gap_days<270)

df_gaps <- df_gaps %>%
  arrange(UniquePatientID, script_date) %>%
  group_by(UniquePatientID, script_date, atc_final) %>%
  mutate(
    numberofissues = max(numberofissues, na.rm = TRUE),
    gap_days = min(gap_days, na.rm = TRUE)
  ) %>%
  filter(row_number() == 1) %>%  # keep only one row per group (first row after arrange)
  ungroup() %>%
  filter(gap_days != 0)%>%
  mutate(numberofissues = as.numeric(numberofissues))%>%
  filter(!is.na(age) & !is.na(numberofissues))

all_results_270 <- list()

df_for_wtd<- data.frame()

# Outlier removal using z-score
vars_to_check <- c("age", "gap_days", "numberofissues")


if (analytics$on[analytics$methods == "multi-level"] == 1){
  for (i in seq_along(selected_drugs)) {
    drug_name <- selected_drugs[i]
    print(i)
    
    df_subset <- df_gaps %>%
      filter(str_detect(atc_final, drug_name),
      )%>%
      as.data.frame()
    
    # removing NA covariates
    df_subset <- df_subset %>%
      mutate(patienttype = as.factor(patienttype),
             atc_final = as.factor(atc_final),
             patienttype_grouped = case_when(
               grepl("GMS", patienttype) ~ "GMS",
               grepl("DVC", patienttype) ~ "DVC",
               TRUE ~ "Other"
             ))%>%
      filter(sex != 3)
    
    
    
    df_cleaned <- df_subset %>%
      filter(
        if_all(all_of(vars_to_check), ~ abs(scale(.)) <= 3)
      )
    
    # centering
    df_cleaned$age <- as.numeric(scale(df_cleaned$age, center = TRUE, scale = FALSE))
    df_cleaned$UniquePracticeID <- as.factor(df_cleaned$UniquePracticeID)
    df_cleaned$UniquePatientID  <- as.factor(df_cleaned$UniquePatientID)
    df_cleaned$sex              <- as.factor(df_cleaned$sex)
    df_cleaned$patienttype_grouped <- as.factor(df_cleaned$patienttype_grouped)
    
    df_for_wtd <- rbind(df_for_wtd,df_cleaned)
    
    
    # intercept only model
    null_model <- lmer(gap_days ~ (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned,REML = TRUE)
    #summary(null_model)
    
    # Level 1 slopes
    level1_model <- lmer(gap_days ~ numberofissues +  (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned,REML = TRUE)
    #summary(level1_model)
    
    # Level 1+2 slopes
    level2_model <- lmer(gap_days ~ numberofissues + age + sex + patienttype_grouped + (1|UniquePracticeID) + (1|UniquePatientID), data = df_cleaned,REML = TRUE)
    summary(level2_model)
    
    
    
    #tbl_null <- as.data.frame(broom.mixed::tidy(null_model, effects = "fixed", conf.int = TRUE))
    
    #tbl_1 <- as.data.frame(broom.mixed::tidy(level1_model, effects = "fixed", conf.int = TRUE))
    
    #tbl_2 <- as.data.frame(broom.mixed::tidy(level2_model, effects = "fixed", conf.int = TRUE))
    
    all_results_270[[drug_name]]<-list(
      drug = drug_name,
      level0 = null_model,
      level1 = level1_model,
      level2 = level2_model
    )
    
  }
  
  
  
  saveRDS(all_results_270, "all_results_270.rds")
  
}


all_results_tbls <- extract_tbls(all_results_270)



tbl_all <- lapply(names(all_results_tbls), function(drug) {
  all_results_tbls[[drug]]$tbl_level2 %>%
    mutate(drug = drug)
}) %>% bind_rows()

tbl_all <- tbl_all %>%
  mutate(drug_label = recode(drug, !!!atc_labels))


# Combine selected_drugs into a single regex pattern
pattern <- paste0("^(", paste(selected_drugs, collapse = "|"), ")")


null_models <- lapply(all_results_270, function(x) x$level0)
level2_models <- lapply(all_results_270, function(x) x$level2)

vpc_null <- lapply(null_models, function(m) icc(m, tolerance = 0, check_singularity = FALSE,by_group = TRUE))
vpc_level2 <- lapply(level2_models,function(m) icc(m, tolerance = 0, check_singularity = FALSE,by_group = TRUE))

combine_vpc <- function(vpc_list, model_name) {
  bind_rows(
    lapply(names(vpc_list), function(drug) {
      df <- vpc_list[[drug]]
      df$drug <- drug
      df$model <- model_name
      df
    })
  )
}

vpc_all <- bind_rows(
  combine_vpc(vpc_null, "null"),
  combine_vpc(vpc_level2, "level2")
)



# Example: create tidy VPC table for level2 models
vpc_plot_data_level_2 <- lapply(names(level2_models), function(drug) {
  m <- level2_models[[drug]]
  
  # Extract variance components
  vc <- as.data.frame(VarCorr(m)) %>%
    rename(Group = grp, Variance = vcov)
  
  vc$drug <- drug
  vc
}) %>%
  bind_rows() %>%
  mutate(Group = factor(Group, levels = c("UniquePracticeID", "UniquePatientID", "Residual")))

vpc_plot_data_level_2 <- vpc_plot_data_level_2 %>%
  group_by(drug) %>%
  mutate(Proportion = Variance) %>%
  ungroup()%>%
  mutate(drug_label = recode(drug, !!!atc_labels))

vpc_plot_data_level_0 <- lapply(names(null_models), function(drug) {
  m <- null_models[[drug]]
  
  # Extract variance components
  vc <- as.data.frame(VarCorr(m)) %>%
    rename(Group = grp, Variance = vcov)
  
  vc$drug <- drug
  vc
}) %>%
  bind_rows() %>%
  mutate(Group = factor(Group, levels = c("UniquePracticeID", "UniquePatientID", "Residual")))

vpc_plot_data_level_0 <- vpc_plot_data_level_0 %>%
  group_by(drug) %>%
  mutate(Proportion = Variance) %>%
  ungroup()%>%
  mutate(drug_label = recode(drug, !!!atc_labels))


vpc_plot_data_combined <- bind_rows(
  vpc_plot_data_level_2 %>% mutate(model = "Full model"),
  vpc_plot_data_level_0 %>% mutate(model = "Intercept only")
)
vpc_plot_data_combined$drug_label <- factor(vpc_plot_data_combined$drug_label,
                                            levels = levels(ordered_drugs))

vpc_plot_data_combined$model <- factor(vpc_plot_data_combined$model,)

var_plot <- ggplot(vpc_plot_data_combined, aes(x = Proportion, y = drug_label, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2",labels=c('GP practice', 'Patient','Residual'))  +
  facet_wrap(~factor(model,levels = c('Intercept only','Full model')), ncol = 2) +   # two panels side by side
  labs(
    title = "Variance Partition per Drug",
    x = expression(Variance~(days^2)),
    y = "",
    fill = "Level"
  ) +
  #scale_fill_discrete(labels=c('GP practice', 'Patient','Residual'))+
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12),
    axis.text.y = element_text(size = 10)
  )
print(var_plot)
ggsave(filename = "var_plot_270.png", plot = var_plot, width = 12, height = 5, dpi = 300)

tbl_all$drug_label <- factor(tbl_all$drug_label, levels = levels(ordered_drugs))


drug_levels <- levels(tbl_all$drug_label)

# Create alternating background info
bg_df <- data.frame(
  drug_label = factor(drug_levels, levels = drug_levels),
  ymin = seq_along(drug_levels) - 0.5,  # bottom of rectangle
  ymax = seq_along(drug_levels) + 0.5,  # top of rectangle
  fill = rep(c("grey95", "white"), length.out = length(drug_levels))  # alternating
)

fixed_effects <- ggplot() +
  # 1. Background rectangles first
  geom_rect(data = bg_df, 
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax, fill = fill),
            inherit.aes = FALSE) +
  # 2. Coefficients points and lines on top
  geom_linerange(data = tbl_all, 
                 aes(xmin = conf.low, xmax = conf.high, y = drug_label, color = term),
                 position = position_dodge(width = 0.5),
                 linewidth = 0.5, height = 0) +
  geom_point(data = tbl_all, 
             aes(x = estimate, y = drug_label, color = term),
             position = position_dodge(width = 0.5), size = 0.6) +
  scale_fill_identity() +  # use exact colors from bg_df$fill
  scale_color_bmj(labels = c('Intercept', 'Age','Number of issues','GMS:DVC', 'Other:DVC','Female')) +
  xlab("Estimate (days)") +
  ylab("") +
  theme_bw(base_size = 14) +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "right",
        legend.title = element_blank())


ggsave(filename = "fixed_effects_270.png", plot = fixed_effects, width = 10, height = 5, dpi = 300)

