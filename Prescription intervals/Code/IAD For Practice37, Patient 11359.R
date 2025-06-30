##---##Getting Started##--------------------------------------------------------
library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)

view(prescriptions_subsample_170625)

##---##Isolating Statin Data----------------------------------------------------
df <- prescriptions_subsample_170625

codes <- c("C10AA07","C10AA01", "C10AA03", "C10AA05" )

statins <- df %>% 
  filter(atc_final %in% codes)
view(statins)

##---##Formatting Date##--------------------------------------------------------
statins <- statins %>% 
  mutate(script_date = as.Date(as.character(script_date), 
                               format = "%d%b%Y"))  

##---##Isolating one practice---------------------------------------------------

statinGP37 <- statins %>% 
  filter(UniquePracticeID == 37)

view(statinGP37)

##---##Isolating one patient----------------------------------------------------
one_patient <- statinGP37 %>%
  filter(UniquePatientID == 11359) %>%
  arrange(script_date) %>%
  mutate(gap_days = as.numeric(difftime(script_date, lag(script_date), units = "days")))


view(one_patient)

##---##Plot the gaps------------------------------------------------------------
ggplot(one_patient[-1, ], aes(x = script_date, y = gap_days)) +
  geom_col() +
  labs(title = "Number of days consecutive statin prescriptions for patient 11359 from practice 37",
       x = "Date",
       y = "Days Since Last Prescription")