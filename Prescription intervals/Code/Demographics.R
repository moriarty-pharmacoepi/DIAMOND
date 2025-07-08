##---Packages
library(tidyverse)
library(dplyr)     
library(lubridate)  
library(ggplot2)

##---Load File
demographics <- read.csv("/Users/padraicdonoghue/Library/CloudStorage/OneDrive-SharedLibraries-RoyalCollegeofSurgeonsinIreland/Frank Moriarty - RSS 2025/demographics_subsample.csv")
#view(demographics)

##---Filter By scheme
demographicsGMS <- demographics %>% 
  filter(scheme == "GMS")%>% 
  filter(status == "Active")
#view(demographicsGMS)

demographicsPRIV <- demographics %>% 
  filter(scheme == "Private") %>% 
  filter(status == "Active")
#view(demographicsPRIV)


