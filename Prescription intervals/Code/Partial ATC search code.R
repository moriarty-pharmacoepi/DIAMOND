
library(tidyverse)
df <- read.csv("/Users/padraicdonoghue/Library/CloudStorage/OneDrive-SharedLibraries-RoyalCollegeofSurgeonsinIreland/Frank Moriarty - RSS 2025/prescriptions_subsample_170625.csv")


library(stringr)

# Filter for ATC codes that contain "C10A"

filtered_df <- df %>% 
  filter(str_detect(atc_final, "C10AA"))
view(filtered_df)


#this code works as a partial search, so now if we combine it with the ATC altering code
#on the anticoagulant code string we can make our custom ATC codes and then search by those codes!