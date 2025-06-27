
library(tidyverse)
view(prescriptions_subsample_170625)


library(stringr)

# Filter for ATC codes that contain "C10A"
df <- prescriptions_subsample_170625
filtered_df <- df %>% 
  filter(str_detect(atc_final, "C10A"))

view(filtered_df)


#this code works as a partial search, so now if we combine it with the ATC altering code
#on the anticoagulant code string we can make our custom ATC codes and then search by those codes!