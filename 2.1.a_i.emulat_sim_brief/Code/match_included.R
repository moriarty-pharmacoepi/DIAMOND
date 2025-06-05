library(readr)
library(rstudioapi)
library(dplyr)

# Get the path of the current script
script_path <- rstudioapi::getActiveDocumentContext()$path
print(script_path)

# Set the working directory to the script's location
setwd(dirname(script_path))
included_data_filename <- "review_522747_included_csv_20250411194606.csv"

# read in csv file of included data from Covidence
included_data <- read.csv(included_data_filename)

# get PMIDs of included data
included_pmids <- as.numeric(included_data$"Accession.Number")
included_pmids <- as.data.frame(included_pmids)
colnames(included_pmids)<-"pmid" 

# read in Tazare's clean data
results_tazare <- read.csv('clean_results.csv')

# read in the search data from Pubmed search by Muddiman
results_muddiman <- read.csv('all_pubs.csv')

# Data from Tazare included by Muddiman
included_partial_1 <-merge(x=included_pmids,y=results_tazare,by="pmid")
included_partial_1 <- included_partial_1[, c("pmid", "doi", "TITLE","year","article_type_pds")]
colnames(included_partial_1) <-c("pmid", "doi", "title","year","publication_types")
Tazare_sims <- results_tazare[results_tazare["simulation"]=="Yes",]

#Data from new search included by Muddiman
included_partial_2 <-merge(x=included_pmids,y=results_muddiman,by="pmid")
included_partial_2 <- included_partial_2[, c("pmid", "doi", "title","year","publication_types")]

# joining both individual searches
included_final <-rbind(included_partial_1,included_partial_2)

# There is a discrepancy betwen article classification, making column name similar
included_final <- included_final %>%
  mutate(publication_types = ifelse(publication_types == "Original Article", "Journal Article", publication_types))

# this is a mislabeled article, labelled as meta-analysis but its an article
included_final <- included_final %>%
  mutate(publication_types = ifelse(publication_types == "Meta-Analysis", "Journal Article", publication_types))

# this is a mislabeled article, labelled as article but its a brief report
included_final <- included_final %>%
  mutate(publication_types = ifelse(pmid == 30375122, "Brief report", publication_types))

# this is a mislabeled article, labelled as article but its a brief report
included_final <- included_final %>%
  mutate(publication_types = ifelse(pmid == 38450934, "Brief report", publication_types))

# get the excluded articles from Tazare (screening and full-text)
excluded_tazare <- anti_join(y=included_partial_1,x=Tazare_sims, by = c("pmid"))

# get the excluded articles from Muddiman (screening and full-text)
excluded_muddiman <- anti_join(y=included_partial_2,x=results_muddiman, by = c("pmid"))

# Save included data to a CSV file
#write_csv(included_final, "included_final.csv")