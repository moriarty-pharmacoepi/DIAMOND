library(easyPubMed)
library(readr)
library(XML)
library(tidyverse)
library(rstudioapi)


# Get the path of the current script
script_path <- rstudioapi::getActiveDocumentContext()$path
print(script_path)

# Set the working directory to the script's location
setwd(dirname(script_path))
# homepath (assumes an R project in the appropriate parent folder)

path <- getwd() 
#data_path <- paste0(path, "/data")

query <- '"Pharmacoepidemiology and drug safety"[Journal] AND
(("2022/12/31"[Date - Publication] : "2024/10/31"[Date - Publication]))'


#my_entrez_id <- get_pubmed_ids(my_query)
# Submit the Query
epm <- epm_query(query) 

# Retrieve Records (xml format)
epm <- epm_fetch(epm, format = 'xml')

# Extract Information
epm <- epm_parse(epm)

# Raw records is a large list of articles
raw_records <- get_epm_raw(epm)

# elements are named after the corresponding PMIDs
head(names(raw_records))
# elements include raw PubMed records
first_record <- raw_records[[1]] 

#########################################
# Get the first raw record
first_record <- epm@raw[["37690792"]] 


# Initialize an empty list to store results
publication_data <- list()

# loop over all articles
for (i in 1:length(raw_records)) {
  # Split the raw record into lines
  lines <- unlist(strsplit(epm@raw[[i]], "\n"))
  
  # Extract PT (publication type) lines and clean up
  matches <- gregexpr("<PublicationType UI=\"[^\"]+\">[^<]+</PublicationType>", lines)
  publication_tags <- regmatches(lines, matches)[[1]]
  
  # get all content between the ><
  content <- sub('.*>([^<]+)<.*', '\\1', publication_tags[1])
  
  # Store the results
  publication_data[[i]] <- data.frame(pmid = names(raw_records)[i], publication_types = paste(content, collapse = ", "))
}

# Combine all results into a data frame
df_publication_types <- do.call(rbind, publication_data)

# get processed data
proc_data <- get_epm_data(epm)

# all_data is our formatted table of articles
all_data <-merge(proc_data,df_publication_types,by="pmid")

# show an excerpt (first 6 records, selected columns)
fields <- c('pmid',
            'doi',
            'title',
            'jabbrv',
            'year',
            'month',
            'day',
            'publication_types')

head(all_data[, fields])

# Store the data in a CSV file
write_csv(all_data[,fields], "all_pubs.csv")

# Generate a new XML document for Covidence importing
doc <- newXMLDoc()
root <- newXMLNode("xml", doc = doc)

# Create a new root node "records"
records <- newXMLNode("records")

# Loop through each row in all_pubs to create XML nodes
for (i in seq_len(nrow(all_data))) {
  
  # Create a new record node for each row
  record <- newXMLNode("record", parent = records)
  
  # Add reference type
  newXMLNode("ref-type", "17", attrs = c(name = str_to_title(all_data$publication_types[i])), parent = record)
  # Add contributors
  contributors <- newXMLNode("contributors", parent = record)
  authors_node <- newXMLNode("authors", parent = contributors)
  authors <- unlist(strsplit(all_data$authors[i], ";")) # Split authors by semicolon
  
  # Loop over each author and create a new author node
  for (author in authors) {
    newXMLNode("author", author, parent = authors_node)
  }
  # Add titles
  titles <- newXMLNode("titles", parent = record)
  newXMLNode("title", all_data$title[i], parent = titles)
  
  # Add periodical (journal)
  periodical <- newXMLNode("periodical", parent = record)
  newXMLNode("full-title", all_data$journal[i], parent = periodical)
  
  # Add publication year
  dates <- newXMLNode("dates", parent = record)
  newXMLNode("year", all_data$year[i], parent = dates)
  
  # Add DOI if available
  if (!is.na(all_data$doi[i]) && all_data$doi[i] != "") {
    newXMLNode("electronic-resource-num", all_data$doi[i], parent = record)
  }
  
  # Add PMID if available
  if (!is.na(all_data$pmid[i]) && all_data$pmid[i] != "") {
    newXMLNode("accession-num", all_data$pmid[i], parent = record)
  }
  
  # Add abstract if available
  if (!is.na(all_data$abstract[i]) && all_data$abstract[i] != "") {
    newXMLNode("abstract", all_data$abstract[i], parent = record)
  }
}

# Save the XML document to a file
xml_filename <- "search_result_from_query.xml"
saveXML(records, xml_filename)


# Importing PMIDs from Tazare et al. --------------------------------------
file_url <- "https://raw.githubusercontent.com/ehr-lshtm/code-sharing/main/data/clean_results.csv"

# Define the local destination file name
dest_file <- "clean_results.csv"

# Download the file
download.file(file_url, destfile = dest_file)

results_tazare <- read.csv('clean_results.csv')


filtered_data <- results_tazare %>%
  filter(simulation == "Yes")

# show an excerpt (first 6 records, selected columns)
fields <- c('pmid',
            'doi',
            'AUTHORS',
            'TITLE',
            'ABSTRACT',
            'article_type_pds')

simulated_data <- filtered_data[,fields]

doc <- newXMLDoc()
root <- newXMLNode("xml", doc = doc)

# Create a new XML document with a root node "records"
records <- newXMLNode("records")

# Loop through each row in all_pubs to create XML nodes
for (i in seq_len(nrow(simulated_data))) {
  
  # Create a new record node for each row
  record <- newXMLNode("record", parent = records)
  
  # Add reference type
  newXMLNode("ref-type", "17", attrs = c(name = str_to_title(simulated_data$article_type_pds[i])), parent = record)
  # Add contributors
  contributors <- newXMLNode("contributors", parent = record)
  authors_node <- newXMLNode("authors", parent = contributors)
  authors <- unlist(strsplit(simulated_data$AUTHORS[i], ";")) # Split authors by semicolon
  for (author in authors) {
    newXMLNode("author", author, parent = authors_node)
  }
  # Loop over each author and create a new author node
 
  titles <- newXMLNode("titles", parent = record)
  newXMLNode("title", simulated_data$TITLE[i], parent = titles)
  
  # Add DOI if available
  if (!is.na(simulated_data$doi[i]) && simulated_data$doi[i] != "") {
    newXMLNode("electronic-resource-num", simulated_data$doi[i], parent = record)
  }
  
  # Add PMID if available
  if (!is.na(simulated_data$pmid[i]) && simulated_data$pmid[i] != "") {
    newXMLNode("accession-num", simulated_data$pmid[i], parent = record)
  }
  
  # Add abstract if available
  if (!is.na(simulated_data$ABSTRACT[i]) && simulated_data$ABSTRACT[i] != "") {
    newXMLNode("abstract", simulated_data$ABSTRACT[i], parent = record)
  }
}

# Save the XML document to a file
xml_filename <- "Tazare et al.xml"
saveXML(records, xml_filename)
