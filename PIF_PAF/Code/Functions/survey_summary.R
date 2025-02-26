## DATASET SUMMARY ##

# This code is designed to move through datasets from UKDS used in the analysis, and extract sample sizes from the surveys.

# Set working directory
# This sets wd to wherever the document is saved - this should be the github desktop folder
if(Sys.getenv("RSTUDIO") == '1' & !knitr::is_html_output()) { # If using Rstudio and not rendering
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else if(Sys.getenv("RSTUDIO") != '1'){ # If using Rscript
  initial.options <- commandArgs(trailingOnly = FALSE)
  file.arg.name <- "--file="
  script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
  script.basename <- dirname(script.name)
  setwd(file.path(getwd(), script.basename))
}

# Packages
library(dplyr)

# Read in dictionary of UKDS data
dict <- read.csv("../../Documentation/UKDS_Dictionary.csv")

# Loop through HSE datasets
hse_ukds <- dict |>
  filter(Survey_Name == "HSE") |>
  pull(UKDS_Number)

for(hse_number in hse_ukds){
  
  # Get year
  year <- dict$Year[dict$UKDS_Number == hse_number]
  
  # Get filename
  filename <- dict$Indiv_Dataset_Name[dict$UKDS_Number == hse_number]
  
  # Read in file
  data <- read.delim(file = paste0("../../../../Data/UKDA-", hse_number, "-tab/tab/", filename))
  
  # Extract N
  N <- nrow(data)
  
  # Print statement including N
  cat(paste0("HSE - ", year, ": N = ", N, "\n"))
  
}

# Loop through UKDS datasets
hse_ndns <- dict |>
  filter(Survey_Name == "NDNS") |>
  pull(Year)

ndns_number <- dict |>
  filter(Survey_Name == "NDNS") |>
  pull(UKDS_Number) |>
  unique()

for(year_range in hse_ndns){
  
  # Get year range
  spec_years <- as.numeric(gsub("\\-.*","",year_range)):as.numeric(gsub(".*\\-","",year_range))
  
  # Get filename
  filename <- dict$Indiv_Dataset_Name[dict$Year == year_range]
  
  # Read in file
  data <- read.delim(file = paste0("../../../../Data/UKDA-", ndns_number, "-tab/tab/", filename))
  
  colnames(data) <- tolower(colnames(data))
  
  # Loop through years and extract N
  for(y in spec_years){
    
    data_spec <- data |>
      filter(surveyyear == y - 2007)
    
    # Extract N
    N <- nrow(data_spec)
    
    # Print statement including N
    cat(paste0("NDNS - ", y, ": N = ", N, "\n"))
    
  }
  
}
