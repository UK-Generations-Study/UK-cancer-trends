### CANCER INCIDENCE DATA GENERATION ###

# This code is intended to read in cancer incidence data and clean it for use in PAF analysis and plotting.
# It outputs a dataframe with columns:
#   year: year of diagnosis
#   gender: gender group considered
#   age_group: age_group considered
#   cancer_site: cancer site (globocan definitions)
#   count: total cancer incidences when stratified by the other columns

## Packages
necessary_packages <- c("dplyr")
suppressMessages(
  for (p in necessary_packages) {
    if (!require(p, character.only = TRUE)){
      install.packages(p)
    }
    library(p, character.only = TRUE)
  }
)

## Set working directory
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

  
# Read in data - filepath should point to where data is located
data_inc_u50 <- read.csv("../../Data/Incidence_data_for_England_2024-10-30.csv")
data_inc_oe50 <- read.csv("../../Data/Incidence_data_for_England_2024-10-30 (1).csv")

# Merge and clean data
data_inc <- rbind(data_inc_u50, data_inc_oe50) |>
  mutate(
    
    # Simplify age group names
    age_group = if_else(Age_at_Diagnosis == "20 to 24, 25 to 29, 30 to 34, 35 to 39, 40 to 44, 45 to 49", "20-49", "50+"),
    
    # Change sex labelling
    sex = case_when(Gender == "Male" ~ "Men",
                    Gender == "Female" ~ "Women",
                    TRUE ~ "All"),
    
    # group by globocan specified ICD10 codes for the cancer sites desired
    # globocan sites by ICD10 code here: https://gco.iarc.fr/overtime/en/database#cancer-dictionary
    cancer_site_globocan = case_when(
      ICD10_code == "C50" ~ "Breast",
      ICD10_code %in% c("C18", "C19", "C20", "C21") ~ "Colorectum",
      ICD10_code == "C54" ~ "Endometrium",
      ICD10_code %in% c("C23", "C24") ~ "Gallbladder",
      ICD10_code == "C64" ~ "Kidney",
      ICD10_code == "C22" ~ "Liver",
      ICD10_code %in% c("C88", "C90") ~ "MultipleMyeloma",
      ICD10_code == "C00-C14" ~ "Oral",
      ICD10_code == "C25" ~ "Pancreas",
      ICD10_code == "C73" ~ "Thyroid",
      TRUE ~ NA
    )
    
  ) |>
  filter(!is.na(cancer_site_globocan)) |>
  filter(!(cancer_site_globocan == "Breast" & sex == "Men")) |>
  filter(sex != "All") |>
  group_by(Year, sex, age_group, cancer_site_globocan) |>
  summarise(count = sum(Count)) |>
  rename(year = Year, cancer_site = cancer_site_globocan) |>
  arrange(sex, age_group, cancer_site, year) |>
  filter(year <= 2019)

# Return clean data
write.csv(data_inc, file = "../../Data/incidence_data_clean.csv", row.names = F)
