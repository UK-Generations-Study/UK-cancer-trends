# This code cleans England ICD Incidence Data for JoinPoint usage #

# Packages
library(dplyr)

# Set working directory
# Setting up wd for relative file paths
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

# Read in data
# Assumed working directory is same relationship as is set up in the Github
data_u50 <- read.csv(r"(..\..\..\Data\Incidence_data_for_England_2025-04-07.csv)")
data_oe50 <- read.csv(r"(..\..\..\Data\Incidence_data_for_England_2025-04-07 (1).csv)")

# Format data
data <- rbind(data_u50 |>
                mutate(age_group = "20-49"),
              data_oe50 |>
                mutate(age_group = "50+"))

# Create cancer site groupings
data <- data |>
  mutate(
    
    cancer_site = case_when(
      ICD10_code == "C67" ~ "Bladder",
      ICD10_code %in% c("C70", "C71", "C72") ~ "Brain and CNS",
      ICD10_code == "C50" ~ "Breast",
      ICD10_code == "C53" ~ "Cervix",
      ICD10_code %in% c("C18-C20", "C21") ~ "Colorectum",
      ICD10_code == "C54" ~ "Endometrium",
      ICD10_code %in% c("C23", "C24") ~ "Gallbladder",
      ICD10_code == "C81" ~ "Hodgkin Lymphoma",
      ICD10_code == "C46" ~ "Kaposi Sarcoma",
      ICD10_code == "C64" ~ "Kidney",
      ICD10_code == "C32" ~ "Larynx",
      ICD10_code == "C91-C95" ~ "Leukaemia",
      ICD10_code == "C22" ~ "Liver",
      ICD10_code == "C33-C34" ~ "Lung",
      ICD10_code == "C43" ~ "Melanoma",
      ICD10_code %in% c("C88", "C90") ~ "MultipleMyeloma",
      ICD10_code %in% c("C82-C86", "C96") ~ "Non-Hodgkin Lymphoma",
      ICD10_code == "C15" ~ "Oesophagus",
      ICD10_code == "C00-C14" ~ "Oral",
      ICD10_code == "C56" ~ "Ovary",
      ICD10_code == "C25" ~ "Pancreas",
      ICD10_code == "C61" ~ "Prostate",
      ICD10_code == "C16" ~ "Stomach",
      ICD10_code == "C62" ~ "Testis",
      ICD10_code == "C73" ~ "Thyroid",
      TRUE ~ NA
    ),
    
    Year = as.numeric(Year),
    
    Rate = if_else(is.na(Rate) | Rate == "[u]", 0, as.numeric(Rate))
    
  ) |>
  filter(!is.na(cancer_site)) |>
  filter(Gender != "Persons") |>
  filter(Year <= 2019) |>
  mutate(
    
    Gender = if_else(Gender == "Male", "Men", "Women")
    
  ) |>
  select(year = Year, sex = Gender, age_group, cancer_site, Rate, Count) |>
  group_by(year, sex, age_group, cancer_site) |>
  summarise(Rate = sum(as.numeric(Rate)), 
            count = sum(as.numeric(Count))) |>
  arrange(
    sex, age_group, cancer_site, year
  )


# Output
write.csv(data, r"(..\..\..\Data\all_incidence_joinpoint.csv)", row.names = F)
