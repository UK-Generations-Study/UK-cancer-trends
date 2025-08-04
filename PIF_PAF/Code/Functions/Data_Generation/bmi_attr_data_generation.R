### BMI attributable trends

# This code is written to obtain the trends in the BMI attributable cases. This will then be used in joinpoint regression

## Packages
library(dplyr)

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

## Reading in functions
source("rolling_average.R")
source("../PAF_Calculations/paf_byrf_function.R")


## Read in data

# Read in ESP weighted BMI data
data_bmi_esp <- read.csv("../../../../../Data/Cleaned_Data/clean_bmi_data_esp.csv") |>
  rolling_average()

# Read in all cancer incidence data 
data_ALLcancer <- read.csv("../../../Data/all_incidence_joinpoint.csv") |>
  filter(!(sex == "Men" & cancer_site == "Breast"))

# Filter data to only the cancer sites that are increasing 
data_cancer <- data_ALLcancer %>%
  filter(
    cancer_site %in% c("Oral", "Endometrium", "Pancreas", "Gallbladder", "Colorectum", "Liver", "Kidney", "MultipleMyeloma", "Breast", "Thyroid", "Ovary")
  ) |>
  mutate(cancer_site = if_else(cancer_site == "MultipleMyeloma", "Multiple Myeloma", cancer_site))


## Calcaulte PAFs
#Calculating the PAFs
pafs_byrf_under50<- paf_calculation(data_bmi_esp, "20-49")
pafs_byrf_over50<- paf_calculation(data_bmi_esp, "50+")
data_BMI_paf <-rbind(pafs_byrf_under50,pafs_byrf_over50) %>%
  filter(
    variable == "BMI"
  )%>%
  pivot_longer(
    cols = ends_with("PAF"),   
    names_to = "cancer_site",  
    values_to = "PAF"   
  ) %>%
  mutate(
    cancer_site = gsub("PAF", "",cancer_site),
    year = year + 10 # Applying 10 year time lag
  ) |>
  # Now filter out PAF past 2019
  filter(year <= 2019)

BMI_paf_cases <- data_BMI_paf %>%
  left_join(
    data_ALLcancer, by = c("year", "sex", "age_group", "cancer_site")
  ) %>%
  mutate(
    attributable = PAF * count, 
    attributable = ceiling(attributable),
    attributable_rate = PAF * Rate,
    non_attributable_rate = Rate - attributable_rate
  )

BMI_paf_cases <- BMI_paf_cases |>
  pivot_longer(cols = c("attributable_rate", "Rate", "non_attributable_rate"), names_to = "rate_type") |>
  mutate(rate_type = as.character(rate_type),
         sex = as.character(sex)) |>
  mutate(rate_type = if_else(rate_type == "Rate", "rate", rate_type)) |>
  mutate(cancer_site = if_else(cancer_site == "MultipleMyeloma", "Multiple Myeloma", cancer_site))


# Order and export
BMI_paf_cases <- BMI_paf_cases |>
  ungroup() |>
  filter(!is.na(value)) |>
  # group_by(cancer_site, rate_type, sex, age_group) |>
  # filter(!max(value) == 0) |>
  # ungroup() |>
  filter(!(cancer_site == "Breast" & age_group == "20-49")) |>
  filter(!(cancer_site == "Oral")) |>
  arrange(
   variable, cancer_site, rate_type, sex, age_group, year
  )
  # mutate(value = if_else(is.na(value), 0, value))

write.csv(BMI_paf_cases, "../../../../../Data/Joinpoint_Cleaned_Data/joinpoint_bmi_paf.csv", row.names = F)
