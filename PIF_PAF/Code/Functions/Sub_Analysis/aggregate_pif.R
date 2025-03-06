### CALCULATING AGGREGATE PIF FOR BMI ###

# This code is written to calculate an aggregate PIF value for the % of cancer cases in the cancer sites of interest are caused by trends in BMI since 1995

# Set working directory to same wd as the qmd
if(Sys.getenv("RSTUDIO") == '1' & !knitr::is_html_output()) { # If using Rstudio and not rendering
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else if(Sys.getenv("RSTUDIO") != '1'){ # If using Rscript
  initial.options <- commandArgs(trailingOnly = FALSE)
  file.arg.name <- "--file="
  script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
  script.basename <- dirname(script.name)
  setwd(file.path(getwd(), script.basename))
}

setwd("../../")

# Packages
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(gt)

# Read in functions
source("Functions/PAF_Calculations/pif_function.R")
source("Functions/Data_Generation/rolling_average.R")
source("Functions/PAF_Calculations/paf_byrf_function.R")
source("Functions/PAF_Calculations/paf_aggregate_function.R")

# Read in data

# Read in clean risk factor trends split by age
data_rf <- read.csv("../../../Data/Cleaned_Data/clean_rf_data.csv") |>
  rolling_average()

data_ALLcancer <- read.csv("../Data/all_incidence_joinpoint.csv") |>
  filter(!(sex == "Men" & cancer_site %in% c("Breast", "Endometrium")))

# Calculate the PAFs by cancer site 
pafs_byrf_under50<- paf_calculation(data_rf, "20-49")
pafs_byrf_over50<- paf_calculation(data_rf, "50+")
data_paf_individual<-rbind(pafs_byrf_under50,pafs_byrf_over50)

# Calculate PIFs
PIF <- pif_calculation_bmi(data_paf_individual) |>
  filter(!(sex == "Men" & cancer_site == "Breast")) |>
  mutate(PIF_past = if_else(is.na(PIF_past), 0, PIF_past))

# Edit cancer sites down to cancer sites of interest
data_cancer <- data_ALLcancer |>
  filter(cancer_site %in% c("Oral", "Endometrium", "Pancreas", "Gallbladder", "Colorectum", "Liver", "Kidney", "MultipleMyeloma", "Breast", "Thyroid")) |>
  filter(year == 2019)

# Combine datasets to calculate an aggregate PIF
data_pif <- merge(PIF, data_cancer, by = c("cancer_site", "sex", "age_group"), all.x = T) |>
  mutate(count = if_else(is.na(count), 0, count)) |>
  mutate(attr_cases = ceiling(count*PIF_past)) |>
  mutate(PIF_past=  round(PIF_past, digits = 4)) |>
  select(cancer_site, age_group, sex, PIF_past, attr_cases)

data_pif |>
  gt()

write.csv(data_pif, r"(../Data/aggregate_pif.csv)")
