### EARLY ONSET CANCER SELECTION ###

# For the PIF/PAF paper, we are selecting cancers based on which cancers are identified as significantly increasing in u50 in the UK by the early onset paper.
# This code will pull from a local representation of the early_onset repo and will identify these cancers.

## Packages
library(dplyr)
library(gt)

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

## Read in data
load(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\early_onset\analysis_new_data\UKUS\ukus_final_df_long.RData)")


## Extract necessar data
# Extracting cancer sites that have a significant increase in u50 for men/women.
cancer_sites_df <- final_df_long |>
  filter(country == "UK" & agegrp == "u50") |>
  select(cancer, sex, aapc, CIlow, CIhigh) |>
  filter(CIlow > 0) |>
  mutate(
    
    CI = paste0("(", round(CIlow, digits = 2), ",", round(CIhigh, digits = 2), ")")
    
  ) |>
  select(cancer, sex, aapc, CI) |>
  arrange(cancer) |>
  mutate(
    
    sex = if_else(sex == "male", "Male", "Female")
    
  ) |>
  rename(`Cancer Site` = cancer, Sex = sex, AAPC = aapc)

# Print gt table
cancer_sites_gt <- cancer_sites_df |>
  gt() |>
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels()
  )

gtsave(cancer_sites_gt, filename = "../../Output/Initial_Data_Evaluation/early_onset_cancer_selection.png")

# Output dataframe to .csv
write.csv(cancer_sites_df, file = "../../Output/Initial_Data_Evaluation/early_onset_cancer_selection.csv", row.names = F)