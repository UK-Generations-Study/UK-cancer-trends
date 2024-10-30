### TIME FRAME ESTIMATION ###

# This code is designed to help us select a suitable time frame from which to analyse the cancer trends over.
# It uses the cancer sites selected via early_onset paper and causal risk factor selection and looks at the time lengths of most recent trends by AAPC.

## Packages


## Set working directory
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
selected_sites <- read.csv("../../Output/Initial_Data_Evaluation/early_onset_cancer_selection.csv")
jp_df <- read.csv(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\early_onset\analysis_new_data\UKUS\2.joinpoint\JP_data\export.Export.APC.txt)")

## Cancer site filtering
# Here we are filtering the list of cancer sites to select only those that have causal risk factors we can analyse the trends of
# The causal risk factors are assessed using IARC and WCRF

selected_sites <- unique(selected_sites$Cancer.Site)
selected_sites

# Sites with no causal rf
sites_no_rf <- c("Hodgkin lymphoma", "Brain and CNS", "Melanoma of skin")

selected_sites <- selected_sites[-which(selected_sites %in% sites_no_rf)]
selected_sites

## Extract most recent trend for selected sites
jp_df <- jp_df |>
  mutate(
    
    Cancer.label_new = case_when(
      Cancer.label == "Cervix uteri" ~ "Cervix",
      Cancer.label == "Corpus uteri" ~ "Endometrium",
      Cancer.label == "Lip oral cavity pharynx" ~ "Oral",
      TRUE ~ Cancer.label
    )
    
  ) |>
  filter(Country.label == "UK" & agegrp == "20to49" & Cancer.label_new %in% selected_sites)

# Check no sites are missing
setdiff(selected_sites, unique(jp_df$Cancer.label_new))

# Group by site, sex and take most recent trend
jp_df <- jp_df |>
  filter(Model == Segment) |>
  mutate(
    
    Segment.Length = Segment.End - Segment.Start
    
  )

# Print summary of most recent trends
table(jp_df$Segment.Length)
summary(jp_df$Segment.Length)
