## CALCULATE PIFS ##

# This code is to calculate PIFs for relevant risk factor/cancer site combinations and output the results for plotting.

## Packages needed
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

## Read in data
data_paf <- read.csv("../../Data/PAF_by_riskfactor.csv")

## Filter to data we need
data_paf <- data_paf |>
  mutate(year = year + 10) |>
  filter(!grepl("\\_mean$", variable)) |>
  filter(year %in% c(2015, 2019)) |>
  pivot_longer(cols = colnames(data_paf)[!colnames(data_paf) %in% c("sex", "year", "age_group", "variable")], names_to = "cancer_site", values_to = "PAF") |>
  mutate(
    
    cancer_site = gsub("PAF$", "", cancer_site),
    
    PIF_component = 1 + PAF/(PAF-1)
    
  ) |>
  group_by(sex, age_group, variable, cancer_site) |>
  arrange(year) |>
  summarise(
    
    PIF = if_else(PAF[1] < PAF[2],
                  (PIF_component[1] - PIF_component[2])/PIF_component[1],
                  NA)
  )
  
  
## Output PIF
write.csv(data_paf, file = "../../Data/PIF.csv", row.names = F)