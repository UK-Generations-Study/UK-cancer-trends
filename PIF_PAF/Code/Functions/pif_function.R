# This is the PIF calculation function 

# Function: 

# Inputs:

# Outputs: 

pif_calculation <- function(dataframe) {
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
  
  
  ## TEMPORARY - removing imputed data from alcohol
  data_paf <- dataframe |>
    filter(!(variable == "Alcohol" & between(year, 2006, 2010))) |>
    mutate(BreastPAF = if_else(sex == "Men", 0, BreastPAF))
  
  ## Filter to data we need
  data_paf <- data_paf |>
    # mutate(year = year + 10) |>
    filter(!grepl("\\_mean$|\\_median$", variable)) |>
    # Removing other physical_activity variable
    filter(variable != "physical_activity") |>
    group_by(variable, sex, age_group) |>
    mutate(
      
      dist_to_2009 = abs(year - 2009)
      
    ) |>
    mutate(
      
      youngest_indicator = year == max(year),
      oldest_indicator = year == min(year),
      close_to_2009 = dist_to_2009 == min(dist_to_2009)
      
    ) |>
    filter(youngest_indicator | close_to_2009 | oldest_indicator) |>
    ungroup() |>
    pivot_longer(cols = colnames(data_paf)[!colnames(data_paf) %in% c("sex", "year", "age_group", "variable")], names_to = "cancer_site", values_to = "PAF") |>
    mutate(
      
      cancer_site = gsub("PAF$", "", cancer_site),
      
      PIF_component = 1 + PAF/(1-PAF)
      
    ) |>
    group_by(sex, age_group, variable, cancer_site) |>
    arrange(year) |>
    summarise(
      
      PIF_past = if_else(PIF_component[1] == 1 | PIF_component[2] == 1, NA, (PIF_component[2] - PIF_component[1])/PIF_component[2]),
      
      PIF_future = if_else(PIF_component[2] == 1 | PIF_component[3] == 1, NA, (PIF_component[3] - PIF_component[2])/PIF_component[3])
      
    )
  
  return(data_paf)
}