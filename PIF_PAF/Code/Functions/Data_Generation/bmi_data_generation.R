### BMI DATA GENERATION ###

# This function is intended to read in HSE data for BMI data and clean it for use in PAF analysis and plotting.
# It requires functions from the hds_data_cleaning_functions.R script
# It requries one input:
#   data.filepath: this should be the filepath to the HSE data
# It outputs a dataframe with columns:
#   year: year of survey
#   gender: gender group considered
#   age_group: age_group considered
#   variable: bmi
#   level: what category of the variable the value describes
#   value: percentage of weighted survey population in the statum that take the value level

## Packages
necessary_packages <- c("dplyr", "yaml")
suppressMessages(
  for (p in necessary_packages) {
    if (!require(p, character.only = TRUE)){
      install.packages(p)
    }
    library(p, character.only = TRUE)
  }
)

## Function
bmi_data_gen <- function(filepath, user_options){
  
  ## Read in data dictionary
  ukds_dict <- read.csv(paste0(filepath, "/UKDS_Dictionary.csv"))
  
  ## Read in variable dictionary depending on user options
  if(user_options$age_groups_indicator){
    var_dict <- read_yaml(paste0(filepath, "/hse_variable_documentation.yaml"))
  } else {
    var_dict <- read_yaml(paste0(filepath, "/hse_variable_documentation_ages_all.yaml"))
  }
  
  # Filter to HSE datasets and filter to year range for bmi data, then extract UKDS datasets
  needed_ukds_data <- ukds_dict |>
    filter(Survey_Name == "HSE") |>
    pull(UKDS_Number)
  
  # Initialise output table
  output_df <- data.frame(year = numeric(0), age_group = character(0), sex = character(0), bmi = character(0), value = numeric(0))
  
  # If needed - add IMD column
  if(user_options$imd_stratification){
    output_df$imd <- character(0)
  }
  
  ## Read in UKDS Data and extract necessary data
  for(dataset_no in needed_ukds_data){
    
    # Get filename from dictionary also
    filename_data <- ukds_dict$Indiv_Dataset_Name[ukds_dict$UKDS_Number == dataset_no]
    
    # Get year of dataset
    ukds_data_temp_year <- ukds_dict$Year[ukds_dict$UKDS_Number == dataset_no]
    
    cat(paste0("Extracting data for ", ukds_data_temp_year, "...\n"))
    
    ## Find and clean bmi variable
    bmi_doc_found <- F
    for(i in 1:length(var_dict$bmi)){
      
      # Check if age_group specification is appropriate for the year specified
      if(check_year_spec(year = ukds_data_temp_year, year_spec = names(var_dict$bmi)[i])){
        
        bmi_doc_found <- T
        
        # Read in data
        ukds_data_temp <- read.delim(file = paste0(filepath, "/UKDA-", dataset_no, "-tab/tab/", filename_data), sep = "\t")
        
        # If year = 2000, care home participants need to be removed
        if(ukds_data_temp_year == 2000){ukds_data_temp <- filter(ukds_data_temp, wt_inst == -1)}
        
        # If year = 2002, boost samples should be removed
        if(ukds_data_temp_year == 2002){ukds_data_temp <- filter(ukds_data_temp, samptype == 2)}
        
        ## Find and clean base variables
        ukds_data_output_temp <- hse_base_variable_cleaning(ukds_data_temp, var_dict, ukds_data_temp_year, user_options)
        
        dict_varname <- var_dict$bmi[[i]]$varname
        
        # Intialise new variable
        ukds_data_output_temp[["bmi"]] <- NA
        
        # Loop through dictionary
        for(j in 1:length(var_dict$bmi[[i]]$dict)){
            
          # Get range of categories for level
          min_cat <- as.numeric(gsub("\\-.*", "", names(var_dict$bmi[[i]]$dict)[j]))
          max_cat <- as.numeric(gsub(".*\\-", "", names(var_dict$bmi[[i]]$dict)[j]))
          
          # For all in range, change new variable to level name
          ukds_data_output_temp[["bmi"]][between(as.numeric(ukds_data_temp[[dict_varname]]), min_cat, max_cat)] <- var_dict$bmi[[i]]$dict[[j]]
        
        }
        
      }
      
    }
    
    ## Find and clean bmi valid variable
    bmi_valid_doc_found <- F
    for(i in 1:length(var_dict$bmi_valid)){
      
      # Check if age_group specification is appropriate for the year specified
      if(check_year_spec(year = ukds_data_temp_year, year_spec = names(var_dict$bmi_valid)[i])){
        
        bmi_valid_doc_found <- T
        
        dict_varname <- var_dict$bmi_valid[[i]]$varname
        
        # Intialise new variable
        ukds_data_output_temp[["bmi_valid"]] <- NA
        
        # Loop through dictionary
        for(j in 1:length(var_dict$bmi_valid[[i]]$dict)){
          
          # For each level, change new variable to level name
          ukds_data_output_temp[["bmi_valid"]][ukds_data_temp[[dict_varname]] == names(var_dict$bmi_valid[[i]]$dict)[j]] <- var_dict$bmi_valid[[i]]$dict[[j]]
          
        }
        
      }
      
    }
    
    # warning if no bmi variable documentation found
    if(!bmi_doc_found){
      
      cat(paste0("No bmi variable documentation found for ", ukds_data_temp_year, "\n"))
      
    } else {
      
      # If bmi_valid variable found - report it and filter to that subset
      if(bmi_valid_doc_found){
        
        cat(paste0("bmi-valid documentation found for ", ukds_data_temp_year, "\n"))
        
        ukds_data_output_temp <- ukds_data_output_temp |>
          filter(bmi_valid == "Valid")
        
      }
      
      ## Remove NA
      ukds_data_output_temp <- ukds_data_output_temp |>
        na.omit()
      
      ## Tabulate - depending on if imd is a needed variable or not
      
      if(user_options$imd_stratification){
        
        # Tabulate bmi
        ukds_data_temp_table <- ukds_data_output_temp |>
          filter(imd>=1) |>
          count(age_group, sex, bmi, imd, wt = weight) |>
          group_by(age_group, sex, imd) |>
          mutate(value = n/sum(n)) |>
          select(-n) |>
          mutate(imd = as.character(imd))
        
        # Count participants per strata
        ukds_data_temp_table_n <- ukds_data_output_temp |>
          filter(imd>=1) |>
          count(age_group, sex, imd) |>
          rename(N = n) |>
          mutate(imd = as.character(imd))
        
        # Now merge together
        ukds_data_temp_table <- merge(ukds_data_temp_table, ukds_data_temp_table_n, by = c("age_group", "sex", "imd"))
        
      } else {
        
        # Tabulate bmi
        ukds_data_temp_table <- ukds_data_output_temp |>
          count(age_group, sex, bmi, wt = weight) |>
          group_by(age_group, sex) |>
          mutate(value = n/sum(n)) |>
          select(-n)
        
        # Count participants per strata
        ukds_data_temp_table_n <- ukds_data_output_temp |>
          count(age_group, sex) |>
          rename(N = n)
        
        # Now merge together
        ukds_data_temp_table <- merge(ukds_data_temp_table, ukds_data_temp_table_n, by = c("age_group", "sex"))
      }
      
      # Add year on
      ukds_data_temp_table$year <- as.numeric(ukds_data_temp_year)
      
      # relocate year to front
      ukds_data_temp_table <- ukds_data_temp_table |>
        relocate(year)
      
      ## Merge into main df
      output_df <- rbind(output_df, ukds_data_temp_table)
      
    }
    
  }
  
  
  ## Format output for use with other dataframes
  output_df <- output_df |>
    rename(level = bmi) |>
    mutate(variable = "bmi")
  
  ## Output df
  return(output_df)
  
  
}
