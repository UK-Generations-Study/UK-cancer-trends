### HSE DATA CLEANING FUNCTIONS ###

# Here are listed functions used in the general cleaning of HSE Data

### CHECK YEAR SPEC
# This function is intended to check if the year of the dataset we are looking at falls within the specification years of the variable documentation

check_year_spec <- function(year, year_spec){
  
  result <- case_when(
    grepl("\\-", year_spec) ~ between(as.numeric(year), left = as.numeric(gsub("\\-.*", "", year_spec)), right = as.numeric(gsub(".*\\-", "", year_spec))), # IF year range - check it falls in year range
    TRUE ~ year == year_spec # Otherwise, precise year must be the same
  )
  
  return(result)
  
}

### 2004 HSE WEIGHTING FIX FUNCTION
fix_hse_2004_weighting <- function(data){
  
  data <- data |>
    mutate(N = case_when(
      year == 2004 ~ N/14.15683,
      TRUE ~ N
    ))
  
  return(data)
  
}


### AGE/SEX/WEIGHT VARIABLE GENERATION
# activity_data_toggle is used for 2006 activity data which uses a different weighting variable than other variables in the 2004 HSE survey

hse_base_variable_cleaning <- function(ukds_data_temp, var_dict, ukds_data_temp_year, user_options, activity_data_toggle = F){
  
  # Initialise empty new dataframe
  ukds_data_output_temp <- as.data.frame(matrix(nrow = nrow(ukds_data_temp), ncol = 0))

  ## Find and clean age variable
  age_group_doc_found <- F
  for(i in 1:length(var_dict$age_group)){
    
    
    # Check if age_group specification is appropriate for the year specified
    if(check_year_spec(year = ukds_data_temp_year, year_spec = names(var_dict$age_group)[i])){
      
      age_group_doc_found <- T
      
      dict_varname <- var_dict$age_group[[i]]$varname
      
      # Intialise new variable
      ukds_data_output_temp[["age_group"]] <- NA
      
      # Loop through dictionary
      for(j in 1:length(var_dict$age_group[[i]]$dict)){
        
        # Get range of categories for level
        min_cat <- as.numeric(gsub("\\-.*", "", names(var_dict$age_group[[i]]$dict)[j]))
        max_cat <- as.numeric(gsub(".*\\-", "", names(var_dict$age_group[[i]]$dict)[j]))
        
        # For all in range, change new variable to level name
        ukds_data_output_temp[["age_group"]][between(as.numeric(ukds_data_temp[[dict_varname]]), min_cat, max_cat)] <- var_dict$age_group[[i]]$dict[[j]]
      }
      
    }
    
  }
  # stop if no age_group variable documentation found
  if(!age_group_doc_found){stop(paste0("No age_group variable documentation found for ", ukds_data_temp_year))}
  
  ## Find and clean sex variable
  sex_doc_found <- F
  for(i in 1:length(var_dict$sex)){
    
    # Check if age_group specification is appropriate for the year specified
    if(check_year_spec(year = ukds_data_temp_year, year_spec = names(var_dict$sex)[i])){
      
      sex_doc_found <- T
      
      dict_varname <- var_dict$sex[[i]]$varname
      
      # Intialise new variable
      ukds_data_output_temp[["sex"]] <- NA
      
      # Loop through dictionary
      for(j in 1:length(var_dict$sex[[i]]$dict)){
        
        # For each level, change new variable to level name
        ukds_data_output_temp[["sex"]][ukds_data_temp[[dict_varname]] == names(var_dict$sex[[i]]$dict)[j]] <- var_dict$sex[[i]]$dict[[j]]
        
      }
      
    }
    
  }
  # stop if no sex variable documentation found
  if(!sex_doc_found){stop(paste0("No sex variable documentation found for ", ukds_data_temp_year))}
  

  
  ## Find and clean weight variable
  weight_doc_found <- F
  for(i in 1:length(var_dict$weight)){
    
    # Check if age_group specification is appropriate for the year specified
    if(check_year_spec(year = ukds_data_temp_year, year_spec = names(var_dict$weight)[i])){
      
      weight_doc_found <- T
      
      dict_varname <- var_dict$weight[[i]]$varname
      
      # Intialise new variable
      ukds_data_output_temp[["weight"]] <- ukds_data_temp[[dict_varname]]
      
      # Check for 2006 activity data - need to change weight variable
      if(activity_data_toggle & ukds_data_temp_year == 2006){
        ukds_data_output_temp[["weight"]] <- ukds_data_temp[["wt_int_s2"]]
      }
      
    }
    
  }
  # create one with all 1 if no weight variable documentation found
  if(!weight_doc_found){ukds_data_output_temp[["weight"]] <- 1}
  
  
  ## Find and clean IMD variable if desired
  if(user_options$imd_stratification){
    
    imd_doc_found <- F
    for(i in 1:length(var_dict$imd)){
      
      # Check if imd specification is appropriate for the year specified
      if(check_year_spec(year = ukds_data_temp_year, year_spec = names(var_dict$imd)[i])){
        
        imd_doc_found <- T
        
        dict_varname <- var_dict$imd[[i]]$varname
        
        # Intialise new variable
        ukds_data_output_temp[["imd"]] <- ukds_data_temp[[dict_varname]]
        
      }
      
    }
    
    # Report if no IMD found
    if(!imd_doc_found){
      cat(paste0("No imd variable documentation found for ", ukds_data_temp_year, "\n"))
      ukds_data_output_temp[["imd"]] <- "All"
    }
    
  }
  
  
  ## Output current dataframe
  return(ukds_data_output_temp)
  

}

