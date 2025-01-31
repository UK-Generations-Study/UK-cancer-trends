### DIET DATA GENERATION ###

# This function is intended to read in NDNS data and clean it for use in PAF analysis and plotting.
# It requries one input:
#   data.filepath: this should be the filepath to the HSE data
# It outputs a dataframe with columns:
#   year: year of survey
#   gender: gender group considered
#   age_group: age_group considered
#   variable: fibre_rec, red_meat, dairy
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
diet_data_gen_cat_mean <- function(filepath, user_options){
  
  ## Read in data dictionary
  ukds_dict <- read.csv(paste0(filepath, "/UKDS_Dictionary.csv"))
  
  ## Initialise empty dataset
  diet_df <- data.frame(surveyyear = numeric(0), totalredmeat = numeric(0), aoac_fibre = numeric(0), sex = numeric(0), age = numeric(0), weight = numeric(0),
                        country = character(0), processed.poultry = numeric(0), processed.redmeat = numeric(0),
                        burgers = numeric(0), sausages = numeric(0), beef = numeric(0), lamb = numeric(0), entrails = numeric(0), other = numeric(0), pork = numeric(0))
  
  ## Loop through datasets and grab all necessary data
  for(i in which(ukds_dict$Survey_Name == "NDNS")){
    
    # Read in data
    diet_df_temp <- read.delim(file = paste0(filepath, "/UKDA-", ukds_dict$UKDS_Number[i], "-tab/tab/", ukds_dict$Indiv_Dataset_Name[i]), sep = "\t")
    weights_df_temp <- read.delim(file = paste0(filepath, "/UKDA-", ukds_dict$UKDS_Number[i], "-tab/tab/", ukds_dict$Weights_Dataset_Name[i]), sep = "\t")
    
    # Extract years of NDNS from years listed in dictionary - this is needed for pulling the weight variable
    start_year <- gsub("\\-.*", "", ukds_dict$Year[i])
    end_year <- gsub(".*\\-", "", ukds_dict$Year[i])
    start_year_ndns <- as.numeric(start_year) - 2007
    end_year_ndns <- as.numeric(end_year) - 2007
    
    # Filter weights for just weights variable and identification number
    weights_df_temp <- weights_df_temp |>
      select(seriali, weight = !!sym(ukds_dict$Weights_Variable[i]))
    
    # Add weights to diet data and select variables of interest
    diet_df_temp <- merge(diet_df_temp, weights_df_temp, by = "seriali") |>
      select(surveyyear = !!sym(if_else(start_year_ndns == 5, "Surveyyear", "SurveyYear")), totalredmeat, aoac_fibre = AOACFibreg, sex = Sex, age = !!sym(if_else(start_year_ndns == 9, "AgeR", "Age")), weight, 
             country = Country, processed.poultry = ProcessedPoultryg, processed.redmeat = ProcessedRedMeatg, 
             burgers = Burgersg, sausages = Sausagesg, beef = Beefg, lamb = Lambg, entrails = Offalg, other = OtherRedMeatg, pork = Porkg)
    
    # Add to dataframe
    diet_df <- rbind(diet_df, diet_df_temp)
    
    
  }
  
  ## General data cleaning
  diet_df <- diet_df |>
    filter(country == "England") |>
    filter(age >= 20)
  
  # Apply age grouping based on user input
  if(user_options$age_groups_indicator){
    diet_df <- mutate(diet_df, age_group = if_else(age<50, "20-49", "50+"))
  } else {
    diet_df$age_group = "All"
  }
  
  diet_df <- diet_df |>
    mutate(
      
      year = surveyyear + 2007,
      
      weight = case_when(
        between(surveyyear, 1, 4) ~ weight * sum(weight) * (1/sum(weight*between(surveyyear, 1, 4))) * (4/11),
        between(surveyyear, 5, 6) ~ weight * sum(weight) * (1/sum(weight*between(surveyyear, 5, 6))) * (2/11),
        between(surveyyear, 7, 8) ~ weight * sum(weight) * (1/sum(weight*between(surveyyear, 7, 8))) * (2/11),
        between(surveyyear, 9, 11) ~ weight * sum(weight) * (1/sum(weight*between(surveyyear, 9, 11))) * (3/11)),
      
      weight = weight/mean(weight),
      
      sex = if_else(sex == 1, "Men", "Women")
      
    )
  
  
  ## Data cleaning by variable
  
  # Initialise empty df
  diet_df_total <- data.frame(sex = character(0), age_group = character(0), year = numeric(0), level = character(0), value = numeric(0), N = numeric(0))
  
  ## FIBRE
  diet_df_fibre <- diet_df |>
    mutate(
      
      fibre_cat = cut(aoac_fibre, breaks = c(seq(0,30), Inf), right = F)
      
    ) |>
    group_by(sex, age_group, year) |>
    mutate(total_weight = sum(weight)) |>
    ungroup() |>
    group_by(sex, age_group, year, fibre_cat) |>
    summarise(value = sum(aoac_fibre*weight)/sum(weight),
              N = total_weight[1]) |>
    mutate(variable = "fibre_consumption_cat_mean") |>
    rename(level = fibre_cat)
  
  diet_df_total <- rbind(diet_df_total, diet_df_fibre)
  
  ## RED MEAT
  diet_df_redmeat <- diet_df |>
    mutate(
      
      redmeat_total =  beef + lamb + pork + entrails + other,
      redmeat_cat = if_else(redmeat_total == 0, "0", cut(redmeat_total, breaks = c(seq(0,100, by = 10), Inf), right = T)),

    ) |>
    group_by(sex, age_group, year) |>
    mutate(total_weight = sum(weight)) |>
    ungroup() |>
    group_by(sex, age_group, year, redmeat_cat) |>
    summarise(value = sum(redmeat_total*weight)/sum(weight),
              N = total_weight[1]) |>
    mutate(variable = "redmeat_consumption_cat_mean") |>
    rename(level = redmeat_cat)
  
  
  diet_df_total <- rbind(diet_df_total, diet_df_redmeat)
  
  ## PROCESSED MEAT
  diet_df_processed <- diet_df |>
    mutate(
      
      processed_meat_total =  processed.redmeat + processed.poultry + burgers + sausages,
      processed_cat = if_else(processed_meat_total == 0, "0", cut(processed_meat_total, breaks = c(seq(0,100, by = 5), Inf), right = T)),

    ) |>
    group_by(sex, age_group, year) |>
    mutate(total_weight = sum(weight)) |>
    ungroup() |>
    group_by(sex, age_group, year, processed_cat) |>
    summarise(value = sum(processed_meat_total*weight)/sum(weight),
              N = total_weight[1]) |>
    mutate(variable = "processed_meat_consumption_cat_mean") |>
    rename(level = processed_cat)
  
  diet_df_total <- rbind(diet_df_total, diet_df_processed)
  
  # Adding IMD variable if needed
  if(user_options$imd_stratification){
    diet_df_total$imd <- "All"
  }
  
  ## Output df
  return(diet_df_total)
  
  
}
