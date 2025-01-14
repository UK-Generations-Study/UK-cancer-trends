# This is the PAF calculation function for calculating PAFs by risk factor

# Function: 

# Inputs:
#   dataframe: 
#   age_group_of_interest: 

# Outputs: 

paf_calculation <- function(dataframe, age_group_of_interest) {
  
  # Read in RR data 
  if(age_group_of_interest == "20-49") {
  rr <-read.csv("../Data/relativerisk_under50.csv")
  } else {
  rr <-read.csv("../Data/relativerisk_over50.csv")
  }
  
  # Fixing the alcohol gap 
  #Data Gap1: Alcohol data from 2005 ( General Household Survey) and 2011(Household Survey England)
  riskfactors_alcoholgap<- dataframe %>%
    filter(
      variable == "alcohol_amt"
    )
  
  # linear interpolation function
  alcgap_function <- function(data) {
    all_years <- data.frame(year = seq(min(data$year), max(data$year)))
    interpolated <- merge(all_years, data, by = "year", all.x = TRUE)
    interpolated$value <- approx(data$year, data$value, xout = all_years$year)$y
    return(interpolated)
  }
  # Group by age_group, sex, and level, then interpolate
  riskfactors_alcoholgap <- riskfactors_alcoholgap %>%
    group_by(age_group, sex, level) %>%
    group_modify(~ alcgap_function(.x)) %>%
    ungroup() %>%
    mutate(
      variable = "alcohol_amt"
    )
  
  
  riskfactors <- dataframe %>%
    filter(
      !(variable == "alcohol_amt")
    ) #taking out the old alcohol data 
  
  riskfactors <- rbind(riskfactors,riskfactors_alcoholgap) #adding in the new filled in data 
  
  # Cleaning the RR
  rr[-1] <- lapply(rr[-1], function(column) {
    as.numeric(sub("\\s*\\(.*\\)", "", column))
  })

  #storing the maximum values for red and processed meat 
  over100 <- dataframe %>%
    mutate(
      exp_low = as.numeric(str_extract(level, "\\d+")),
    ) %>% 
    filter(
      exp_low == 100, 
      variable == "redmeat_consumption_cat_mean" | variable == "processed_meat_consumption_cat_mean"
    ) %>%
    mutate(
      variable = ifelse( variable == "processed_meat_consumption_cat_mean", "Processed_Meat", variable),
      variable = ifelse( variable == "redmeat_consumption_cat_mean", "Red_Meat", variable), 
      level = str_trim(level), 
      variable = str_trim(variable)) %>%
    rename(
      exposure = variable, 
      midpoint2 = value
    ) 
  
  #high estimate rr 
  err <- rr %>%
    mutate(
      Fibre = log(1/Fibre)/10,
      Processed_Meat = (Processed_Meat-1) /100, 
      Red_Meat= (Red_Meat-1) /100, 
      Red_Processed._Meat = (Red_Processed._Meat-1) /100,
      Light_alcohol = (Light_alcohol-1), 
      Medium_alcohol = (Medium_alcohol-1),
      Heavy_alcohol = (Heavy_alcohol-1),
      Current_Smoking_men = (Current_Smoking_men-1),
      Former_Smoking_men = (Former_Smoking_men-1),
      Current_Smoking_women = (Current_Smoking_women-1),
      Former_Smoking_women = (Former_Smoking_women-1),
      BMI_overweight_men = (BMI_overweight_men-1),
      BMI_obese_men = (BMI_obese_men-1),
      BMI_overweight_women = (BMI_overweight_women-1),
      BMI_obese_women = (BMI_obese_women-1),
    ) #calculating the excess relative risk for each exposure level 
  #pivoting the table to restructure it correctly 
  err_2 <- t(err)
  err_2 <- as.data.frame(err_2)
  err_2$exposure <- rownames(err_2)
  colnames(err_2) <- err_2[1, ]
  err_2 <- err_2[-1, ]
  err <- err_2 %>%
    rename(
      exposure = Cancer_sites
    )
  
  #cleaning the risk factors and calculating the (ERR* risk factor prevalence) per risk factor 
  riskfactors <- riskfactors %>% 
    filter(
      age_group == age_group_of_interest
    ) %>% #filtering the data to the years of interest and to only early onset (EO) cases 
    mutate(
      level = str_trim(level), 
      variable = str_trim(variable),
      exposure = ifelse(level == "Light Drinker", "Light_alcohol", level), #renaming the risk factor data so that it matches the RR exposure levels 
      exposure = ifelse(level == "Moderate Drinker", "Medium_alcohol", exposure), 
      exposure = ifelse(level == "Heavy Drinker", "Heavy_alcohol", exposure), 
      exposure = ifelse(level == "Obese"& sex == "Men", "BMI_obese_men", exposure), 
      exposure = ifelse(level == "Obese"& sex == "Women", "BMI_obese_women", exposure), 
      exposure = ifelse(level == "Overweight"& sex == "Men", "BMI_overweight_men", exposure), 
      exposure = ifelse(level == "Overweight"& sex == "Women", "BMI_overweight_women", exposure), 
      exposure = ifelse(level == "Current"& sex == "Men", "Current_Smoking_men", exposure),
      exposure = ifelse(level == "Current"& sex == "Women", "Current_Smoking_women", exposure), 
      exposure = ifelse(level == "Previously"& sex == "Men", "Former_Smoking_men", exposure),
      exposure = ifelse(level == "Previously"& sex == "Women", "Former_Smoking_women", exposure), 
      exposure = ifelse(variable == "processed_meat_consumption", "Processed_Meat", exposure), 
      exposure = ifelse(variable == "redmeat_consumption", "Red_Meat", exposure), 
      exposure = ifelse(variable == "fibre_consumption", "Fibre", exposure), 
      exp_low = ifelse(variable == "fibre_consumption", as.numeric(str_extract(level, "\\d+")), NA), #need to determine the midpoint for the dose response exposure categories so that the correct RR can be calculated
      exp_low = ifelse(variable == "processed_meat_consumption", as.numeric(str_extract(level, "\\d+")), exp_low), 
      exp_low = ifelse(variable == "redmeat_consumption", as.numeric(str_extract(level, "\\d+")), exp_low),
      exp_high = ifelse(variable == "fibre_consumption", as.numeric(str_extract(level, "(?<=,)\\d+")), NA),
      exp_high = ifelse(variable == "processed_meat_consumption", as.numeric(str_extract(level, "(?<=,)\\d+")), exp_high),
      exp_high = ifelse(variable == "redmeat_consumption", as.numeric(str_extract(level, "(?<=,)\\d+")), exp_high), 
      exp_mdpt = exp_low + ((exp_high-exp_low)/2), 
      exp_mdpt = ifelse(is.na(exp_low), 1, exp_mdpt), 
      #exp_lvl = ifelse(exposure == "Fibre", 30 - exp_mdpt, exp_mdpt), #flipping fibre so that the exposure is deficient from the "risk free" 30p/day government guideline
    ) 
  upperlimits <- riskfactors %>%
    merge(
      over100, by = c("exposure", "sex", "age_group", "exp_low", "year", "level"), all.x = T
    ) %>%
    mutate(
      exp_mdpt = ifelse(is.na(exp_mdpt), midpoint2, exp_mdpt), 
      exp_lvl = ifelse(exposure == "Fibre", 30 - exp_mdpt, exp_mdpt), #flipping fibre so that the exposure is deficient from the "risk free" 30p/day government guideline
    )
  
  riskfactors <- upperlimits %>%
    filter(!grepl("^[\\(\\[0]", exposure)) %>% #remove most of the rows that arent being used to remove noise 
    left_join(err %>% 
                select(exposure, Oral, Endometrium, Pancreas, Gallbladder, Colorectum, Liver, Kidney, Thyroid, `Multiple Myeloma`, `Breast`), by = c("exposure" = "exposure")) %>% #add the ERR to the dataframe
    rename(OralERR = Oral, 
           EndometriumERR = Endometrium, 
           PancreasERR = Pancreas, 
           GallbladderERR = Gallbladder, 
           ColorectumERR = Colorectum, 
           LiverERR = Liver, 
           KidneyERR = Kidney, 
           ThyroidERR = Thyroid, 
           MultipleMyelomaERR = `Multiple Myeloma`, 
           BreastERR = `Breast`
    ) %>% #cleaning and renaming the variables 
    mutate(
      Oral = value*exp_lvl*as.numeric(OralERR), 
      Endometrium = value*exp_lvl*as.numeric(EndometriumERR), 
      Pancreas = value*exp_lvl*as.numeric(PancreasERR), 
      Gallbladder = value*exp_lvl*as.numeric(GallbladderERR), 
      Colorectum = value*exp_lvl*as.numeric(ColorectumERR), 
      Liver = value*exp_lvl*as.numeric(LiverERR), 
      Kidney = value*exp_lvl*as.numeric(KidneyERR), 
      Thyroid = value*exp_lvl*as.numeric(ThyroidERR), 
      MultipleMyeloma = value*exp_lvl*as.numeric(MultipleMyelomaERR), 
      Breast = value*exp_lvl*as.numeric(BreastERR)
    ) #this is calculating the main unit of the PAF equation: (ERR*exposure prevalence) per each category
  
  columns_to_check <- c("Oral", "Endometrium", "Pancreas", "Gallbladder", 
                        "Colorectum", "Liver", "Kidney", "Thyroid", 
                        "MultipleMyeloma", "Breast") #list of columns for later functions
  subpaf <- riskfactors %>%
    select (year, age_group, variable, exposure, sex, exp_lvl, Oral, Endometrium, Pancreas, Gallbladder, Colorectum, Liver, Kidney, Thyroid, MultipleMyeloma, Breast) %>%
    mutate(across(all_of(columns_to_check), ~ ifelse(. < 0, 0.00, .))) %>% #recode all of the negative numbers to 0 
    mutate(variable = if_else(variable == "alcohol_amt", "Alcohol", variable), 
           variable = if_else(variable == "bmi", "BMI", variable),
           variable = if_else(variable == "smoking_status", "Smoking", variable),
           variable = if_else(variable == "fibre_consumption", "Fibre", variable),
           variable = if_else(variable == "redmeat_consumption", "RedMeat", variable),
           variable = if_else(variable == "processed_meat_consumption", "ProcessedMeat", variable),
    ) #Renaming and cleaning so that the categories are easier to understand
  
  #calculating risk factor PAF by cancer site 
  paf_cancersite <- subpaf %>%
    group_by(year, variable, sex) %>% #grouping the dataframe by year, variable, and sex
    summarise(
      Oral_num = sum(Oral, na.rm = TRUE),
      Endometrium_num = sum(Endometrium, na.rm = TRUE),
      Pancreas_num = sum(Pancreas, na.rm = TRUE),
      Gallbladder_num = sum(Gallbladder, na.rm = TRUE),
      Colorectum_num = sum(Colorectum, na.rm = TRUE),
      Liver_num = sum(Liver, na.rm = TRUE),
      Kidney_num = sum(Kidney, na.rm = TRUE),
      Thyroid_num = sum(Thyroid, na.rm = TRUE),
      MultipleMyeloma_num = sum(MultipleMyeloma, na.rm = TRUE),
      Breast_num = sum(Breast, na.rm = TRUE)
    ) %>% #summing the PAF sub calculations by variable, year, and sex to create the numerator o
    mutate(
      Oral_denom = Oral_num + 1,
      Endometrium_denom = Endometrium_num + 1,
      Pancreas_denom = Pancreas_num + 1,
      Gallbladder_denom = Gallbladder_num + 1,
      Colorectum_denom = Colorectum_num + 1,
      Liver_denom = Liver_num + 1,
      Kidney_denom = Kidney_num + 1,
      Thyroid_denom = Thyroid_num + 1,
      MultipleMyeloma_denom = MultipleMyeloma_num + 1,
      Breast_denom = Breast_num + 1, #calculating the PAF calculation denominator
      
      OralPAF = Oral_num / Oral_denom,
      EndometriumPAF = Endometrium_num / Endometrium_denom,
      PancreasPAF = Pancreas_num / Pancreas_denom,
      GallbladderPAF = Gallbladder_num / Gallbladder_denom,
      ColorectumPAF = Colorectum_num / Colorectum_denom,
      LiverPAF = Liver_num / Liver_denom,
      KidneyPAF = Kidney_num / Kidney_denom,
      ThyroidPAF = Thyroid_num / Thyroid_denom,
      MultipleMyelomaPAF = MultipleMyeloma_num / MultipleMyeloma_denom,
      BreastPAF = Breast_num / Breast_denom, #calculating the PAFs by cancer site and risk factor
      age_group = age_group_of_interest
    ) %>% 
    select(year, age_group ,variable, sex, OralPAF, EndometriumPAF, PancreasPAF, GallbladderPAF, ColorectumPAF, LiverPAF, KidneyPAF, ThyroidPAF, MultipleMyelomaPAF, BreastPAF) #cleaning the dataset
  
  return(paf_cancersite)
}  

  

  
