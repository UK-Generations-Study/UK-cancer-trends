#This code will calculate the PAFs by cancer site per risk factor and as an aggregate number for the years: 2015 and 2019
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
#Before running any of this, please run "UK-cancer-trends\PIF_PAF\Code\RF_Data_Generation.qmd"

#read in the data
riskfactors<- read.csv("../../../Data/Cleaned_Data/clean_rf_data.csv")
rr_under50<-read.csv("../Data/relativerisk_under50.csv")
rr_over50<-read.csv("../Data/relativerisk_over50.csv")

#cleaning the relative risk data frames so that they are all formatted correctly and representing the correct dose responses 
#under 50 RR
rr_under50[-1] <- lapply(rr_under50[-1], function(column) {
  as.numeric(sub("\\s*\\(.*\\)", "", column))
})
err_under50 <- rr_under50 %>%
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
  )
#over 50 RR
rr_over50[-1] <- lapply(rr_over50[-1], function(column) {
  as.numeric(sub("\\s*\\(.*\\)", "", column))
})
err_over50 <- rr_over50 %>%
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
  )

#understanding the distribution of the dietary factors 
redmeat_under50 <- rf_under50 %>%
  filter(variable == "redmeat_consumption") %>%
  ggplot(aes(x = exp_low, y = value)) +  
  geom_col(fill = "red", color = "black")  +
  labs(
    title = "Red Meat",
    x = "Consumption Category",
    y = "Population Proportion"
  ) +
  theme_minimal()
processedmeat_under50 <- rf_under50 %>%
  filter(variable == "processed_meat_consumption") %>%
  ggplot(aes(x = exp_low, y = value)) +  
  geom_col(fill = "orange", color = "black")  +
  labs(
    title = "Processed Meat",
    x = "Consumption Category",
    y = "Population Proportion"
  ) +
  theme_minimal()
fibre_under50 <- rf_under50 %>%
  filter(variable == "fibre_consumption") %>%
  ggplot(aes(x = exp_low, y = value)) +  
  geom_col(fill = "pink", color = "black")  +
  labs(
    title = "Fibre",
    x = "Consumption Category",
    y = "Population Proportion"
  ) +
  theme_minimal()
print(redmeat_under50)
print(processedmeat_under50)
print(fibre_under50)

#risk factor data cleaning
rf_under50 <- riskfactors %>% 
  filter(
    year >= 2005 & year <= 2011, 
    age_group == "20-49"
    ) %>%
  mutate(
    level = str_trim(level), 
    variable = str_trim(variable),
    exposure = ifelse(level == "Light Drinker", "Light_alcohol", level), 
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
    exp_low = ifelse(variable == "fibre_consumption", as.numeric(str_extract(level, "\\d+")), NA), 
    exp_low = ifelse(variable == "processed_meat_consumption", as.numeric(str_extract(level, "\\d+")), exp_low), 
    exp_low = ifelse(variable == "redmeat_consumption", as.numeric(str_extract(level, "\\d+")), exp_low),
    exp_high = ifelse(variable == "fibre_consumption", as.numeric(str_extract(level, "(?<=,)\\d+")), NA),
    exp_high = ifelse(variable == "processed_meat_consumption", as.numeric(str_extract(level, "(?<=,)\\d+")), exp_high),
    exp_high = ifelse(variable == "redmeat_consumption", as.numeric(str_extract(level, "(?<=,)\\d+")), exp_high),
    exp_mdpt = exp_low + ((exp_high-exp_low)/2), 
    exp_mdpt = ifelse(is.na(exp_low), 1, exp_mdpt)
  )


rf_over50 <- riskfactors %>% 
  filter(
    year >= 2005 & year <= 2011, 
    age_group == "50+"
  )

#this is to calculate the PAFs for under 50 
paf_under50 <- rf_under50 %>%
  mutate(
    Oral = 
  )


