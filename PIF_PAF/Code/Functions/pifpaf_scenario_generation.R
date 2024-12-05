#This code is to clean and run PIF/PAF RR prediciton scenarios for suspected cancer risk factors

#load the functions: 
source("pifpaf_scenario_functions.R")

###AIR POLLUTION

##Nitrogen Dioxide
#load in the dataset 
nitrogendioxide <- read.csv("C:/Users/zrichards.ICR/Downloads/fig01_nitrogen_dioxide_annual.csv") 

#formatting the data correctly 
nitrogendioxide <- nitrogendioxide %>% 
  rename(
    level = Site.type, 
    value = Mean.NO2.concentration, 
    year = Year
  ) %>%
  select(
    level, value, year
  ) %>%
  mutate(
    sex = "all", 
    age_group = "all", 
    variable = "nitrogendioxide", 
    value = value/100
  ) %>%
  filter(
    year >=2009 & year <=2019
  )
#running the functions 
pif_nitrogendioxide <- pif_scenario(nitrogendioxide)
paf_nitrogendioxide <- paf_scenario(nitrogendioxide)
