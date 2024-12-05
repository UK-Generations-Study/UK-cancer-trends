#This code is to clean and run PIF/PAF RR prediciton scenarios for suspected cancer risk factors

#load the functions: 
source("Functions/pifpaf_scenario_functions.R")

##ALL DATA IS FROM THE UK, not England specifically##

###AIR POLLUTION

##Nitrogen Dioxide
#source: Department for Environment, Food, and Rural Affairs 
#https://www.gov.uk/government/statistics/air-quality-statistics/ntrogen-dioxide#:~:text=Urban%20background%20NO2%20pollution%20has,9%20per%20cent%20since%202022.

#load in the dataset 
#csv needs to be edited from download to csv to remove (units) as the characters are not recognized in R
nitrogendioxide <- read.csv("../Data/PotentialExposures/fig01_nitrogen_dioxide_annual.csv") 

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

##Ozone 
#source: Department for Environment, Food, and Rural Affairs 
#https://www.gov.uk/government/statistics/air-quality-statistics/concentrations-of-ozone

#load in the dataset
#csv needs to be edited from download to csv to remove (units) as the characters are not recognized in R
ozone <- read.csv("../Data/PotentialExposures/fig13_ozone_annual.csv")
#cleaning the data 
ozone <- ozone %>% 
  rename(
    level = Site.type, 
    value = Mean.daily.maximum.8.hour.mean.ozone.concentration, 
    year = Year
  ) %>%
  select(
    level, value, year
  ) %>%
  mutate(
    sex = "all", 
    age_group = "all", 
    variable = "ozone", 
    value = value/100
  ) %>%
  filter(
    year >=2009 & year <=2019
  )
#running the functions
pif_ozone <- pif_scenario(ozone)
paf_ozone <- paf_scenario(ozone)

##PM2.5 
#source: Department for Environment, Food, and Rural Affairs 
#https://www.gov.uk/government/statistics/air-quality-statistics/concentrations-of-particulate-matter-pm10-and-pm25

#load in the dataset
#csv needs to be edited from download to csv to remove (units) as the characters are not recognized in R
pm25 <- read.csv("../Data/PotentialExposures/fig06_pm25_annual.csv")
#cleaning the data 
pm25 <- pm25 %>% 
  rename(
    level = Site.type, 
    value = Mean.PM2.5.concentration, 
    year = Year
  ) %>%
  select(
    level, value, year
  ) %>%
  mutate(
    sex = "all", 
    age_group = "all", 
    variable = "PM2.5", 
    value = value/100
  ) %>%
  filter(
    year >=2009 & year <=2019
  )

#running the functions
pif_pm25 <- pif_scenario(pm25)
paf_pm25 <- paf_scenario(pm25)

##PM10 
#source: Department for Environment, Food, and Rural Affairs 
#https://www.gov.uk/government/statistics/air-quality-statistics/concentrations-of-particulate-matter-pm10-and-pm25

#load in the dataset
#csv needs to be edited from download to csv to remove (units) as the characters are not recognized in R
pm10 <- read.csv("../Data/PotentialExposures/fig05_pm10_annual.csv")
#cleaning the data 
pm10 <- pm10 %>% 
  rename(
    level = Site.type, 
    value = Mean.PM10.concentration, 
    year = Year
  ) %>%
  select(
    level, value, year
  ) %>%
  mutate(
    sex = "all", 
    age_group = "all", 
    variable = "PM10", 
    value = value/100
  ) %>%
  filter(
    year >=2009 & year <=2019
  )

#running the functions
pif_pm10 <- pif_scenario(pm10)
paf_pm10 <- paf_scenario(pm10)

#full table 
pifscenarios_allrf <- rbind(pif_ozone, pif_nitrogendioxide, pif_pm25, pif_pm10)
pafscenarios_allrf <- rbind(paf_ozone, paf_nitrogendioxide, paf_pm25, paf_pm10)
