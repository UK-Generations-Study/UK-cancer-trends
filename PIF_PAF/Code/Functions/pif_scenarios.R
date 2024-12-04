###This is the function to move create PIF and PAF scenarios 

#the scenarios will be calculating the RR given the population prevalence if the PIF is: 
  # A) 1%
  # B) 3%
  # C) 5%
  # D) 10%

#PIF scenario function: 
pif_scenario <- function(dataframe) {
  
  cleaned <- dataframe %>% #redefining the input 
    group_by(level, sex, age_group) %>% #grouping the data by the stratifying variables 
    mutate(
      year = as.numeric(year) #ensuring the year variable is numeric  
    ) %>%
    summarize(
      p0 = value[which.min(year)], #the minimum year of the data set 
      p1 = value[which.max(year)],  #the maximum year of the data set 
      .groups = "drop" 
    ) %>% 
    mutate(
      sA = 1 + ((1)/(p1 - p0 - (1*p1))), #scenario (A): PIF is 1%
      sB = 1 + ((3)/(p1 - p0 - (3*p1))), #scenario (B): PIF is 3%
      sC = 1 + ((5)/(p1 - p0 - (5*p1))), #scenario (C): PIF is 5%
      sD = 1 + ((10)/(p1 - p0 - (10*p1))), #scenario (D): PIF is 10%
    ) 
  
    return(cleaned) #return the new scenario dataset 
}



##testing 
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
  )
#official test
nitrogendioxide_pif <- pif_scenario(nitrogendioxide)


  
  
  