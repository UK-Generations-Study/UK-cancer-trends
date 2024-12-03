###This is the function to move create PIF scenarios 

#the scenarios will be calculating the RR given the population prevalence if the PIF is: 
  # A) 1%
  # B) 3%
  # C) 5%
  # D) 10%


pif_scenario <- function(dataframe) {
  
  cleaned <- dataframe %>%
    group_by(level, sex, age_group) %>%
    mutate(
      year = as.numeric(year)
    ) %>%
    summarize(
      p0 = value[which.min(year)], 
      p1 = value[which.max(year)],  
      .groups = "drop" 
    ) %>% 
    mutate(
      sA = 1 + ((1)/(p1 - p0 - (1*p1))),
      sB = 1 + ((3)/(p1 - p0 - (3*p1))),
      sC = 1 + ((5)/(p1 - p0 - (5*p1))),
      sD = 1 + ((10)/(p1 - p0 - (10*p1))),
    ) 
  
    return(cleaned)
}

##testing 
nitrogendioxide <- read.csv("C:/Users/zrichards.ICR/Downloads/fig01_nitrogen_dioxide_annual.csv")
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
  
nitrogendioxide_pif <- pif_scenario(nitrogendioxide)

nitrogendioxide_pif <- nitrogendioxide %>% 
  group_by(level, sex, age_group) %>%
  mutate(
    year = as.numeric(year)
  ) %>%
  summarize(
    p0 = value[which.min(year)], 
    p1 = value[which.max(year)],  
    .groups = "drop" 
    ) %>% 
  mutate(
    sA = 1 + ((1)/(p1 - p0 - (1*p1))),
    sB = 1 + ((3)/(p1 - p0 - (3*p1))),
    sC = 1 + ((5)/(p1 - p0 - (5*p1))),
    sD = 1 + ((10)/(p1 - p0 - (10*p1))),
  ) 
  
  
  
  