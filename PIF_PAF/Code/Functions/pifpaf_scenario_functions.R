###This is the function to move create PIF and PAF scenarios 

#the scenarios will be calculating the RR given the population prevalence if the PIF is: 
  # A) 1%
  # B) 3%
  # C) 5%
  # D) 10%

#PIF scenario function: 
pif_scenario <- function(dataframe) {
  
  variable_name <- deparse(substitute(dataframe)) #storing the variable
  
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
      pif1 = 1 + ((.01)/(p1 - p0 - (.01*p1))), #scenario (A): PIF is 1%
      pif3 = 1 + ((.03)/(p1 - p0 - (.03*p1))), #scenario (B): PIF is 3%
      pif5 = 1 + ((.05)/(p1 - p0 - (.05*p1))), #scenario (C): PIF is 5%
      pif10 = 1 + ((.10)/(p1 - p0 - (.10*p1))), #scenario (D): PIF is 10%
      variable = variable_name
    ) %>%
    select(
      variable, level, sex, age_group, p0, p1, pif1, pif3, pif5, pif10
    )
  
    return(cleaned) #return the new scenario dataset 
}

#PIF scenario function (SPECIFICALLY FOR BMI): 
##reported RR for colorectal and BMI: 
  #overweight = 1.22
  #obesity = 1.46

#this will calculate the scarios if the RR for OBESITY (overweight RR remains constant) is: 
    #A= 0.1 lower
    #B = 0.05 higher 
    #C = 0.10 higher 
    #D = 0.20 higher 

    
pif_scenarioBMI <- function(dataframe) {
  
  variable_name <- deparse(substitute(dataframe)) #storing the variable
  
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
      p0 = as.numeric(p0), 
      p1 = as.numeric(p1),
      pifActual = ((p1-p0)*(1.46-1))/(1 + p1*(1.46-1)), 
      pifA = ((p1-p0)*(1.36-1))/(1 + p1*(1.36-1)),
      pifB = ((p1-p0)*(1.51-1))/(1 + p1*(1.51-1)),
      pifC = ((p1-p0)*(1.56-1))/(1 + p1*(1.56-1)),
      pifD = ((p1-p0)*(1.66-1))/(1 + p1*(1.66-1)),
      variable = variable_name
    ) %>%
    select(
      variable, level, sex, age_group, p0, p1,  pifActual, pifA, pifB, pifC, pifD
    )
  
  return(cleaned) #return the new scenario dataset 
}


#PAF scenario function: 
paf_scenario <- function(dataframe) {
  
  variable_name <- deparse(substitute(dataframe)) #storing the variable
  
  
  cleaned <- dataframe %>% #redefining the input 
    group_by(level, sex, age_group) %>% #grouping the data by the stratifying variables 
    mutate(
      year = as.numeric(year), #ensuring the year variable is numeric  
      ) %>%
    filter(
      year == 2009
    )%>%
    mutate(
      paf5 = 1 + ((.05)/(value - (.05*value))), #scenario (A): PAF is 5%
      paf10 = 1 + ((.10)/(value - (.10*value))), #scenario (B): PAF is 10%
      paf15 = 1 + ((.15)/(value - (.15*value))), #scenario (C): PAF is 15%
      paf25 = 1 + ((.25)/(value - (.25*value))), #scenario (D): PAF is 25%
      paf35 = 1 + ((.35)/(value - (.35*value))), #scenario (E): PAF is 35%
      paf50 = 1 + ((.50)/(value - (.50*value))), #scenario (F): PAF is 50%
      variable = variable_name
    ) %>%
    select(
      variable, level, sex, age_group, year, value, paf5, paf10, paf15, paf15, paf25, paf35, paf50
    )
  
  return(cleaned) #return the new scenario data set 
}



  
  
  