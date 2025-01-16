#This code will clean all of the data for joinpoint and then pull the necessary 
#joinpoint output and save to the Git server 

#This is necessary due to data security restrictions of the data sources 

# 1 : Saving the cleaned data for join point input 
# Risk Factor data by age 
joinpoint <- data_rf %>% 
  filter(
    variable %in% c("alcohol_amt", "physical_activity", "bmi", "smoking_status")
  )%>%
  arrange(
    level, sex, age_group, year
  ) 

# Diet data 
joinpoint_diet <- data_rf %>%
  filter(
    variable %in% c("redmeat_consumption_median", "processedmeat_consumption_median", "fibre_consumption_mean")
  ) %>%
  arrange(
    variable, level, sex, age_group, year
  )

# risk factor IMD data 
joinpoint_imd <- data_rf_imd %>% 
  rename(
    exposure = level 
  ) %>% 
  filter(
    variable %in% c("alcohol_amt", "physical_activity", "bmi", "smoking_status"), 
    imd != "All"
  )%>%
  mutate(
    exposure = ifelse(exposure == "Below Recommendations", "PhysicalActivity", exposure),
  )%>%
  arrange(
    exposure, sex, age_group, imd, year
  ) 

#childhood BMI

#Cancerrates 
joinpoint_cancer <- data_cancer %>%
  arrange(
    sex, age_group, cancer_site, year
  )

# Saving the cleaned datasets 
write.csv(joinpoint_diet, "../../../Data/Joinpoint_Cleaned_Data/joinpoint_rfdiet.csv", row.names = F ) 
write.csv(joinpoint, ("../../../Data/Joinpoint_Cleaned_Data/joinpoint_rf.csv"), row.names = F )
write.csv(joinpoint_imd, ("../../../Data/Joinpoint_Cleaned_Data/joinpoint_rf_imd.csv"), row.names = F )
write.csv(joinpoint_cancer, ("../../../Data/Joinpoint_Cleaned_Data/joinpoint_cancerrates.csv"), row.names = F )

#NOW RUN THE JOINPOINT 
  #Parameters: 
    # Constant Homstadicity 
    # Methods and Parameters -> Parametric Method 
    # If given the option - allow to make recommended number of APCs 

