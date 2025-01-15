# This is to organize the risk factor data set for the join point analysis 

# Function : This is to organize the data set for join point 

# Input : 
    # dataframe: the cleaned risk factor data set
    # name: the name of the data set (for saving the .csv file at the end)

# Output :
    # this just arranges the dataset and saves it in the joinpoint folder as a .csv so that the software will run 

joinpoint_clean <- function(dataframe, name) {
  joinpoint <- dataframe %>% 
    arrange(
      age_group, sex, level, year
      ) 
  
write.csv(joinpoint, paste0("../../../Data/Joinpoint_Cleaned_Data/joinpoint_",name, ".csv"), row.names = F )
    
    
}