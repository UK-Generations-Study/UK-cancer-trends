#Calculating overall PIF calculations 

#First need to calculate overall PAFS regardless of subgroups 

#need to clean and restructure data to ignore subgroups 
riskfactors<- read.csv("../../../../Data/Cleaned_Data/clean_rf_data.csv")
## all rr are the same across age groups except bmi and breast cancer 
rr<-read.csv("../../Data/relativerisk_over50.csv")

#calculating aggregate prevalences 
all_rf <- riskfactors %>%
  mutate(
    subexp_count = value * N
  ) %>%
  filter (
    level %in% c("Heavy Drinker", "Moderate Drinker", "Light Drinker", "Non-Drinker", "Healthy Weight", "Underweight", "Obese", "Overweight", "Current", "Previously", "Never")
  ) %>%
  group_by(
    year, level
  ) %>% 
  summarise(
    totN = sum(N), 
    totexp_count = sum(subexp_count)
  ) %>%
  mutate(
    tot_prev = totexp_count/totN
  ) %>% 
  select(
    year, level, tot_prev
  )


#cleaning the RR
#best estimate rr
rr[-1] <- lapply(rr[-1], function(column) {
  as.numeric(sub("\\s*\\(.*\\)", "", column))
})
#pulling the ERR
err <- rr%>%
  mutate(
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
err2 <- t(err)
err2 <- as.data.frame(err2)
err2$exposure <- rownames(err2)
colnames(err2) <- err2[1, ]
err2 <- err2[-1, ]
err <- err2 %>%
  rename(exposure = Cancer_sites)

#cleaning the risk factor data
rf <- all_rf %>% 
  mutate(
    level = str_trim(level), 
    exposure = ifelse(level == "Light Drinker", "Light_alcohol", level), #renaming the risk factor data so that it matches the RR exposure levels 
    exposure = ifelse(level == "Moderate Drinker", "Medium_alcohol", exposure), 
    exposure = ifelse(level == "Heavy Drinker", "Heavy_alcohol", exposure), 
    exposure = ifelse(level == "Obese", "BMI_obese", exposure), 
    exposure = ifelse(level == "Overweight", "BMI_overweight", exposure), 
    exposure = ifelse(level == "Current", "Current_Smoking", exposure),
    exposure = ifelse(level == "Previously", "Former_Smoking", exposure),
    exp_mdpt = 1
  ) 

#calculating the smallest math unit
rf_sub <- rf %>%
  #filter(!grepl("^[\\(\\[0]", exposure)) %>% #remove most of the rows that arent being used to remove noise 
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
) #%>% #cleaning and renaming the variables
# mutate(
#   Oral = tot_prev*exp_mdpt*as.numeric(OralERR),
#   Endometrium = tot_prev*exp_mdpt*as.numeric(EndometriumERR),
#   Pancreas = tot_prev*exp_mdpt*as.numeric(PancreasERR),
#   Gallbladder = tot_prev*exp_mdpt*as.numeric(GallbladderERR),
#   Colorectum = tot_prev*exp_mdpt*as.numeric(ColorectumERR),
#   Liver = tot_prev*exp_mdpt*as.numeric(LiverERR),
#   Kidney = tot_prev*exp_mdpt*as.numeric(KidneyERR),
#   Thyroid = tot_prev*exp_mdpt*as.numeric(ThyroidERR),
#   MultipleMyeloma = tot_prev*exp_mdpt*as.numeric(MultipleMyelomaERR),
#   Breast = tot_prev*exp_mdpt*as.numeric(BreastERR)
# ) #this is calculating the main unit of the PAF equation: (ERR*exposure prevalence) per each category

#cleaning the data 
columns_to_check <- c("Oral", "Endometrium", "Pancreas", "Gallbladder", 
                      "Colorectum", "Liver", "Kidney", "Thyroid", 
                      "MultipleMyeloma", "Breast") #list of columns for later functions
rf_sub <- rf_sub %>%
  select (year, exposure, Oral, Endometrium, Pancreas, Gallbladder, Colorectum, Liver, Kidney, Thyroid, MultipleMyeloma, Breast) %>%
  mutate(across(all_of(columns_to_check), ~ ifelse(. < 0, 0.00, .))) %>% #recode all of the negative numbers to 0 
  mutate(variable = if_else(grepl("alcohol", exposure, ignore.case = TRUE), "alcohol", exposure), 
         variable = if_else(grepl("BMI", exposure, ignore.case = TRUE), "BMI", variable),
         variable = if_else(grepl("Smoking", exposure, ignore.case = TRUE), "Smoking", variable),
  ) #Renaming and cleaning so that the categories are easier to understand
