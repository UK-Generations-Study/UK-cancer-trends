#Calculating overall PIF calculations 

#First need to calculate overall PAFS regardless of subgroups 

#need to clean and restructure data to ignore subgroups 
riskfactors<- read.csv("../../../Data/Cleaned_Data/clean_rf_data.csv")
## all rr are the same across age groups except bmi and breast cancer 
rr<-read.csv("../Data/relativerisk_over50.csv")

#calculating aggregate prevalences 
all_rf <- riskfactors %>%
  mutate(
    subexp_count = value * N
  ) %>%
  filter (
    level %in% c("Heavy Drinker", "Moderate Drinker", "Light Drinker", "Non-Drinker", "Healthy Weight", "Underweight", "Obese", "Overweight", "Current", "Previously", "Never")
  ) %>%
  group_by(
    year,sex,level
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

#####STEP 1
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
    exposure = ifelse(level == "Obese"& sex == "Men", "BMI_obese_men", exposure), 
    exposure = ifelse(level == "Obese"& sex == "Women", "BMI_obese_women", exposure), 
    exposure = ifelse(level == "Overweight"& sex == "Men", "BMI_overweight_men", exposure), 
    exposure = ifelse(level == "Overweight"& sex == "Women", "BMI_overweight_women", exposure), 
    exposure = ifelse(level == "Current"& sex == "Men", "Current_Smoking_men", exposure),
    exposure = ifelse(level == "Current"& sex == "Women", "Current_Smoking_women", exposure), 
    exposure = ifelse(level == "Previously"& sex == "Men", "Former_Smoking_men", exposure),
    exposure = ifelse(level == "Previously"& sex == "Women", "Former_Smoking_women", exposure), 
    exp_mdpt = 1
  ) 

#####STEP 2
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
) %>% #cleaning and renaming the variables
mutate(
  Oral = tot_prev*exp_mdpt*as.numeric(OralERR),
  Endometrium = tot_prev*exp_mdpt*as.numeric(EndometriumERR),
  Pancreas = tot_prev*exp_mdpt*as.numeric(PancreasERR),
  Gallbladder = tot_prev*exp_mdpt*as.numeric(GallbladderERR),
  Colorectum = tot_prev*exp_mdpt*as.numeric(ColorectumERR),
  Liver = tot_prev*exp_mdpt*as.numeric(LiverERR),
  Kidney = tot_prev*exp_mdpt*as.numeric(KidneyERR),
  Thyroid = tot_prev*exp_mdpt*as.numeric(ThyroidERR),
  MultipleMyeloma = tot_prev*exp_mdpt*as.numeric(MultipleMyelomaERR),
  Breast = tot_prev*exp_mdpt*as.numeric(BreastERR)
) #this is calculating the main unit of the PAF equation: (ERR*exposure prevalence) per each category

#cleaning the data 
columns_to_check <- c("Oral", "Endometrium", "Pancreas", "Gallbladder", 
                      "Colorectum", "Liver", "Kidney", "Thyroid", 
                      "MultipleMyeloma", "Breast") #list of columns for later functions
rf_sub <- rf_sub %>%
  select (year, sex, exposure, Oral, Endometrium, Pancreas, Gallbladder, Colorectum, Liver, Kidney, Thyroid, MultipleMyeloma, Breast) %>%
  mutate(across(all_of(columns_to_check), ~ ifelse(. < 0, 0.00, .))) %>% #recode all of the negative numbers to 0 
  mutate(variable = if_else(grepl("alcohol", exposure, ignore.case = TRUE), "Alcohol", exposure), 
         variable = if_else(grepl("BMI", exposure, ignore.case = TRUE), "BMI", variable),
         variable = if_else(grepl("Smoking", exposure, ignore.case = TRUE), "Smoking", variable),
  ) #Renaming and cleaning so that the categories are easier to understand


#calculating the pafs 
paf_cancersite <- rf_sub %>%
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
    age_group = "50+"
  ) %>% 
  select(year, variable, sex, OralPAF, EndometriumPAF, PancreasPAF, GallbladderPAF, ColorectumPAF, LiverPAF, KidneyPAF, ThyroidPAF, MultipleMyelomaPAF, BreastPAF) %>% #cleaning the dataset
  filter(
    variable %in% c("Alcohol", "BMI", "Smoking")
  )

#####STEP 3
#aggregate PAFs 
calculate_cumulative_paf <- function(paf_values) {
  #setting the base values    
  cumulative_paf <- 0
  remaining_whole <- 1
  
  for (paf in paf_values) {
    if (!is.na(paf)) {
      cumulative_paf <- cumulative_paf + paf * remaining_whole
      remaining_whole <- remaining_whole * (1 - paf)
    }
  }
  
  return(cumulative_paf)
}


aggregate_pafs <- paf_cancersite %>%
  group_by(year, sex) %>% #grouping by year/age/sex
  summarise(
    Oral = calculate_cumulative_paf(OralPAF),
    Endometrium = calculate_cumulative_paf(EndometriumPAF),
    Pancreas = calculate_cumulative_paf(PancreasPAF),
    Gallbladder = calculate_cumulative_paf(GallbladderPAF),
    Colorectum = calculate_cumulative_paf(ColorectumPAF),
    Liver = calculate_cumulative_paf(LiverPAF),
    Kidney = calculate_cumulative_paf(KidneyPAF),
    Thyroid = calculate_cumulative_paf(ThyroidPAF),
    MultipleMyeloma = calculate_cumulative_paf(MultipleMyelomaPAF),
    Breast = calculate_cumulative_paf(BreastPAF)
  ) %>% #applying the calculation above 
  ungroup() 

#cleaning the dataset and then saving it back to the git server
#men have values for Breast PAFS due to not have RR by sex for alcohol - these are incorrect and should be 0 
aggregate_pafs <- aggregate_pafs %>% 
  mutate( 
    Breast = ifelse(sex == "Men", 0, Breast)
  )
write.csv(aggregate_pafs, file = "../Data/PAF_allages.csv", row.names = F)

#####STEP 4
#total PIF generation
data_paf <- paf_cancersite |>
  filter(!(variable == "Alcohol" & between(year, 2006, 2010))) |>
  mutate(BreastPAF = if_else(sex == "Men", 0, BreastPAF))

## Filter to data we need
data_paf <- data_paf |>
  # mutate(year = year + 10) |>
  filter(!grepl("\\_mean$|\\_median$", variable)) |>
  group_by(variable, sex) |>
  mutate(
    
    dist_to_2009 = abs(year - 2009)
    
  ) |>
  mutate(
    
    youngest_indicator = year == max(year),
    oldest_indicator = year == min(year),
    close_to_2009 = dist_to_2009 == min(dist_to_2009)
    
  ) |>
  filter(youngest_indicator | close_to_2009 | oldest_indicator) |>
  ungroup() |>
  pivot_longer(cols = colnames(data_paf)[!colnames(data_paf) %in% c("sex", "year", "variable")], names_to = "cancer_site", values_to = "PAF") |>
  mutate(
    
    cancer_site = gsub("PAF$", "", cancer_site),
    
    PIF_component = 1 + PAF/(1-PAF)
    
  ) |>
  group_by(sex, variable, cancer_site) |>
  arrange(year) |>
  summarise(
    
    PIF_past = if_else(PIF_component[1] == 1 | PIF_component[2] == 1, NA, (PIF_component[2] - PIF_component[1])/PIF_component[2]),
    
    PIF_future = if_else(PIF_component[2] == 1 | PIF_component[3] == 1, NA, (PIF_component[3] - PIF_component[2])/PIF_component[3])
    
  )

write.csv(data_paf, file = "../Data/PIF_allages.csv", row.names = F)