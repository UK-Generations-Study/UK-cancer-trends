#load in necessary packages 
library(dplyr)
library(tidyr)
library(stringr)
library (ggplot2)
library(gridExtra)

#importing and cleaning the data 
#importing the data 
rr <- read.csv("C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\Data\\PAF_Data\\RR.csv")
prev <- read.csv("C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\Data\\PAF_Data\\Risk Factor Trends.csv")
cleanedprev <- prev
cancerrates <- read.csv("C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\Data\\PAF_Data\\Cancer Incidence Trends.csv")

#cleaning the data 
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Men" & variable == "% Obese", "obese_men", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Women" & variable == "% Obese", "obese_women", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Men" & variable == "% Overweight not obese", "overweight_men", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Women" & variable == "% Overweight not obese", "overweight_women", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(variable == "% HRT Usage", "hrt", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Men" & variable == "% Doctor Diagnosed Diabetes", "diabetes_men", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Women" & variable == "% Doctor Diagnosed Diabetes", "diabetes_women", variable))

cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Men" & variable == "% Alcohollight", "lightalcohol_men", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Women" & variable == "% Alcohollight", "lightalcohol_women", variable))

cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Men" & variable == "% Alcoholmedium", "medalcohol_men", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Women" & variable == "% Alcoholmedium", "medalcohol_women", variable))

cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Men" & variable == "% Alcoholheavy", "heavyalcohol_men", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Women" & variable == "% Alcoholheavy", "heavyalcohol_women", variable))

cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Men" & variable == "% Current Smoker", "smoker_men", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Women" & variable == "% Current Smoker", "smoker_women", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Men" & variable == "% Former Smoking", "formersmoker_men", variable))
cleanedprev <- cleanedprev %>%
  mutate(variable = if_else(sex == "Women" & variable == "% Former Smoking", "formersmoker_women", variable))


cleanedprev <- cleanedprev %>% 
  mutate(prev = value/100)
cleanedprev <- cleanedprev %>% 
  mutate(datayear = year)
cleanedprev <- cleanedprev %>% 
  dplyr::select(-year)

#clean the rr calculation
rr <- rr %>% 
  mutate(across(-1, as.numeric))

#risk factor visualization 
# prev_englandwomen <- ggplot(cleanedprev %>% 
#   filter (sex == "Women", country == "England"), aes(x = datayear, y = prev, color = variable)) + 
#   geom_point() + 
#   geom_line() +
#   labs(title = "Risk Factors for Women",
#        x = "Year",
#        y = "Risk Factor Prevalence") +
#   theme_minimal()
# 
# prev_englandmen <- ggplot(cleanedprev %>% 
#   filter (sex == "Men", country == "England"), aes(x = datayear, y = prev, color = variable)) + 
#   geom_point() + 
#   geom_line() +
#   labs(title = "Risk Factors for Men",
#        x = "Year",
#        y = "Risk Factor Prevalence") +
#   theme_minimal()
# 
# #print 
# riskfactors <- grid.arrange(prev_englandmen, prev_englandwomen)
# 
# #cancer incidence visualization 
# 
# #MEN
# cancerratesEM <- cancerrates %>% filter(Country == "England", Sex =="Men", Year <= 2019)
# 
# # Create the plot with a separate line and prediction line for each Cancer_Site
# cancercases_englandmen <- ggplot(cancerratesEM, aes(x = Year, y = Incidences, color = Cancer_Site)) + 
#   geom_point() + 
#   geom_line() +
#   labs(title = "Cancer Incidences in England by Cancer Site for Men",
#        x = "Year",
#        y = "Incidence Cases") +
#   theme_minimal()
# 
# 
# #WOMEN
# cancerratesEW <- cancerrates %>% filter(Country == "England", Sex =="Women", Year <= 2019)
# 
# # Create the plot with a separate line and prediction line for each Cancer_Site
# cancercases_englandwomen <- ggplot(cancerratesEW, aes(x = Year, y = Incidences, color = Cancer_Site)) + 
#   geom_point() + 
#   geom_line() +
#   labs(title = "Cancer Incidences in England by Cancer Site for Women",
#        x = "Year",
#        y = "Incidence Cases") +
#   theme_minimal()
# 
# #print 
# englandcancer <- grid.arrange(cancercases_englandmen, cancercases_englandwomen)

########################################################################################
#Calculating PAFS
cleanedprev1 <- cleanedprev
#cancercases1 <- rbind (cancerrates_predictionsM %>% 
   # mutate(Country = "England", Sex = "Men"), cancerrates_predictionsW %>% mutate(Country = "England", Sex = "Women"))
cancercases1 <- cancerrates

#trying on a loop 
years <- seq(2003,2019)

# Initialize lists to store results
paf_results_men <- list()
paf_results_women <- list()

for (year in years) {
  # Men calculations
  # Overweight
  prevalence_men <- cleanedprev1 %>%
    filter(country == "England", variable == paste0("overweight_men"), datayear == year) %>% pull(prev)
  if(length(prevalence_men) != 1) {stop(paste("Error: More than one value found for overweight_men in", year))}
  paf_men <- rr %>% rowwise() %>%
    mutate(across(-1, ~ if_else(riskfactor == "overweight", (prevalence_men * (. - 1)), .))) %>% ungroup()
  
  # Obese
  prevalence_men <- cleanedprev1 %>%
    filter(country == "England", variable == paste0("obese_men"), datayear == year) %>% pull(prev)
  if(length(prevalence_men) != 1) {stop(paste("Error: More than one value found for obese_men in", year))}
  paf_men <- paf_men %>% rowwise() %>% 
    mutate(across(-1, ~ if_else(riskfactor == "obese", (prevalence_men * (. - 1)), .))) %>% ungroup()
  
  obese <- paf_men %>% filter(riskfactor %in% c("obese"))
  obese <- obese %>%   dplyr::select(-riskfactor)
  overweight <- paf_men %>% filter(riskfactor %in% c("overweight"))
  overweight <- overweight %>% dplyr::select(-riskfactor)
  bmi <- ((overweight + obese)/ ((overweight + obese) +1))
  bmi_row <- data.frame(riskfactor = "bmi",bmi)
  paf_men <-bind_rows(paf_men, bmi_row)
  
  # Current Smoker
  prevalence_men <- cleanedprev1 %>%
    filter(country == "England", variable == paste0("smoker_men"), datayear == year) %>% pull(prev)
  if(length(prevalence_men) != 1) {stop(paste("Error: More than one value found for smoker_men in", year))}
  paf_men <- paf_men %>% rowwise() %>%
    mutate(across(-1, ~ if_else(riskfactor == "men_currentsmoker", (prevalence_men * (. - 1)), .))) %>% ungroup()
  
  # Former Smoker
  prevalence_men <- cleanedprev1 %>%
    filter(country == "England", variable == paste0("formersmoker_men"), datayear == year) %>% pull(prev)
  if(length(prevalence_men) != 1) {stop(paste("Error: More than one value found for formersmoker_men in", year))}
  paf_men <- paf_men %>% rowwise() %>%
    mutate(across(-1, ~ if_else(riskfactor == "men_formersmoker", (prevalence_men * (. - 1)), .))) %>% ungroup()
  
  cs <- paf_men %>% filter(riskfactor %in% c("men_currentsmoker"))
  cs <- cs %>%   dplyr::select(-riskfactor)
  fs <- paf_men %>% filter(riskfactor %in% c("men_formersmoker"))
  fs <- fs %>%   dplyr::select(-riskfactor)
  smoking <- ((cs + fs)/ ((cs + fs) +1))
  smoking_row <- data.frame(riskfactor = "smoking",smoking)
  paf_men <-bind_rows(paf_men, smoking_row)
  
  
  # Light Alcohol
  prevalence_men <- cleanedprev1 %>%
    filter(country == "England", variable == paste0("lightalcohol_men"), datayear == year) %>% pull(prev)
  if(length(prevalence_men) != 1) {
    prevalence_men<-0
  }
  if (length(prevalence_men) == 1) {
    paf_men <- paf_men %>% rowwise() %>%
      mutate(across(-1, ~ if_else(riskfactor == "lowalc", (prevalence_men * (. - 1)), .))) %>% ungroup()
  }
  
  # Medium Alcohol
  prevalence_men <- cleanedprev1 %>% 
    filter(country == "England", variable == paste0("medalcohol_men"), datayear == year) %>% pull(prev)
  if(length(prevalence_men) != 1) {
    prevalence_men<-0
  }
  if (length(prevalence_men) == 1) {
    paf_men <- paf_men %>% rowwise() %>% 
      mutate(across(-1, ~ if_else(riskfactor == "medalc", (prevalence_men * (. - 1)), .))) %>% ungroup()
  }
  
  
  # Heavy Alcohol
  prevalence_men <- cleanedprev1 %>%
    filter(country == "England", variable == paste0("heavyalcohol_men"), datayear == year) %>% pull(prev)
  if(length(prevalence_men) != 1) {
    prevalence_men<-0
  }
  if (length(prevalence_men) == 1) {
    paf_men <- paf_men %>% rowwise() %>%
      mutate(across(-1, ~ if_else(riskfactor == "highalc", (prevalence_men * (. - 1)), .))) %>% ungroup()
  }
  
  
  lowalc <- paf_men %>% filter(riskfactor %in% c("lowalc"))
  lowalc <- lowalc %>%   dplyr::select(-riskfactor)
  midalc <- paf_men %>% filter(riskfactor %in% c("medalc"))
  midalc <- midalc %>%   dplyr::select(-riskfactor)
  highalc <- paf_men %>% filter(riskfactor %in% c("highalc"))
  highalc <- highalc %>%   dplyr::select(-riskfactor)
  alcohol <- ((lowalc + midalc + highalc)/ ((lowalc + midalc + highalc) +1))
  alcohol_row <- data.frame(riskfactor = "alcohol",alcohol)
  paf_men <-bind_rows(paf_men, alcohol_row)
  
  
  # Filter out specific risk factors
  paf_men <- paf_men %>% 
    filter(!riskfactor %in% c("women_formersmoker","all_formersmoker", "women_currentsmoker", "all_currentsmoker", "parity", "hrt", "firstbirth"))
  
  paf_men <- paf_men %>% mutate(yr = year)
  
  # Store the result in the list
  paf_results_men[[year]] <- paf_men
  
  # Women calculations (similar steps as Men)
  
  # Overweight
  prevalence_women <- cleanedprev1 %>%
    filter(country == "England", variable == paste0("overweight_women"), datayear == year) %>% pull(prev)
  if(length(prevalence_women) != 1) {stop(paste("Error: More than one value found for overweight_women in", year))}
  paf_women <- rr %>% rowwise() %>%
    mutate(across(-1, ~ if_else(riskfactor == "overweight", (prevalence_women * (. - 1)), .))) %>% ungroup()
  
  # Obese
  prevalence_women <- cleanedprev1 %>%
    filter(country == "England", variable == paste0("obese_women"), datayear == year) %>% pull(prev)
  if(length(prevalence_women) != 1) {stop(paste("Error: More than one value found for obese_women in", year))}
  paf_women <- paf_women %>% rowwise() %>% 
    mutate(across(-1, ~ if_else(riskfactor == "obese", (prevalence_women * (. - 1)), .))) %>% ungroup()
  
  obese <- paf_women %>% filter(riskfactor %in% c("obese"))
  obese <- obese %>%   dplyr::select(-riskfactor)
  overweight <- paf_women %>% filter(riskfactor %in% c("overweight"))
  overweight <- overweight %>%   dplyr::select(-riskfactor)
  bmi <- ((overweight + obese)/ ((overweight + obese) +1))
  bmi_row <- data.frame(riskfactor = "bmi",bmi)
  paf_women <-bind_rows(paf_women, bmi_row)
  
  
  # Current Smoker
  prevalence_women <- cleanedprev1 %>% 
    filter(country == "England", variable == paste0("smoker_women"), datayear == year) %>% pull(prev)
  if(length(prevalence_women) != 1) {stop(paste("Error: More than one value found for smoker_women in", year))}
  paf_women <- paf_women %>% rowwise() %>%
    mutate(across(-1, ~ if_else(riskfactor == "women_currentsmoker", (prevalence_women * (. - 1)), .))) %>% ungroup()
  
  # Former Smoker
  prevalence_women <- cleanedprev1 %>% 
    filter(country == "England", variable == paste0("formersmoker_women"), datayear == year) %>% pull(prev)
  if(length(prevalence_women) != 1) {stop(paste("Error: More than one value found for formersmoker_women in", year))}
  paf_women <- paf_women %>% rowwise() %>% 
    mutate(across(-1, ~ if_else(riskfactor == "women_formersmoker", (prevalence_women * (. - 1)), .))) %>% ungroup()
  
  cs <- paf_women %>% filter(riskfactor %in% c("women_currentsmoker"))
  cs <- cs %>%   dplyr::select(-riskfactor)
  fs <- paf_women %>% filter(riskfactor %in% c("women_formersmoker"))
  fs <- fs %>%   dplyr::select(-riskfactor)
  smoking <- ((cs + fs)/ ((cs + fs) +1))
  smoking_row <- data.frame(riskfactor = "smoking",smoking)
  paf_women <-bind_rows(paf_women, smoking_row)
  
  
  # Light Alcohol
  prevalence_women <- cleanedprev1 %>%
    filter(country == "England", variable == paste0("lightalcohol_women"), datayear == year) %>% pull(prev)
  if(length(prevalence_women) != 1) {
    prevalence_women<-0
  }
  if (length(prevalence_women) == 1) {
    paf_women <- paf_women %>% rowwise() %>%
      mutate(across(-1, ~ if_else(riskfactor == "lowalc", (prevalence_women * (. - 1)), .))) %>% ungroup()
  }
  
  # Medium Alcohol
  prevalence_women <- cleanedprev1 %>% 
    filter(country == "England", variable == paste0("medalcohol_women"), datayear == year) %>% pull(prev)
  if(length(prevalence_women) != 1) {
    prevalence_women<-0
  }
  if (length(prevalence_women) == 1) {
    paf_women <- paf_women %>%
      rowwise() %>% mutate(across(-1, ~ if_else(riskfactor == "medalc", (prevalence_women * (. - 1)), .))) %>% ungroup()
  }
  
  # Heavy Alcohol
  prevalence_women <- cleanedprev1 %>%
    filter(country == "England", variable == paste0("heavyalcohol_women"), datayear == year) %>% pull(prev)
  if(length(prevalence_women) != 1) {
    prevalence_women<-0
  }
  if (length(prevalence_women) == 1) {
    paf_women <- paf_women %>% rowwise() %>%
      mutate(across(-1, ~ if_else(riskfactor == "highalc", (prevalence_women * (. - 1)), .))) %>% ungroup()
  }
  
  
  lowalc <- paf_women %>% filter(riskfactor %in% c("lowalc"))
  lowalc <- lowalc %>%   dplyr::select(-riskfactor)
  midalc <- paf_women %>% filter(riskfactor %in% c("medalc"))
  midalc <- midalc %>%   dplyr::select(-riskfactor)
  highalc <- paf_women %>% filter(riskfactor %in% c("highalc"))
  highalc <- highalc %>%  dplyr::select(-riskfactor)
  alcohol <- ((lowalc + midalc + highalc)/ ((lowalc + midalc + highalc) +1))
  alcohol_row <- data.frame(riskfactor = "alcohol",alcohol)
  paf_women <-bind_rows(paf_women, alcohol_row)
  

  paf_women <- paf_women %>% 
    filter(!riskfactor %in% c("men_formersmoker", "all_formersmoker", "men_currentsmoker", "all_currentsmoker", "parity", "firstbirth"))
  
  paf_women <- paf_women %>% mutate(yr = year)
  
  # Store the result in the list
  paf_results_women[[year]] <- paf_women
}

# Combine results if needed
paf_results_men_combined <- bind_rows(paf_results_men, .id = "year")
paf_results_women_combined <- bind_rows(paf_results_women, .id = "year")

##Full big three data 
complete_paf_men <- paf_results_men_combined %>% filter(riskfactor %in% c("bmi", "smoking", "alcohol"))
complete_paf_women <- paf_results_women_combined %>% filter(riskfactor %in% c("bmi", "smoking", "alcohol"))

complete_paf_men <- complete_paf_men %>% mutate(combined = paste0(riskfactor, "", yr))
complete_paf_women <- complete_paf_women %>% mutate(combined = paste0(riskfactor, "", yr))

complete_paf_men <- complete_paf_men %>%   dplyr::select(combined, yr, everything())
complete_paf_women <- complete_paf_women %>%   dplyr::select(combined, yr, everything())

complete_paf_men <- complete_paf_men %>% mutate(across(-c(1:4), ~ if_else(. == 0, NA_real_, .)))
complete_paf_women <- complete_paf_women %>% mutate(across(-c(1:4), ~ if_else(. == 0, NA_real_, .)))


############################################################################################################################################
#trimming the column names to make it cleaner
colnames(complete_paf_men) <- trimws(colnames(complete_paf_men))
colnames(complete_paf_women) <- trimws(colnames(complete_paf_women))
complete_paf_men <- complete_paf_men %>%
  rename(Lung = lungRR, Lung_low = lungRR_low, Lung_high = lungRR_high, Colorectal = colorectalRR, Colorectal_low = colorectalRR_low, Colorectal_high = colorectalRR_high, Breast = breastRR, Breast_low = breastRR_low, Breast_high = breastRR_high, Pancreas = pancreasRR, Pancreas_low = pancreasRR_low, Pancreas_high = pancreasRR_high, SCC = SCCRR, SCC_low =SCCRR_low, SCC_high = SCCRR_high, AC = ACRR, AC_low = ACRR_low, AC_high = ACRR_high, Prostate = prostateRR, Prostate_low = prostateRR_low, Prostate_high = prostateRR_high)
complete_paf_women <- complete_paf_women %>%
  rename(Lung = lungRR, Lung_low = lungRR_low, Lung_high = lungRR_high, Colorectal = colorectalRR, Colorectal_low = colorectalRR_low, Colorectal_high = colorectalRR_high, Breast = breastRR, Breast_low = breastRR_low, Breast_high = breastRR_high, Pancreas = pancreasRR, Pancreas_low = pancreasRR_low, Pancreas_high = pancreasRR_high, SCC = SCCRR, SCC_low =SCCRR_low, SCC_high = SCCRR_high, AC = ACRR, AC_low = ACRR_low, AC_high = ACRR_high, Prostate = prostateRR, Prostate_low = prostateRR_low, Prostate_high = prostateRR_high)

############################################################################################################################################
#FIBRE
#adding the data from fibre to the paf cases 
#run Fibre Code
source("C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\Code\\PAF\\FibreCode.R")
#run Red Meat 
source("C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\Code\\PAF\\RedMeatCode.R")
#run Processed Meat 
source("C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\Code\\PAF\\ProcessedMeatCode.R")

#create a clean table 
#cleaning the bmi/smoking/alcohol pafs by removing all the negative numbers 
cleanPAF_men <- complete_paf_men %>%
  mutate(sex ="Men") %>%
  dplyr::select(combined, yr, riskfactor, sex, Lung, Colorectal, Breast, Pancreas, SCC, AC, Prostate)
cleanPAF_men[] <- lapply(cleanPAF_men, function(x) ifelse(is.numeric(x) & x < 0, NA, x))

cleanPAF_women <- complete_paf_women %>%
  mutate(sex ="Women") %>%
  dplyr::select(combined, yr, riskfactor, sex, Lung, Colorectal, Breast, Pancreas, SCC, AC, Prostate)
cleanPAF_women[] <- lapply(cleanPAF_women, function(x) ifelse(is.numeric(x) & x < 0, NA, x))

#combing all of the PAF into one dataset
PAF <- rbind (cleanPAF_women, cleanPAF_men)

#complete dataset and saving 
PAF_all <-rbind(PAF, cleanfibre, all_redmeat, all_processedmeat)

#write.csv(PAF_all, "Findings/PAF_all.csv", row.names= FALSE)

############################################################################################################################################
############################################################################################################################################
# 
# #creating the indivudal graphs 
# ############# MEN ###################
# smoking <- PAF_all %>% filter(riskfactor == "smoking", sex == "Men")
# bmi <- PAF_all %>% filter(riskfactor == "bmi", sex == "Men")
# alcohol <- PAF_all %>% filter(riskfactor == "alcohol", sex == "Men")
# fibre <- PAF_all %>% filter(riskfactor == "fibre", sex == "Men")
# redmeat <- PAF_all %>% filter(riskfactor =="redmeat", sex == "Men")
# processedmeat <- PAF_all %>% filter(riskfactor =="processedmeat", sex == "Men")
# #####################################################
# #run graphs 
# #LUNG CANCER
# smoking_lungmen <- ggplot(smoking, aes(x= yr, y = Lung)) + geom_line() + geom_point() + 
#   labs(title = "Lung Cancer", x = "Year", y = "PAF") +
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.75)+xlim(2005, 2019)
# bmi_lungmen <- ggplot(bmi, aes(x= yr, y = Lung)) + geom_line() + geom_point() + 
#   labs(title = "Lung Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "**BMI") +
#   theme_minimal ()+ ylim(0,.4)+xlim(2005, 2019)
# alcohol_lungmen <- ggplot(alcohol, aes(x= yr, y = Lung)) + geom_line() + geom_point() + 
#   labs(title = "Lung Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "**Alcohol") +
#   theme_minimal ()+ ylim(0,.45)+xlim(2005, 2019)
# #COLORECTAL CANCER
# smoking_colorectalmen <- ggplot(smoking, aes(x= yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Colorectal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.75) +xlim(2005, 2019)
# bmi_colorectalmen <- ggplot(bmi, aes(x= yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Colorectal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "BMI") +
#   theme_minimal () + ylim(0,.4)+xlim(2005, 2019)
# alcohol_colorectalmen <- ggplot(alcohol, aes(x= yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Colorectal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Alcohol") +
#   theme_minimal () + ylim(0,.45) +xlim(2005, 2019)
# fibre_colorectalmen <- ggplot(fibre, aes( x = yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Fibre", x = "Year", y = "PAF") + 
#   labs(caption = "**Colorectal Cancer") +
#   theme_minimal () + ylim(0,0.2) +xlim(2005, 2019)
# redmeat_colorectalmen <- ggplot(redmeat, aes( x = yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Red Meat", x = "Year", y = "PAF") + 
#   labs(caption = "**Colorectal Cancer") +
#   theme_minimal () + ylim(0,0.2) +xlim(2005, 2019)
# processedmeat_colorectalmen <- ggplot(processedmeat, aes( x = yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Processed Meat", x = "Year", y = "PAF") + 
#   labs(caption = "**Colorectal Cancer") +
#   theme_minimal () + ylim(0,0.2) +xlim(2005, 2019)
# #PANCREAS CANCER
# smoking_pancreasmen <- ggplot(smoking, aes(x= yr, y = Pancreas)) + geom_line() + geom_point() + 
#   labs(title = "Pancreas Cancer", x = "Year", y = "PAF") +
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.75) +xlim(2005, 2019)
# bmi_pancreasmen <- ggplot(bmi, aes(x= yr, y = Pancreas)) + geom_line() + geom_point() + 
#   labs(title = "Pancreas Cancer", x = "Year", y = "PAF") +
#   labs(caption = "BMI") +
#   theme_minimal () + ylim(0,.4) +xlim(2005, 2019)
# alcohol_pancreasmen <- ggplot(alcohol, aes(x= yr, y = Pancreas)) + geom_line() + geom_point() + 
#   labs(title = "Pancreas Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Alcohol") +
#   theme_minimal () + ylim(0,.45) +xlim(2005, 2019)
# #BREAST CANCER
# # smoking_breastmen <- ggplot(smoking, aes(x= yr, y = Breast)) + geom_line() + geom_point()  +
# #   labs(title = "Smoking", x = "Year", y = "PAF") + 
# #   labs(caption = "**Breast Cancer") +
# #   theme_minimal () + ylim(0,0.5)
# # bmi_breastmen <- ggplot(bmi, aes(x= yr, y = Breast)) + geom_line() + geom_point() +
# #   labs(title = "BMI", x = "Year", y = "PAF") +
# #   labs(caption = "**Breast Cancer") +
# #   theme_minimal () + ylim(0,0.5)
# # alcohol_breastmen <- ggplot(alcohol, aes(x= yr, y = Breast)) + geom_line() + geom_point() + 
# #   labs(title = "Alcohol", x = "Year", y = "PAF") + 
# #   labs(caption = "**Breast Cancer") +
# #   theme_minimal () + ylim(0,0.5)
# #SCC CANCER
# smoking_SCCmen <- ggplot(smoking, aes(x= yr, y = SCC)) + geom_line() + geom_point() + 
#   labs(title = "SCC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.75) +xlim(2005, 2019)
# bmi_SCCmen <- ggplot(bmi, aes(x= yr, y = SCC)) + geom_line() + geom_point() + 
#   labs(title = "SCC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "BMI") +
#   theme_minimal () + ylim(0,.4) +xlim(2005, 2019)
# alcohol_SCCmen <- ggplot(alcohol, aes(x= yr, y = SCC)) + geom_line() + geom_point() + 
#   labs(title = "SCC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Alcohol") +
#   theme_minimal () + ylim(0,.45) +xlim(2005, 2019)
# #AC CANCER
# smoking_ACmen <- ggplot(smoking, aes(x= yr, y = AC)) + geom_line() + geom_point() + 
#   labs(title = "AC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.75) +xlim(2005, 2019)
# bmi_ACmen <- ggplot(bmi, aes(x= yr, y = AC)) + geom_line() + geom_point() + 
#   labs(title = "AC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "BMI") +
#   theme_minimal () + ylim(0,0.4)+xlim(2005, 2019)
# alcohol_ACmen <- ggplot(alcohol, aes(x= yr, y = AC)) + geom_line() + geom_point() + 
#   labs(title = "AC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Alcohol") +
#   theme_minimal () + ylim(0,.45) +xlim(2005, 2019)
# 
# ############# WOMEN ###################
# smoking <- PAF_all %>% filter(riskfactor == "smoking", sex == "Women")
# bmi <- PAF_all %>% filter(riskfactor == "bmi", sex == "Women")
# alcohol <- PAF_all %>% filter(riskfactor == "alcohol", sex == "Women")
# fibre <- PAF_all %>% filter(riskfactor == "fibre", sex == "Women")
# redmeat <- PAF_all %>% filter(riskfactor =="redmeat", sex == "Women")
# processedmeat <- PAF_all %>% filter(riskfactor =="processedmeat", sex == "Women")
# #####################################################
# #LUNG CANCER
# smoking_lungwomen <- ggplot(smoking, aes(x= yr, y = Lung)) + geom_line() + geom_point() + 
#   labs(title = "Lung Cancer", x = "Year", y = "PAF") +
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.75)+xlim(2005, 2019)
# bmi_lungwomen <- ggplot(bmi, aes(x= yr, y = Lung)) + geom_line() + geom_point() + 
#   labs(title = "Lung Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "BMI") +
#   theme_minimal ()+ ylim(0,.4) +xlim(2005, 2019)
# alcohol_lungwomen <- ggplot(alcohol, aes(x= yr, y = Lung)) + geom_line() + geom_point() + 
#   labs(title = "Lung Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Alcohol") +
#   theme_minimal ()+ ylim(0,.45) +xlim(2005, 2019)
# #COLORECTAL CANCER
# smoking_colorectalwomen <- ggplot(smoking, aes(x= yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Colorectal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.75) +xlim(2005, 2019)
# bmi_colorectalwomen <- ggplot(bmi, aes(x= yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Colorectal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "BMI") +
#   theme_minimal () + ylim(0,.4) +xlim(2005, 2019)
# alcohol_colorectalwomen <- ggplot(alcohol, aes(x= yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Colorectal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Alcohol") +
#   theme_minimal () + ylim(0,.45) +xlim(2005, 2019)
# fibre_colorectalwomen <- ggplot(fibre, aes( x = yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Fibre", x = "Year", y = "PAF") + 
#   labs(caption = "**Colorectal Cancer") +
#   theme_minimal () + ylim(0,0.2) +xlim(2005, 2019)
# redmeat_colorectalwomen <- ggplot(redmeat, aes( x = yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Red Meat", x = "Year", y = "PAF") + 
#   labs(caption = "**Colorectal Cancer") +
#   theme_minimal () + ylim(0,0.2) +xlim(2005, 2019)
# processedmeat_colorectalwomen <- ggplot(processedmeat, aes( x = yr, y = Colorectal)) + geom_line() + geom_point() + 
#   labs(title = "Processed Meat", x = "Year", y = "PAF") + 
#   labs(caption = "**Colorectal Cancer") +
#   theme_minimal () + ylim(0,0.2) +xlim(2005, 2019)
# #PANCREAS CANCER
# smoking_pancreaswomen <- ggplot(smoking, aes(x= yr, y = Pancreas)) + geom_line() + geom_point() + 
#   labs(title = "Pancreas Cancer", x = "Year", y = "PAF") +
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.75) +xlim(2005, 2019)
# bmi_pancreaswomen <- ggplot(bmi, aes(x= yr, y = Pancreas)) + geom_line() + geom_point() + 
#   labs(title = "Pancreas Cancer", x = "Year", y = "PAF") +
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.4) +xlim(2005, 2019)
# alcohol_pancreaswomen <- ggplot(alcohol, aes(x= yr, y = Pancreas)) + geom_line() + geom_point() + 
#   labs(title = "Pancreas Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.45) +xlim(2005, 2019)
# #BREAST CANCER
# smoking_breastwomen <- ggplot(smoking, aes(x= yr, y = Breast)) + geom_line() + geom_point()  +
#   labs(title = "Breast Cancer", x = "Year", y = "PAF") +
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.75) +xlim(2005, 2019)
# bmi_breastwomen <- ggplot(bmi, aes(x= yr, y = Breast)) + geom_line() + geom_point() +
#   labs(title = "Breast Cancer", x = "Year", y = "PAF") +
#   labs(caption = "BMI") +
#   theme_minimal () + ylim(0,.4) +xlim(2005, 2019)
# alcohol_breastwomen <- ggplot(alcohol, aes(x= yr, y = Breast)) + geom_line() + geom_point() +
#   labs(title = "Breast Cancer", x = "Year", y = "PAF") +
#   labs(caption = "Alcohol") +
#   theme_minimal () + ylim(0,.45) +xlim(2005, 2019)
# #SCC CANCER
# smoking_SCCwomen <- ggplot(smoking, aes(x= yr, y = SCC)) + geom_line() + geom_point() + 
#   labs(title = "SCC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.75)+xlim(2005, 2019)
# bmi_SCCwomen <- ggplot(bmi, aes(x= yr, y = SCC)) + geom_line() + geom_point() + 
#   labs(title = "SCC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "BMI") +
#   theme_minimal () + ylim(0,.4) +xlim(2005, 2019)
# alcohol_SCCwomen <- ggplot(alcohol, aes(x= yr, y = SCC)) + geom_line() + geom_point() + 
#   labs(title = "SCC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Alcohol") +
#   theme_minimal () + ylim(0,.45) +xlim(2005, 2019)
# #AC CANCER
# smoking_ACwomen <- ggplot(smoking, aes(x= yr, y = AC)) + geom_line() + geom_point() + 
#   labs(title = "AC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Smoking") +
#   theme_minimal () + ylim(0,.75) +xlim(2005, 2019)
# bmi_ACwomen <- ggplot(bmi, aes(x= yr, y = AC)) + geom_line() + geom_point() + 
#   labs(title = "AC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "BMI") +
#   theme_minimal () + ylim(0,0.4)+xlim(2005, 2019)
# alcohol_ACwomen <- ggplot(alcohol, aes(x= yr, y = AC)) + geom_line() + geom_point() + 
#   labs(title = "AC Oesophageal Cancer", x = "Year", y = "PAF") + 
#   labs(caption = "Alcohol") +
#   theme_minimal () + ylim(0,.45)+xlim(2005, 2019)
# 
# ########################################################################################
# #Visualzing by Cancer Site 
# #Men
# Mlungcancer <- grid.arrange(smoking_lungmen, ncol = 1, top = "MEN")
# Mcolorectalcancer <- grid.arrange(smoking_colorectalmen, bmi_colorectalmen, alcohol_colorectalmen, fibre_colorectalmen, redmeat_colorectalmen, ncol = 2, top = "MEN")
# Mpancreascancer <- grid.arrange(smoking_pancreasmen, bmi_pancreasmen, ncol = 2, top = "MEN")
# MSCCcancer <- grid.arrange(smoking_SCCmen, bmi_SCCmen, alcohol_SCCmen, ncol = 2, top = "MEN")
# MACcancer <- grid.arrange(smoking_ACmen, bmi_ACmen, alcohol_ACmen, ncol = 2, top = "MEN")
# #Women
# Wlungcancer <- grid.arrange(smoking_lungwomen, ncol = 1, top = "WOMEN")
# Wcolorectalcancer <- grid.arrange(smoking_colorectalwomen, bmi_colorectalwomen, alcohol_colorectalwomen, fibre_colorectalwomen, redmeat_colorectalwomen, ncol = 2, top = "WOMEN")
# Wpancreascancer <- grid.arrange(smoking_pancreaswomen, bmi_pancreaswomen,ncol = 2, top = "WOMEN")
# Wbreastcancer <- grid.arrange( bmi_breastwomen, alcohol_breastwomen, ncol = 2, top = "WOMEN Estimates for Population Attributable Fractions on Breast Cancer")
# WSCCcancer <- grid.arrange(smoking_SCCwomen, bmi_SCCwomen, alcohol_SCCwomen, ncol = 2, top = "WOMEN")
# WACcancer <- grid.arrange(smoking_ACwomen, bmi_ACwomen, alcohol_ACwomen, ncol = 2, top = "WOMEN")
# #ALL
# lung <- grid.arrange(Mlungcancer, Wlungcancer, ncol=2, top = "Estimates for Population Attributable Fractions on Lung Cancer")
# colorectal <- grid.arrange(Mcolorectalcancer, Wcolorectalcancer, ncol=2, top = "Estimates for Population Attributable Fractions on Colorectal Cancer" )
# pancreas <- grid.arrange(Mpancreascancer, Wpancreascancer, ncol=2, top = "Estimates for Population Attributable Fractions on Pancreas Cancer")
# Wbreastcancer <- grid.arrange( bmi_breastwomen, alcohol_breastwomen, ncol = 2, top = "WOMEN Estimates for Population Attributable Fractions on Breast Cancer")
# SCC <- grid.arrange(MSCCcancer, WSCCcancer, ncol = 2, top = "Estimates for Population Attributable Fractions on SCC Cancer")
# AC <- grid.arrange(MACcancer, WACcancer, ncol = 2, top = "Estimates for Population Attributable Fractions on AC Cancer")
# 
# ggsave(filename = "PAF Figures/lungPAF.jpg", plot = lung, width = 8, height = 6, dpi = 300)
# ggsave(filename = "PAF Figures/ColorectalPAF.jpg", plot = colorectal, width = 8, height = 6, dpi = 300)
# ggsave(filename = "PAF Figures/PancreasPAF.jpg", plot = pancreas, width = 8, height = 6, dpi = 300)
# ggsave(filename = "PAF Figures/BreastPAF.jpg", plot = Wbreastcancer, width = 8, height = 6, dpi = 300)
# ggsave(filename = "PAF Figures/SCCPAF.jpg", plot = SCC, width = 8, height = 6, dpi = 300)
# ggsave(filename = "PAF Figures/ACPAF.jpg", plot = AC, width = 8, height = 6, dpi = 300)
# 
# ####################################################################################################
# 
# ##Visualizing by risk factor
# smokingmen <- grid.arrange(smoking_lungmen,smoking_SCCmen, smoking_ACmen, smoking_pancreasmen, smoking_colorectalmen, ncol = 5,  top = "MEN")
# smokingwomen <- grid.arrange(smoking_lungwomen, smoking_SCCwomen, smoking_ACwomen, smoking_pancreaswomen, smoking_colorectalwomen,ncol = 5,  top = "WOMEN")
# smoking_allcancersites <- grid.arrange(smokingmen, smokingwomen, nrow = 2, top = "Smoking as a Cancer Risk Factor")
# 
# bmimen <- grid.arrange(bmi_ACmen, bmi_colorectalmen, bmi_pancreasmen, ncol =4, top = "MEN")
# bmiwomen <- grid.arrange( bmi_ACwomen, bmi_colorectalwomen, bmi_breastwomen, bmi_pancreaswomen, ncol = 4, top = "WOMEN")
# bmi_allcancersites <- grid.arrange(bmimen, bmiwomen, nrow = 2, top = "BMI as a Cancer Risk Factor")
# 
# alcoholmen <- grid.arrange(alcohol_SCCmen, alcohol_colorectalmen, alcohol_ACmen, ncol=4, top = "MEN")
# alcoholwomen <- grid.arrange(alcohol_SCCwomen,alcohol_breastwomen, alcohol_colorectalwomen, alcohol_ACwomen, ncol = 4, top = "WOMEN")
# alcohol_allcancersites <- grid.arrange(alcoholmen, alcoholwomen, nrow=2, top = "Alcohol as a Cancer Risk Factor")
# 
# dietaryMEN <- grid.arrange(fibre_colorectalmen, redmeat_colorectalmen, processedmeat_colorectalmen, ncol=3, top = "Men")
# dietaryWOMEN <- grid.arrange(fibre_colorectalwomen, redmeat_colorectalwomen,processedmeat_colorectalwomen, ncol =3, top = "Women")
# dietary <- grid.arrange(dietaryMEN,dietaryWOMEN, nrow = 2,  top = "Dietary Cancer Risk Factors in Colorectal Cancer")
# 
# ggsave(filename = "PAF Figures/SMOKING.jpg", plot = smoking_allcancersites, width = 8, height = 6, dpi = 300)
# ggsave(filename = "PAF Figures/BMI.jpg", plot = bmi_allcancersites , width = 8, height = 6, dpi = 300)
# ggsave(filename = "PAF Figures/ALCOHOL.jpg", plot = alcohol_allcancersites, width = 8, height = 6, dpi = 300)
# ggsave(filename = "PAF Figures/DIET.jpg", plot = dietary, width = 8, height = 6, dpi = 300)
####################################################################################################
#Aggregate PAFS
library(segmented)
#need to extend the alcohol pafs back to 2005
#MEN
alcohol <- PAF_all %>% 
  dplyr::select(riskfactor, yr, sex, Colorectal, SCC) %>%
  filter(riskfactor == "alcohol", sex == "Men", !is.na(Colorectal))
#these are the years that I need to fill in data for 
backyears <- data.frame(yr = 2005:2010)
#Colorectal Cancer 
mcolorectal_lm<- lm(Colorectal ~ yr, data = alcohol)
mcolorectal_seg <- segmented(mcolorectal_lm, seg.Z = ~yr, npsi = 1) 
predictions <- predict(mcolorectal_seg, newdata = backyears)
backyears$Colorectal <- predictions
#SCC
mSCC_lm<- lm(SCC ~ yr, data = alcohol)
mSCC_seg <- segmented(mSCC_lm, seg.Z = ~yr, npsi = 1) 
predictions <- predict(mSCC_seg, newdata = backyears)
backyears$SCC <- predictions 
backyears <- backyears %>%
  mutate(riskfactor = "alcohol", sex = "Men")
menalcohol <- bind_rows(alcohol, backyears)

#visual
check <- ggplot(menalcohol, aes(x= yr, y = SCC)) + geom_line() + geom_point() + 
  labs(title = "Alcohol Prevalence Predictions - Men", x = "Year", y = "PAF") + 
  theme_minimal () + ylim(0,.5) +xlim(2005, 2019)
print(check)
check1 <- ggplot(menalcohol, aes(x= yr, y = Colorectal)) + geom_line() + geom_point() + 
  labs(title = "Alcohol Prevalence Predictions - Women", x = "Year", y = "PAF") + 
  theme_minimal () + ylim(0,.1) +xlim(2005, 2019)
print(check1)

#WOMEN
alcohol <- PAF_all %>% 
  dplyr::select(riskfactor, yr, sex, Breast, Colorectal, SCC) %>%
  filter(riskfactor == "alcohol", sex == "Women", !is.na(Colorectal))
#these are the years that I need to fill in data for 
backyears <- data.frame(yr = 2005:2010)
#Colorectal Cancer 
wcolorectal_lm<- lm(Colorectal ~ yr, data = alcohol)
wcolorectal_seg <- segmented(wcolorectal_lm, seg.Z = ~yr, npsi = 1) 
predictions <- predict(wcolorectal_seg, newdata = backyears)
backyears$Colorectal <- predictions
#SCC
wSCC_lm<- lm(SCC ~ yr, data = alcohol)
wSCC_seg <- segmented(wSCC_lm, seg.Z = ~yr, npsi = 1) 
predictions <- predict(wSCC_seg, newdata = backyears)
backyears$SCC <- predictions 
#Breast
wBreast_lm<- lm(Breast ~ yr, data = alcohol)
wBreast_seg <- segmented(wBreast_lm, seg.Z = ~yr, npsi = 1) 
predictions <- predict(wBreast_seg, newdata = backyears)
backyears$Breast <- predictions 
backyears <- backyears %>%
  mutate(riskfactor = "alcohol", sex = "Women")
womenalcohol <- bind_rows(alcohol, backyears)

#check
check <- ggplot(womenalcohol, aes(x= yr, y = SCC)) + geom_line() + geom_point() + 
  labs(title = "SCC Predictions", x = "Year", y = "PAF") + 
  labs(caption = "Smoking") +
  theme_minimal () + ylim(0,.5) +xlim(2005, 2019)
print(check)
check1 <- ggplot(womenalcohol, aes(x= yr, y = Colorectal)) + geom_line() + geom_point() + 
  labs(title = "Colorectal Predictions", x = "Year", y = "PAF") + 
  labs(caption = "Smoking") +
  theme_minimal () + ylim(0,.1) +xlim(2005, 2019)
print(check1)
check2 <- ggplot(womenalcohol, aes(x= yr, y = Breast)) + geom_line() + geom_point() + 
  labs(title = "Breast Predictions", x = "Year", y = "PAF") + 
  labs(caption = "Smoking") +
  theme_minimal () + ylim(0,.1) +xlim(2005, 2019)
print(check2)
#create a dataset of just the predicted values
alcohol <- bind_rows(menalcohol, womenalcohol) 
alcohol <- alcohol %>% 
  dplyr::filter(yr<=2010)
alcohol$combined <- paste(alcohol$riskfactor, alcohol$yr, sep = "")
#create a dataset of just the known alcohol values
alcohol_all <- PAF_all %>%
  filter(riskfactor =="alcohol")
alcohol_all <- alcohol_all %>%
  dplyr::filter(yr>2010)
#create a new dataset for all of the alcohol values (predicted and known) 
alcohol_all <- bind_rows(alcohol_all,alcohol)
#add them the full alcohol variable back into the dataset 
PAF_all <- PAF_all %>%
  filter(riskfactor !="alcohol")
PAF_all <- bind_rows(alcohol_all, PAF_all)

source("Code/PAF/AggregatePAFCode.R")
source("Code/PAF/CaseCode.R")
men <- grid.arrange(aggregatepaf_plot_men, mencases_visualized)
women <- grid.arrange(aggregatepaf_plot_women, womencases_visualized)
ggsave(filename = "PAF Figures/Men_AttributableCases.jpg", plot = men, width = 8, height = 6, dpi = 300)
ggsave(filename = "PAF Figures/Women_AttributableCases.jpg", plot = women, width = 8, height = 6, dpi = 300)


####################################################################################################
#Investigating the PAF relationships 

#SCENARIO ONE: broad PAF relationship: 
#creating a sequence of prevalence 
prevalencerange <- seq(0, 1, by = 0.01)
# Define a constant Relative Risk (RR)
RRconstantlow <- 1.2 # Example RR constant
RRconstant4 <- 1.5
RRconstantmed <- 5.0
RRconstanthigh <- 10.0 
# Calculate the PAF for each prevalence value
PAFlow <- (prevalencerange * (RRconstantlow-1)) / (1 + (prevalencerange * (RRconstantlow-1)))
PAF4 <- (prevalencerange * (RRconstant4-1)) / (1 + (prevalencerange * (RRconstant4-1)))
PAFmed <- (prevalencerange * (RRconstantmed-1)) / (1 + (prevalencerange * (RRconstantmed-1)))
PAFhigh <- (prevalencerange * (RRconstanthigh-1)) / (1 + (prevalencerange * (RRconstanthigh-1)))
# Plot the PAF against the prevalence
pafplotlow <- ggplot(data.frame(prevalence = prevalencerange, PAF = PAFlow), aes(x = prevalence, y = PAF)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "RR = 1.2",
       x = "RF Prevalence",
       y = "PAF") +
  ylim(0, 1) +
  theme_minimal()
pafplot4 <- ggplot(data.frame(prevalence = prevalencerange, PAF = PAF4), aes(x = prevalence, y = PAF)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "RR = 1.5",
       x = "RF Prevalence",
       y = "PAF") +
  ylim(0, 1) +
  theme_minimal()
pafplotmed <- ggplot(data.frame(prevalence = prevalencerange, PAF = PAFmed), aes(x = prevalence, y = PAF)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "RR = 5.0",
       x = "RF Prevalence",
       y = "PAF") +
  ylim(0, 1) +
  theme_minimal()
pafplothigh<- ggplot(data.frame(prevalence = prevalencerange, PAF = PAFhigh), aes(x = prevalence, y = PAF)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "RR = 10.0",
       x = "RF Prevalence",
       y = "PAF") +
  ylim(0, 1) +
  theme_minimal() 
# Showing the plot
changingpaf <- grid.arrange(pafplotlow, pafplot4, pafplotmed, pafplothigh, ncol=4)


#SCENARIO TWO: Smoking relationship - If there was 100% smoking cessation in 2019 with no initiation
#pulling the relevant 2019 prevalence 
malesmokers2019 <- cleanedprev %>%
  filter(variable == "smoker_men", datayear == 2019, country == "England") %>%
  pull(prev)
maleformersmokers2019 <- cleanedprev %>%
  filter(variable == "formersmoker_men", datayear == 2019, country == "England") %>%
  pull(prev)
maxformersmoker = maleformersmokers2019+malesmokers2019
RRformersmoke <- rr %>% 
  filter(riskfactor == "men_formersmoker") %>% 
  pull(lungRR)
RRcurrentsmoke <- rr %>% 
  filter(riskfactor == "men_currentsmoker") %>% 
  pull(lungRR)
#creating the range of possible formal smokers and corresponding current smokers across the range in 2019 
formersmokingrange2019 <- seq(maleformersmokers2019, maxformersmoker, by = 0.01)
smokingprevalence2019 <- data.frame(count = seq(1,19))
smokingprevalence2019$former <- formersmokingrange2019
smokingprevalence2019$current <- (maxformersmoker - smokingprevalence2019$former)
#calculating the PAFs across the time 
smokingprevalence2019$paf <- ((smokingprevalence2019$current * (RRcurrentsmoke-1))+ (smokingprevalence2019$former * (RRformersmoke-1))) / (1 + (smokingprevalence2019$current * (RRcurrentsmoke-1))+ (smokingprevalence2019$former * (RRformersmoke-1)))
#plot the scenario graph 
pafformersmoker<- ggplot(data.frame(prevalence = formersmokingrange2019, PAF = smokingprevalence2019$paf), aes(x = prevalence, y = PAF)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Lung Cancer PAF if all Current Smokers became Former Smokers in English Men in 2019",
       x = "Former Smoker Prevalence",
       y = "PAF") +
  ylim(0, 1) +
  theme_minimal() 
print(pafformersmoker)
#FOLLOWING-UP : SCENARIO TWO
male_smoking <- ggplot(cleanedprev %>% filter (country == "England", variable %in% c("smoker_men", "formersmoker_men")), aes(x = datayear, y = prev, color = variable)) + 
  geom_point() + 
  geom_line() +
  labs(title = "Men",
       x = "Year",
       y = "Prevalence") +
  ylim(0.1, 0.35) +
  theme_minimal()
female_smoking <- ggplot(cleanedprev %>% filter (country == "England", variable %in% c("smoker_women", "formersmoker_women")), aes(x = datayear, y = prev, color = variable)) + 
  geom_point() + 
  geom_line() +
  labs(title = "Women",
       x = "Year",
       y = "Prevalence") +
  ylim(0.1, 0.35) +
  theme_minimal()
smokingdistribution <- grid.arrange(male_smoking, female_smoking, ncol = 2, top = "Smoking Over Time: Current and Former Smokers in England ")


#SCENARIO THREE: all those obese lose weight to fall into the overweight category
male_bmi <- ggplot(cleanedprev %>% filter (country == "England", variable %in% c("overweight_men", "obese_men")), aes(x = datayear, y = prev, color = variable)) + 
  geom_point() + 
  geom_line() +
  labs(title = "Men",
       x = "Year",
       y = "Prevalence") +
  ylim(0, 0.45) +
  theme_minimal()
female_bmi <- ggplot(cleanedprev %>% filter (country == "England", variable %in% c("overweight_women", "obese_women")), aes(x = datayear, y = prev, color = variable)) + 
  geom_point() + 
  geom_line() +
  labs(title = "Women",
       x = "Year",
       y = "Prevalence") +
  ylim(0, 0.45) +
  theme_minimal()
bmidistribution <- grid.arrange(male_bmi, female_bmi, ncol = 2, top = "BMI Over Time: Obese amd Overweight in England ")

##################################################################################
