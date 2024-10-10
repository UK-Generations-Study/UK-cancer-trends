#load in necessary packages 
library(dplyr)
library(tidyr)
library(stringr)
library (ggplot2)
library(gridExtra)
library(rstudioapi)
library(table1)

#trying this file directory thing 
# Setting up wd for relative file paths
# This sets wd to wherever the document is saved - this should be the github desktop folder
# if(Sys.getenv("RSTUDIO") == '1' & !knitr::is_html_output()) { # If using Rstudio and not rendering
#   setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# } else if(Sys.getenv("RSTUDIO") != '1'){ # If using Rscript
#   initial.options <- commandArgs(trailingOnly = FALSE)
#   file.arg.name <- "--file="
#   script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
#   script.basename <- dirname(script.name)
#   setwd(file.path(getwd(), script.basename))
# }

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

###################################################################################################
#Alcohol back predictions using a logistic regression model 
cleanedprev <- cleanedprev %>% 
  dplyr::select(-X.1, -X.2)
alcohol <- cleanedprev %>% 
  filter(country == "England") %>%
  filter (datayear >= 2011 & datayear <= 2020) %>%
  filter(str_detect(variable, "alcohol"))

#MEN 
### 1: Light Alcohol Predictions
light <- alcohol %>% 
  filter(variable == "lightalcohol_men")
#fit a ligustic regression model for the available years
log <- glm(prev~datayear, data=light, family= binomial)
newyrs <- data.frame(datayear = 2005:2010)
predict <- predict(log, newdata=newyrs, type = "response")
done <-newyrs %>%
  mutate(prev=predict, variable = "lightalcohol_men", country = "England", sex = "Men", value = prev*100, age_group = "16+", X = "99")
light<- rbind(light, done)
### 2: Medium Alcohol Predictions
medium <- alcohol %>% 
  filter(variable == "medalcohol_men")
#fit a ligustic regression model for the available years
log <- glm(prev~datayear, data=medium, family= binomial)
newyrs <- data.frame(datayear = 2005:2010)
predict <- predict(log, newdata=newyrs, type = "response")
done <-newyrs %>%
  mutate(prev=predict, variable = "medalcohol_men", country = "England", sex = "Men", value = prev*100, age_group = "16+", X = "99")
medium<- rbind(medium, done)
### 3: Heavy Alcohol Predictions
heavy <- alcohol %>% 
  filter(variable == "heavyalcohol_men")
#fit a ligustic regression model for the available years
log <- glm(prev~datayear, data=heavy, family= binomial)
newyrs <- data.frame(datayear = 2005:2010)
predict <- predict(log, newdata=newyrs, type = "response")
done <-newyrs %>%
  mutate(prev=predict, variable = "heavyalcohol_men", country = "England", sex = "Men", value = prev*100, age_group = "16+", X = "99")
heavy<- rbind(heavy, done)
#complete male dataset
alcohol_fullmen <- rbind(light, medium, heavy)

#WOMEN 
### 1: Light Alcohol Predictions
light <- alcohol %>% 
  filter(variable == "lightalcohol_women")
#fit a ligustic regression model for the available years
log <- glm(prev~datayear, data=light, family= binomial)
newyrs <- data.frame(datayear = 2005:2010)
predict <- predict(log, newdata=newyrs, type = "response")
done <-newyrs %>%
  mutate(prev=predict, variable = "lightalcohol_women", country = "England", sex = "Women", value = prev*100, age_group = "16+", X = "99")
light<- rbind(light, done)
### 2: Medium Alcohol Predictions
medium <- alcohol %>% 
  filter(variable == "medalcohol_women")
#fit a ligustic regression model for the available years
log <- glm(prev~datayear, data=medium, family= binomial)
newyrs <- data.frame(datayear = 2005:2010)
predict <- predict(log, newdata=newyrs, type = "response")
done <-newyrs %>%
  mutate(prev=predict, variable = "medalcohol_women", country = "England", sex = "Women", value = prev*100, age_group = "16+", X = "99")
medium<- rbind(medium, done)
### 3: Heavy Alcohol Predictions
heavy <- alcohol %>% 
  filter(variable == "heavyalcohol_women")
#fit a ligustic regression model for the available years
log <- glm(prev~datayear, data=heavy, family= binomial)
newyrs <- data.frame(datayear = 2005:2010)
predict <- predict(log, newdata=newyrs, type = "response")
done <-newyrs %>%
  mutate(prev=predict, variable = "heavyalcohol_women", country = "England", sex = "Women", value = prev*100, age_group = "16+", X = "99")
heavy<- rbind(heavy, done)
#complete women dataset
alcohol_fullwomen <- rbind(light, medium, heavy)

alcoholall <- rbind(alcohol_fullmen, alcohol_fullwomen)
# 


#adding the alchol back in
allprev_trial <- cleanedprev %>%
  filter(!str_detect(variable, "alcohol"))
all<- rbind(alcoholall,allprev_trial)
all<- all %>%
  filter(datayear>=2005)
cleanedprev <- all 
##########################################################################################

#clean the rr calculation
rr <- rr %>% 
  mutate(across(-1, as.numeric))

# #MEN
cancerratesEM <- cancerrates %>% filter(Country == "England", Sex =="Men", Year <= 2019)
# 
# #WOMEN
cancerratesEW <- cancerrates %>% filter(Country == "England", Sex =="Women", Year <= 2019)
# 

########################################################################################
#Calculating PAFS
cleanedprev1 <- cleanedprev
#cancercases1 <- rbind (cancerrates_predictionsM %>% 
   # mutate(Country = "England", Sex = "Men"), cancerrates_predictionsW %>% mutate(Country = "England", Sex = "Women"))
cancercases1 <- cancerrates

#trying on a loop 
years <- seq(2005,2019)

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
source("C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\Code\\PAF\\AvgFibre_Code.R")
#run Red Meat 
source("C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\Code\\PAF\\AvgRedMeat_Code.R")
#run Processed Meat 
source("C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\Code\\PAF\\AvgProcessedMeat_Code.R")

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

source("C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\Code\\PAF\\AggregatePAFCode.R")
source("C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\Code\\PAF\\CaseCode.R")
#men <- grid.arrange(aggregatepaf_plot_men, mencases_visualized)
#women <- grid.arrange(aggregatepaf_plot_women, womencases_visualized)
#ggsave(filename = "C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\PAF Figures\\Men_AggregatePAF.jpg", plot = aggregatepaf_plot_men, width = 8, height = 6, dpi = 300)
#ggsave(filename = "C:\\Users\\zrichards.ICR\\OneDrive - The Institute of Cancer Research\\Git\\UK-cancer-trends\\PAF Figures\\Women_AggregatePAF.jpg", plot = aggregatepaf_plot_women, width = 8, height = 6, dpi = 300)


####################################################################################################


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
# pafformersmoker<- ggplot(data.frame(prevalence = formersmokingrange2019, PAF = smokingprevalence2019$paf), aes(x = prevalence, y = PAF)) +
#   geom_line(color = "blue", size = 1) +
#   labs(title = "Lung Cancer PAF if all Current Smokers became Former Smokers in English Men in 2019",
#        x = "Former Smoker Prevalence",
#        y = "PAF") +
#   ylim(0, 1) +
#   theme_minimal() 
# print(pafformersmoker)
#FOLLOWING-UP : SCENARIO TWO
# male_smoking <- ggplot(cleanedprev %>% filter (country == "England", variable %in% c("smoker_men", "formersmoker_men")), aes(x = datayear, y = prev, color = variable)) + 
#   geom_point() + 
#   geom_line() +
#   labs(title = "Men",
#        x = "Year",
#        y = "Prevalence") +
#   ylim(0.1, 0.35) +
#   theme_minimal()
# female_smoking <- ggplot(cleanedprev %>% filter (country == "England", variable %in% c("smoker_women", "formersmoker_women")), aes(x = datayear, y = prev, color = variable)) + 
#   geom_point() + 
#   geom_line() +
#   labs(title = "Women",
#        x = "Year",
#        y = "Prevalence") +
#   ylim(0.1, 0.35) +
#   theme_minimal()
# smokingdistribution <- grid.arrange(male_smoking, female_smoking, ncol = 2, top = "Smoking Over Time: Current and Former Smokers in England ")

##################################################################################
