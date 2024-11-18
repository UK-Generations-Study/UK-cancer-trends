#This code will calculate the PAFs by cancer site per risk factor and as an aggregate number for the years: 2015 and 2019

#Before running any of this, please run "UK-cancer-trends\PIF_PAF\Code\RF_Data_Generation.qmd"

#read in the risk factor data
riskfactors<- read.csv("../../../Data/Cleaned_Data/clean_rf_data.csv")
