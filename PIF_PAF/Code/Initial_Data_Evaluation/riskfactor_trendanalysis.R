#This analysis tests of the linear trends of each risk factor are
  # (1) increasing slope 
  # (2) if the slope is different split by age 
library(dplyr)
library(broom)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)


#loading in and cleaning the datasets 
riskfactors<- read.csv("../../../Data/Cleaned_Data/clean_rf_data.csv")


riskfactors <- riskfactors %>%
  mutate(
    level = str_trim(level), 
    variable = str_trim(variable),
    value = as.numeric(value),
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
    exposure = ifelse(variable == "processedmeat_consumption_median", "Processed_Meat", exposure), 
    exposure = ifelse(variable == "redmeat_consumption_median", "Red_Meat", exposure), 
    exposure = ifelse(variable == "fibre_consumption_mean", "Fibre", exposure),
    value = ifelse(exposure =="Fibre", 30-value, value), #calculates fibre deficiency
    variable = if_else(variable == "alcohol_amt", "Alcohol", variable), 
    variable = if_else(variable == "bmi", "BMI", variable),
    variable = if_else(variable == "smoking_status", "Smoking", variable),
    variable = if_else(variable == "fibre_consumption", "Fibre", variable),
    variable = if_else(variable == "redmeat_consumption", "RedMeat", variable),
    variable = if_else(variable == "processed_meat_consumption", "ProcessedMeat", variable),
    SE = sqrt((value*(1-value))/N) #this is to calculate the proportion SE but only for BMI/Smoking/Alcohol
  ) %>%
  filter(
    #year >= 2009 & year <= 2019, #selecting the years of interest 
    exposure %in% c("Light_alcohol", "Medium_alcohol","Heavy_alcohol","Non-Drinker","BMI_obese_men","BMI_obese_women","BMI_overweight_men","BMI_overweight_women","Healthy Weight","Current_Smoking_men","Current_Smoking_women","Former_Smoking_men","Former_Smoking_women","Never", "Processed_Meat","Red_Meat", "Fibre")
  )%>%
  dplyr::select(variable, age_group, sex, exposure, year, value, SE)

#organizing and grouping the data correctly for joint point 
rf_clean <- riskfactors %>%
  filter(
    variable %in% c("Alcohol", "Smoking", "BMI")
  ) %>%
  arrange(
    exposure, age_group, sex, year
  )
dietrf_clean <- riskfactors %>%
  filter(
    exposure %in% c("Red_Meat", "Processed_Meat", "Fibre")
  ) %>%
  arrange(
    exposure, age_group, sex, year
  )

#saving the data frame for joint point 
write.table(
  rf_clean, 
   file = "../Data/rf_forjointpoint.txt", 
   sep = "\t", 
   row.names = FALSE, 
   quote = FALSE
 )

#****DIET DATA CANT GO ON GITHUB
write.table(
  dietrf_clean,
  file = "../../../Data/Cleaned_Data/dietrf_forjointpoint.txt",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)



#Grouping the data by age_group, sex, and exposure and fitting a linear model on the value column
# rf_linearmodel <- riskfactors %>%
#   group_by(age_group, sex, exposure) %>%  # Group by age_group, sex, and exposure
#   do({
#     model <- lm(value ~ year, data = .)# Fit the linear model
#    tidy_model <- tidy(model)# Extract coefficients and p-values using broom::tidy()
#     tidy_model #%>% # Return the relevant parts
#      # filter(term == "year")  # Filter for the 'year' coefficient
#   }) %>%
#   ungroup()

#testing for homoscedacity using the Breusch-Pagan test 
bpresults <- riskfactors %>%
  group_by(age_group, sex, exposure) %>%  # Group 
  summarise(
    bptest_pvalue = {
      model <- lm(value ~ year, data = cur_data())  # Fit linear model for the group
      test <- bptest(model)  # Perform Breusch-Pagan test
      test$p.value  # Extract p-value
    },
    .groups = "drop"  # Ungroup the result for a clean output
  )


# #adding the overarching exposure variable 
# rf_linearmodel <- rf_linearmodel %>%
#   mutate(
#     variable = case_when(
#       exposure %in% riskfactors$exposure ~ 
#         riskfactors$variable[match(exposure, riskfactors$exposure)], # Match exposures
#       TRUE ~ "none"
#     )
#   )
# write.csv(rf_linearmodel, file = "../Data/riskfactor_lineartrends.csv", row.names = F)

##Returns list of statistically increasing risk factors
# sigincrease_rf <- rf_linearmodel %>%
#   filter(
#     term == "year"
#   )%>%
#   mutate(
#     term = str_replace(term, "exposure", ""), 
#     sigincrease = ifelse(estimate > 0 & p.value < 0.05, 1, 0) #coded 1 if there is a significant increase and a positive coefficient
#     ) 


###Do risk factors change by age group 
#this is a ztest for a difference in slopes 
#ztest = difference in slopes/difference in std. errors
# sigchange_byage <- rf_linearmodel %>%
#   group_by(
#     sex, exposure
#     ) %>% # Group by sex and term
#  mutate(
#    def = ifelse(age_group == "50+", 2, 1)
#  )%>%
#   filter(
#     term == "year"
#   )%>%
#    summarise(
#      estimate_1 = sum(estimate[def == 1]), # Extract estimate for 20-49
#      estimate_2 = sum(estimate[def == 2]), # Extract estimate for 50+
#      std_error_1 = sum(std.error[def == 1]), # Extract std.error for 20-49
#      std_error_2 = sum(std.error[def == 2]), # Extract std.error for 50+
#      z_value = (estimate_1 - estimate_2) / sqrt(std_error_1^2 + std_error_2^2), # Compute z-value
#      p_value = 2 * (1 - pnorm(abs(z_value))) # Compute p-value
#   )%>%
#   mutate(
#     sig = ifelse(p_value < 0.05, 1, 0) #if there is a significant difference (arbitrary cutoff) it will be coded 1, if insignificant it will be coded 0
#     ) %>%
#   dplyr::select(
#     sex, exposure, z_value, p_value,sig 
#   )


############trying to graph 
# 
# coef_wide_full <- rf_linearmodel %>%
#   pivot_wider(
#     names_from = term,
#     values_from = estimate, 
#     id_cols = c("age_group", "sex", "exposure", "variable")
#     ) %>%
#   rename(
#     intercept = "(Intercept)"
#   ) %>%
#   select(
#     age_group, sex, variable, exposure, intercept, year
#   ) 
# 
# #Alcohol
# coef_wide <- coef_wide_full %>%
#   filter(
#     variable == "Alcohol"
#   )
# alcohol_plot <- ggplot(riskfactors %>% filter(variable == "Alcohol"), aes(x = year, y = value, color = exposure)) +
#   geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
#   facet_grid(sex ~ age_group) +  # grid by both age_group and sex
#   geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
#               linetype = "solid", size = 1) +  # Add regression lines
#   theme_minimal() +
#   scale_color_manual( #fixing the colors 
#     values = c(
#       "Heavy_alcohol" = "#3d405b",  
#       "Medium_alcohol" = "#81b29a", 
#       "Light_alcohol" = "#e07a5f"  
#     )
#   ) +
#   labs(
#     title = "Alcohol",
#     x = "Year",
#     y = "Population prevalence",
#     color = "Exposure Category"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
#     panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
#     panel.grid.minor = element_blank()
#   )
# 
# 
# #BMI 
# coef_wide <- coef_wide_full %>%
#   filter(
#     variable == "BMI"
#   )
# bmi_plot <- ggplot(riskfactors %>% filter(variable == "BMI"), aes(x = year, y = value, color = exposure)) +
#   geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
#   facet_grid(sex ~ age_group) +  # grid by both age_group and sex
#   geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
#               linetype = "solid", size = 1) +  # Add regression lines
#   theme_minimal() +
#   scale_color_manual( #fixing the colors 
#     values = c(
#       "BMI_obese_men" = "#81b29a",  
#       "BMI_overweight_men" = "#e07a5f", 
#       "BMI_obese_women" = "#81b29a",  
#       "BMI_overweight_women" = "#e07a5f" 
#     )
#   ) +
#   labs(
#     title = "BMI",
#     x = "Year",
#     y = "Population prevalence",
#     color = "Exposure Category"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
#     panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
#     panel.grid.minor = element_blank()
#   )
# 
# #Smoking 
# coef_wide <- coef_wide_full %>%
#   filter(
#     variable == "Smoking"
#   )
# smoking_plot <- ggplot(riskfactors %>% filter(variable == "Smoking"), aes(x = year, y = value, color = exposure)) +
#   geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
#   facet_grid(sex ~ age_group) +  # grid by both age_group and sex
#   geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
#               linetype = "solid", size = 1) +  # Add regression lines
#   theme_minimal() +
#   scale_color_manual( #fixing the colors 
#     values = c(
#       "Current_Smoking_men" = "#81b29a",  
#       "Former_Smoking_men" = "#e07a5f", 
#       "Current_Smoking_women" = "#81b29a",  
#       "Former_Smoking_women" = "#e07a5f" 
#     )
#   ) +
#   labs(
#     title = "Smoking",
#     x = "Year",
#     y = "Population prevalence",
#     color = "Exposure Category"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
#     panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
#     panel.grid.minor = element_blank()
#   )
# 
# #Fibre
# coef_wide <- coef_wide_full %>%
#   filter(
#     exposure == "Fibre"
#   )
# fibre_plot <- ggplot(riskfactors %>% filter(exposure == "Fibre"), aes(x = year, y = value, color = exposure)) +
#   geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
#   facet_grid(sex ~ age_group) +  # grid by both age_group and sex
#   geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
#               linetype = "solid", size = 1) +  # Add regression lines
#   theme_minimal() +
#   scale_color_manual( #fixing the colors 
#     values = c(
#       "Fibre" = "#81b29a"
#     )
#   ) +
#   labs(
#     title = "Fibre Deficiency (Based on 30g/day recommendation)",
#     x = "Year",
#     y = "Population prevalence",
#     color = "Exposure"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
#     panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
#     panel.grid.minor = element_blank()
#   )
# 
# #Processed meat
# coef_wide <- coef_wide_full %>%
#   filter(
#     exposure == "Processed_Meat"
#   )
# processedmeat_plot <- ggplot(riskfactors %>% filter(exposure == "Processed_Meat"), aes(x = year, y = value, color = exposure)) +
#   geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
#   facet_grid(sex ~ age_group) +  # grid by both age_group and sex
#   geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
#               linetype = "solid", size = 1) +  # Add regression lines
#   theme_minimal() +
#   scale_color_manual( #fixing the colors 
#     values = c(
#       "Processed_Meat" = "#81b29a"
#     )
#   ) +
#   labs(
#     title = "Processed Meat Consumption",
#     x = "Year",
#     y = "Population prevalence",
#     color = "Sex"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
#     panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
#     panel.grid.minor = element_blank()
#   )
# 
# #Red Meat
# coef_wide <- coef_wide_full %>%
#   filter(
#     exposure == "Red_Meat"
#   )
# redmeat_plot <- ggplot(riskfactors %>% filter(exposure == "Red_Meat"), aes(x = year, y = value, color = exposure)) +
#   geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
#   facet_grid(sex ~ age_group) +  # grid by both age_group and sex
#   geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
#               linetype = "solid", size = 1) +  # Add regression lines
#   theme_minimal() +
#   scale_color_manual( #fixing the colors 
#     values = c(
#       "Red_Meat" = "#81b29a"
#     )
#   ) +
#   labs(
#     title = "Red Meat Consumption",
#     x = "Year",
#     y = "Population prevalence",
#     color = "Exposure"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
#     panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
#     panel.grid.minor = element_blank()
#   )
# print(alcohol_plot)
# print(bmi_plot)
# print(smoking_plot)
# print(fibre_plot)
# print(processedmeat_plot)
# print(redmeat_plot)
