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
  ) %>%
  filter(
    year >= 2009 & year <= 2019,
    exposure %in% c("Light_alcohol", "Medium_alcohol","Heavy_alcohol","BMI_obese_men","BMI_obese_women","BMI_overweight_men","BMI_overweight_women","Current_Smoking_men","Current_Smoking_women","Former_Smoking_men","Former_Smoking_women", "Processed_Meat","Red_Meat", "Fibre")
  ) %>%
  select(year, age_group, sex, variable, exposure, value)

#Grouping the data by age_group, sex, and exposure and fitting a linear model on the value column
rf_linearmodel <- riskfactors %>%
  group_by(age_group, sex, exposure) %>%  # Group by age_group, sex, and exposure
  do({
    model <- lm(value ~ year, data = .)# Fit the linear model
    tidy_model <- tidy(model)# Extract coefficients and p-values using broom::tidy()
    tidy_model #%>% # Return the relevant parts
     # filter(term == "year")  # Filter for the 'year' coefficient
  }) %>%
  ungroup()
#adding the overarching exposure variable 
rf_linearmodel <- rf_linearmodel %>%
  mutate(
    variable = case_when(
      exposure %in% riskfactors$exposure ~ 
        riskfactors$variable[match(exposure, riskfactors$exposure)], # Match exposures
      TRUE ~ "none"
    )
  )


##Returns list of statistically increasing risk factors
sigincrease_rf <- rf_linearmodel %>%
  filter(
    term == "year"
  )%>%
  mutate(
    term = str_replace(term, "exposure", ""), 
    sigincrease = ifelse(estimate > 0 & p.value < 0.05, 1, 0) #coded 1 if there is a significant increase and a positive coefficient
    ) 


##Do risk factors change by age group 
sigchange_byage <- rf_linearmodel %>%
  group_by(
    sex, exposure
    ) %>%  # Group by sex and term
  summarise(
    t_test = list(t.test(estimate)),  # t-test
    .groups = "drop"  # Drop grouping structure
  ) %>%
  mutate(
    t_statistic = map_dbl(t_test, ~ .x$statistic),  # Extract t-statistic
    p_value = map_dbl(t_test, ~ .x$p.value)        # Extract p-value
  ) %>%
  select(sex, exposure, t_statistic, p_value)


############trying to graph 

coef_wide <- rf_linearmodel %>%
  pivot_wider(
    names_from = term,
    values_from = estimate, 
    id_cols = c("age_group", "sex", "exposure", "variable")
    ) %>%
  rename(
    intercept = "(Intercept)"
  ) %>%
  select(
    age_group, sex, variable, exposure, intercept, year
  ) %>%
  filter(
    #variable == "Smoking", 
    exposure == "Red_Meat"
    )

#Alcohol 
alcohol_plot <- ggplot(riskfactors %>% filter(variable == "Alcohol"), aes(x = year, y = value, color = exposure)) +
  geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
  facet_grid(sex ~ age_group) +  # grid by both age_group and sex
  geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
              linetype = "solid", size = 1) +  # Add regression lines
  theme_minimal() +
  scale_color_manual( #fixing the colors 
    values = c(
      "Heavy_alcohol" = "#3d405b",  
      "Medium_alcohol" = "#81b29a", 
      "Light_alcohol" = "#e07a5f"  
    )
  ) +
  labs(
    title = "Alcohol",
    x = "Year",
    y = "Population prevalence",
    color = "Exposure Category"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
    panel.grid.minor = element_blank()
  )


#BMI 
bmi_plot <- ggplot(riskfactors %>% filter(variable == "BMI"), aes(x = year, y = value, color = exposure)) +
  geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
  facet_grid(sex ~ age_group) +  # grid by both age_group and sex
  geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
              linetype = "solid", size = 1) +  # Add regression lines
  theme_minimal() +
  scale_color_manual( #fixing the colors 
    values = c(
      "BMI_obese_men" = "#81b29a",  
      "BMI_overweight_men" = "#e07a5f", 
      "BMI_obese_women" = "#81b29a",  
      "BMI_overweight_women" = "#e07a5f" 
    )
  ) +
  labs(
    title = "BMI",
    x = "Year",
    y = "Population prevalence",
    color = "Exposure Category"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

#Smoking 
smoking_plot <- ggplot(riskfactors %>% filter(variable == "Smoking"), aes(x = year, y = value, color = exposure)) +
  geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
  facet_grid(sex ~ age_group) +  # grid by both age_group and sex
  geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
              linetype = "solid", size = 1) +  # Add regression lines
  theme_minimal() +
  scale_color_manual( #fixing the colors 
    values = c(
      "Current_Smoking_men" = "#81b29a",  
      "Former_Smoking_men" = "#e07a5f", 
      "Current_Smoking_women" = "#81b29a",  
      "Former_Smoking_women" = "#e07a5f" 
    )
  ) +
  labs(
    title = "Smoking",
    x = "Year",
    y = "Population prevalence",
    color = "Exposure Category"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

#Fibre
fibre_plot <- ggplot(riskfactors %>% filter(exposure == "Fibre"), aes(x = year, y = value, color = exposure)) +
  geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
  facet_grid(sex ~ age_group) +  # grid by both age_group and sex
  geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
              linetype = "solid", size = 1) +  # Add regression lines
  theme_minimal() +
  scale_color_manual( #fixing the colors 
    values = c(
      "Fibre" = "#81b29a"
    )
  ) +
  labs(
    title = "Fibre Deficiency (Based on 30g/day recommendation)",
    x = "Year",
    y = "Population prevalence",
    color = "Exposure"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

#Processed meat
processedmeat_plot <- ggplot(riskfactors %>% filter(exposure == "Processed_Meat"), aes(x = year, y = value, color = exposure)) +
  geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
  facet_grid(sex ~ age_group) +  # grid by both age_group and sex
  geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
              linetype = "solid", size = 1) +  # Add regression lines
  theme_minimal() +
  scale_color_manual( #fixing the colors 
    values = c(
      "Processed_Meat" = "#81b29a"
    )
  ) +
  labs(
    title = "Processed Meat Consumption",
    x = "Year",
    y = "Population prevalence",
    color = "Sex"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

#Red Meat
redmeat_plot <- ggplot(riskfactors %>% filter(exposure == "Red_Meat"), aes(x = year, y = value, color = exposure)) +
  geom_point(alpha = 0.6, size = 2) +  # Scatter plot for the original data
  facet_grid(sex ~ age_group) +  # grid by both age_group and sex
  geom_abline(data = coef_wide, aes(intercept = intercept, slope = year, color = exposure), 
              linetype = "solid", size = 1) +  # Add regression lines
  theme_minimal() +
  scale_color_manual( #fixing the colors 
    values = c(
      "Red_Meat" = "#81b29a"
    )
  ) +
  labs(
    title = "Red Meat Consumption",
    x = "Year",
    y = "Population prevalence",
    color = "Exposure"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    panel.grid.major = element_line(size = 0.5, linetype = "dotted"),
    panel.grid.minor = element_blank()
  )
print(alcohol_plot)
print(bmi_plot)
print(smoking_plot)
print(fibre_plot)
print(processedmeat_plot)
print(redmeat_plot)
