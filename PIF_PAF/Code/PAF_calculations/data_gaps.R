#This code is to create consistent risk factor data across the window of interest

#Data Gap1: Alcohol data from 2005 ( General Household Survey) and 2011(Household Survey England)
riskfactors_alcoholgap<- riskfactors %>%
  filter(
    variable == "alcohol_amt"
  )

# linear interpolation function
alcgap_function <- function(data) {
  all_years <- data.frame(year = seq(min(data$year), max(data$year)))
  interpolated <- merge(all_years, data, by = "year", all.x = TRUE)
  interpolated$value <- approx(data$year, data$value, xout = all_years$year)$y
  return(interpolated)
}
# Group by age_group, sex, and level, then interpolate
riskfactors_alcoholgap <- riskfactors_alcoholgap %>%
  group_by(age_group, sex, level) %>%
  group_modify(~ alcgap_function(.x)) %>%
  ungroup() %>%
  mutate(
    variable = "alcohol_amt"
  )


riskfactors <- riskfactors %>%
  filter(
    !(variable == "alcohol_amt")
  ) #taking out the old alcohol data 

riskfactors <- rbind(riskfactors,riskfactors_alcoholgap) #adding in the new filled in data 

#visual check to ensure interpolation is correct 
# men_data <- riskfactors_alcoholgap %>%
#   filter(
#     sex == "Men",
#     age_group == "20-49"
#          )
# 
# # Plot the graph
# ggplot(men_data, aes(x = year, y = value, color = level, group = level)) +
#   geom_line(size = 1) +
#   geom_point(size = 2) +
#   labs(
#     title = "Alcohol Consumption Over Years",
#     x = "Year",
#     y = "Value",
#     color = "Age Group"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "top",
#     plot.title = element_text(hjust = 0.5, size = 16),
#     axis.title = element_text(size = 12)
#   )
# 

