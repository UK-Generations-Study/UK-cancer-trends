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


