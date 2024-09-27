#looking at total PAFs and cancer cases 

#Men 
#clean the data so datasets can be joined 
cancerratesEM <- cancerratesEM %>%
  rename(cancersite = Cancer_Site) %>%
  mutate(cancersite = case_when(
    cancersite == "Pancreatic" ~ "Pancreas",
    TRUE ~ cancersite
  ))
cancerratesEM <- cancerratesEM %>%
  mutate(Yr_cases = Year)
aggregate_paf_men <- aggregate_paf_men %>%
  mutate(Yr_cases = Year +10)
#calculating attributable cases 
mencases <- aggregate_paf_men %>%
  left_join(cancerratesEM, by = c ("cancersite", "Yr_cases")) %>%
  mutate(attrcases = Incidences * PAF) %>%
  dplyr::select(cancersite, Yr_cases, PAF, attrcases)
#round cancer cases down 
mencases$attrcases <- floor(mencases$attrcases)
#graph the incident cancer cases
# mencases_visualized <- ggplot(mencases, aes(x = Yr_cases, y = attrcases, color = cancersite)) + 
#   geom_point() + 
#   geom_line() +
#   labs(title = "Cases Attributable to Modifiable RF in England for Men",
#        x = "Year",
#        y = "Attributable Cases") +
#   scale_x_continuous(breaks = 2005:2019, limits = c(2005, 2019)) +
#   scale_color_brewer(palette = "Dark2") +
#   theme_minimal() 
# print(mencases_visualized)

#Women 
#clean the data so datasets can be joined 
cancerratesEW <- cancerratesEW %>%
  rename(cancersite = Cancer_Site) %>%
  mutate(cancersite = case_when(
    cancersite == "Pancreatic" ~ "Pancreas",
    TRUE ~ cancersite
  ))
cancerratesEW <- cancerratesEW %>%
  mutate(Yr_cases = Year)
aggregate_paf_women <- aggregate_paf_women %>%
  mutate(Yr_cases = Year +10)
#calculating attributable cases 
womencases <- aggregate_paf_women %>%
  left_join(cancerratesEW, by = c ("cancersite", "Yr_cases")) %>%
  mutate(attrcases = Incidences * PAF) %>%
  dplyr::select(cancersite, Yr_cases, PAF, attrcases)
#round cancer cases down 
womencases$attrcases <- floor(womencases$attrcases)
#graph the incident cancer cases
# womencases_visualized <- ggplot(womencases, aes(x = Yr_cases, y = attrcases, color = cancersite)) + 
#   geom_point() + 
#   geom_line() +
#   labs(title = "Cases Attributable to Modifiable RF in England for Women",
#        x = "Year",
#        y = "Attributable Cases") +
#   scale_x_continuous(breaks = 2005:2019, limits = c(2005, 2019)) +
#   scale_color_brewer(palette = "Dark2") +
#   theme_minimal() 
# print(womencases_visualized)
