#this is code originally from Reuben 

# Grab data on fibre and processed meat from the UKDS: https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6533
# Read in data years 1-4
data_1_4 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr1-4a_personleveldietarydata_uk_v2.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr1-4a_indiv_uk.tab)") |>
  mutate(weight = wti_UKY1234) |>
  dplyr::select(seriali, weight)
data_1_4 <- merge(data_1_4, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = SurveyYear, sex = Sex, age = Age, weight, country = Country, totalredmeat, processed.redmeat = ProcessedRedMeatg)

# Read in data years 5-6
data_5_6 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr5-6a_personleveldietarydata_v2.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr5-6a_indiv.tab)") |>
  mutate(weight = wti_Y56) |>
  dplyr::select(seriali, weight)
data_5_6 <- merge(data_5_6, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = Surveyyear, sex = Sex, age = Age, weight, country = Country, totalredmeat, processed.redmeat = ProcessedRedMeatg)

# Read in data years 7-8
data_7_8 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr7-8a_personleveldietarydata.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr7-8a_indiv.tab)") |>
  mutate(weight = wti_Y78) |>
  dplyr::select(seriali, weight)
data_7_8 <- merge(data_7_8, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = SurveyYear, sex = Sex, age = Age, weight, country = Country, totalredmeat, processed.redmeat = ProcessedRedMeatg)

# Read in data years 9-11
data_9_11 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr9-11a_personleveldietarydata_uk_20210831.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr9-11a_indiv_20211020.tab)") |>
  mutate(weight = wti_Y911) |>
  dplyr::select(seriali, weight)
data_9_11 <- merge(data_9_11, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = SurveyYear, sex = Sex, age = AgeR, weight, country = Country, totalredmeat, processed.redmeat = ProcessedRedMeatg)

# RBind all data together
data_ndhns <- rbind(data_1_4, data_5_6) |>
  rbind(data_7_8) |>
  rbind(data_9_11) |>
  # Filter to adults
  filter(age >= 19) |>
  mutate(country = if_else(country %in% c("NI", "Northern Ireland"), "N. Ireland", country)) |>
  # Create new weights
 #group_by(sex, country) |>
  mutate(
    weight = case_when(
      between(surveyyear, 1, 4) ~ weight * sum(weight) * (1/sum(weight*between(surveyyear, 1, 4))) * (4/11),
      between(surveyyear, 5, 6) ~ weight * sum(weight) * (1/sum(weight*between(surveyyear, 5, 6))) * (2/11),
      between(surveyyear, 7, 8) ~ weight * sum(weight) * (1/sum(weight*between(surveyyear, 7, 8))) * (2/11),
      between(surveyyear, 9, 11) ~ weight * sum(weight) * (1/sum(weight*between(surveyyear, 9, 11))) * (3/11)),
    
    weight = weight/mean(weight),
    
    sex = if_else(sex == 1, "Men", "Women"),
    
    surveyyear_cat = case_when(
      surveyyear <= 2 ~ 2009,
      surveyyear <= 4 ~ 2011,
      surveyyear <= 6 ~ 2013,
      surveyyear <= 8 ~ 2015,
      surveyyear <= 11 ~ 2017.5,
      TRUE ~ NA)
  )


#looking at prevalence of over 100g of red meat 
all_redmeat <- data_ndhns |>
  filter(country == "England") |>
  group_by(surveyyear_cat, sex, country) |>
  mutate(redmeat_exp = weight*(totalredmeat  >= 100)) |>
  summarise(mean_weight = mean(weight), redmeat_exp = 100*sum(redmeat_exp)/sum(weight))

all_redmeat_visualize <- ggplot(all_redmeat, aes(x= surveyyear_cat, y = redmeat_exp, colour = country)) +
  geom_line() +
  geom_point() +
  xlab("Year") +
  ylab("% eating over the red meat guidelines") +
  ggtitle("%  Exposed to Red Meat Overconsumption**") +
  labs(colour = "Country", caption = "**Guidelines are under 100g per day") +
  facet_wrap(~sex, scales = "fixed", dir = "v")
print(all_redmeat_visualize)

#filling out the dataset so that there are datapoints for every year 
#MEN
all_redmeat_M <- all_redmeat %>% 
  filter(sex == "Men") %>%
  rename(yr = surveyyear_cat) %>%
  dplyr::select(-mean_weight)
all_redmeat_M$yr <- floor(all_redmeat_M$yr)
Mnew2008 <- all_redmeat_M %>%
  filter(yr == 2009) %>%      
  mutate(yr = 2008) 
Mnew2010 <- all_redmeat_M %>%
  filter(yr == 2011) %>%      
  mutate(yr = 2010) 
Mnew2012 <- all_redmeat_M %>%
  filter(yr == 2013) %>%      
  mutate(yr = 2012) 
Mnew2014 <- all_redmeat_M %>%
  filter(yr == 2015) %>%      
  mutate(yr = 2014) 
Mnew2016 <- all_redmeat_M %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2016) 
Mnew2018 <- all_redmeat_M %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2018) 
all_redmeat_M <- bind_rows(all_redmeat_M,Mnew2008, Mnew2010, Mnew2012, Mnew2014, Mnew2016, Mnew2018)

#WOMEN
all_redmeat_W <- all_redmeat %>% 
  filter(sex == "Women") %>%
  rename(yr = surveyyear_cat) %>%
  dplyr::select(-mean_weight)
all_redmeat_W$yr <- floor(all_redmeat_W$yr)
Wnew2008 <- all_redmeat_W %>%
  filter(yr == 2009) %>%      
  mutate(yr = 2008) 
Wnew2010 <- all_redmeat_W %>%
  filter(yr == 2011) %>%      
  mutate(yr = 2010) 
Wnew2012 <- all_redmeat_W %>%
  filter(yr == 2013) %>%      
  mutate(yr = 2012) 
Wnew2014 <- all_redmeat_W %>%
  filter(yr == 2015) %>%      
  mutate(yr = 2014) 
Wnew2016 <- all_redmeat_W %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2016) 
Wnew2018 <- all_redmeat_W %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2018) 
all_redmeat_W <- bind_rows(all_redmeat_W,Wnew2008, Wnew2010, Wnew2012, Wnew2014, Wnew2016,Wnew2018)

all_redmeat <- rbind(all_redmeat_M, all_redmeat_W)


#pull the relevant risk ratio
RR_redmeat <- rr %>% 
  filter(riskfactor == "redmeat") %>%
  pull(colorectalRR)
ERR_redmeat <- RR_redmeat - 1

#calculating the paf 
all_redmeat$prev <- all_redmeat$redmeat_exp/100
all_redmeat$paf <- ((ERR_redmeat*all_redmeat$prev)/((ERR_redmeat*all_redmeat$prev) + 1))

#cleaning the data 
all_redmeat<- all_redmeat %>%
  mutate(riskfactor ="redmeat")
all_redmeat$combined <- paste(all_redmeat$riskfactor, all_redmeat$yr, sep = "")

all_redmeat <- all_redmeat %>% 
  mutate(Lung = NA, Breast = NA, Pancreas = NA, SCC = NA, AC = NA, Prostate = NA) %>% 
  rename(Colorectal = paf) %>% 
  dplyr::select(combined, yr, riskfactor, sex, Lung, Colorectal, Breast, Pancreas, SCC, AC, Prostate)