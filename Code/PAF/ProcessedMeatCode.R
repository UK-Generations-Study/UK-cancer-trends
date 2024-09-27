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
all_processedmeat <- data_ndhns |>
  filter(country == "England") |>
  group_by(surveyyear_cat, sex, country) |>
  mutate(processedmeat_exp = weight*(processed.redmeat  >= 50)) |>
  summarise(mean_weight = mean(weight), processedmeat_exp = 100*sum(processedmeat_exp)/sum(weight))

all_processedmeat_visualize <- ggplot(all_processedmeat, aes(x= surveyyear_cat, y = processedmeat_exp, colour = country)) +
    geom_line() +
    geom_point() +
    xlab("Year") +
    ylab("% eating over the processed meat guidelines") +
    ggtitle("%  Exposed to Procesed Meat Overconsumption**") +
    labs(colour = "Country", caption = "**Guidelines are under 50g per day") +
   facet_wrap(~sex, scales = "fixed", dir = "v")
#print(all_processedmeat_visualize)

#filling out the dataset so that there are datapoints for every year 
#MEN
all_processedmeat_M <- all_processedmeat %>% 
  filter(sex == "Men") %>%
  rename(yr = surveyyear_cat) %>%
  dplyr::select(-mean_weight)
all_processedmeat_M$yr <- floor(all_processedmeat_M$yr)
Mnew2008 <- all_processedmeat_M %>%
  filter(yr == 2009) %>%      
  mutate(yr = 2008) 
Mnew2010 <- all_processedmeat_M %>%
  filter(yr == 2011) %>%      
  mutate(yr = 2010) 
Mnew2012 <- all_processedmeat_M %>%
  filter(yr == 2013) %>%      
  mutate(yr = 2012) 
Mnew2014 <- all_processedmeat_M %>%
  filter(yr == 2015) %>%      
  mutate(yr = 2014) 
Mnew2016 <- all_processedmeat_M %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2016) 
Mnew2018 <- all_processedmeat_M %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2018) 
all_processedmeat_M <- bind_rows(all_processedmeat_M,Mnew2008, Mnew2010, Mnew2012, Mnew2014, Mnew2016, Mnew2018)

#WOMEN
all_processedmeat_W <- all_processedmeat %>% 
  filter(sex == "Women") %>%
  rename(yr = surveyyear_cat) %>%
  dplyr::select(-mean_weight)
all_processedmeat_W$yr <- floor(all_processedmeat_W$yr)
Wnew2008 <- all_processedmeat_W %>%
  filter(yr == 2009) %>%      
  mutate(yr = 2008) 
Wnew2010 <- all_processedmeat_W %>%
  filter(yr == 2011) %>%      
  mutate(yr = 2010) 
Wnew2012 <- all_processedmeat_W %>%
  filter(yr == 2013) %>%      
  mutate(yr = 2012) 
Wnew2014 <- all_processedmeat_W %>%
  filter(yr == 2015) %>%      
  mutate(yr = 2014) 
Wnew2016 <- all_processedmeat_W %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2016) 
Wnew2018 <- all_processedmeat_W %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2018) 
all_processedmeat_W <- bind_rows(all_processedmeat_W,Wnew2008, Wnew2010, Wnew2012, Wnew2014, Wnew2016,Wnew2018)

all_processedmeat <- rbind(all_processedmeat_M, all_processedmeat_W)


#pull the relevant risk ratio
rr <- rr %>% 
  mutate(riskfactor = str_trim(riskfactor, side = "both"))
RR_processedmeat <- rr %>% 
  filter(riskfactor == "processedmeat") %>%
  pull(colorectalRR)
ERR_processedmeat <- RR_processedmeat - 1

#calculating the paf 
all_processedmeat$prev <- all_processedmeat$processedmeat_exp/100
all_processedmeat$paf <- ((ERR_processedmeat*all_processedmeat$prev)/((ERR_processedmeat*all_processedmeat$prev) + 1))

#cleaning the data 
all_processedmeat<- all_processedmeat %>%
  mutate(riskfactor ="processedmeat")
all_processedmeat$combined <- paste(all_processedmeat$riskfactor, all_processedmeat$yr, sep = "")

all_processedmeat <- all_processedmeat %>% 
  mutate(Lung = NA, Breast = NA, Pancreas = NA, SCC = NA, AC = NA, Prostate = NA) %>% 
  rename(Colorectal = paf) %>% 
  dplyr::select(combined, yr, riskfactor, sex, Lung, Colorectal, Breast, Pancreas, SCC, AC, Prostate)