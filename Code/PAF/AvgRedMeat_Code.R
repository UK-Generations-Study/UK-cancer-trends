#this is code originally from Reuben 

# Grab data on fibre and processed meat from the UKDS: https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6533
# Read in data years 1-4
data_1_4 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr1-4a_personleveldietarydata_uk_v2.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr1-4a_indiv_uk.tab)") |>
  mutate(weight = wti_UKY1234) |>
  dplyr::select(seriali, weight)
data_1_4 <- merge(data_1_4, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = SurveyYear, sex = Sex, age = Age, weight, country = Country, totalredmeat, processed.redmeat = ProcessedRedMeatg, beef = Beefg, lamb = Lambg, entrails = Offalg, other = OtherRedMeatg, pork = Porkg)

# Read in data years 5-6
data_5_6 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr5-6a_personleveldietarydata_v2.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr5-6a_indiv.tab)") |>
  mutate(weight = wti_Y56) |>
  dplyr::select(seriali, weight)
data_5_6 <- merge(data_5_6, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = Surveyyear, sex = Sex, age = Age, weight, country = Country, totalredmeat, processed.redmeat = ProcessedRedMeatg, beef = Beefg, lamb = Lambg, entrails = Offalg, other = OtherRedMeatg, pork = Porkg)

# Read in data years 7-8
data_7_8 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr7-8a_personleveldietarydata.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr7-8a_indiv.tab)") |>
  mutate(weight = wti_Y78) |>
  dplyr::select(seriali, weight)
data_7_8 <- merge(data_7_8, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = SurveyYear, sex = Sex, age = Age, weight, country = Country, totalredmeat, processed.redmeat = ProcessedRedMeatg, beef = Beefg, lamb = Lambg, entrails = Offalg, other = OtherRedMeatg, pork = Porkg)

# Read in data years 9-11
data_9_11 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr9-11a_personleveldietarydata_uk_20210831.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr9-11a_indiv_20211020.tab)") |>
  mutate(weight = wti_Y911) |>
  dplyr::select(seriali, weight)
data_9_11 <- merge(data_9_11, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = SurveyYear, sex = Sex, age = AgeR, weight, country = Country, totalredmeat, processed.redmeat = ProcessedRedMeatg, beef = Beefg, lamb = Lambg, entrails = Offalg, other = OtherRedMeatg, pork = Porkg)

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

#creating a red meat variable 
data_ndhns <- data_ndhns %>%
  mutate(redmeat_all = beef + lamb + pork + entrails + other)

#MEAN
all_redmeat <- data_ndhns |>
  filter(country == "England") |>
  group_by(surveyyear_cat, sex) |>
  summarise(
    redmeat_avg = weighted.mean(redmeat_all, weight, na.rm = TRUE), 
    .groups = "drop"
  )
all_redmeat_visualize <- ggplot(all_redmeat, aes(x= surveyyear_cat, y = redmeat_avg)) +
  geom_line() +
  geom_point() +
  xlab("Year") +
  ylab("G/day") +
  ggtitle("Average Red and Processed Meat Consumption in the UK **") +
  labs(colour = "Country") +
  facet_wrap(~sex, scales = "fixed", dir = "v") +
  theme_minimal() 
#print(all_redmeat_visualize)

#CATEGORIES
rm_cat <- data_ndhns |>
  filter(country == "England") |>
  mutate(redmeat_category = cut(
    redmeat_all, 
    breaks = c(seq(0, 100, by = 10), Inf),  
    labels = c(paste(seq(0, 90, by = 10), seq(10, 100, by = 10), sep = "-"), "100+"),  
    include.lowest = FALSE, # does not include 0 red meat
    left = TRUE  # upper boundary of interval excluded
  )) |>
  group_by(surveyyear_cat, sex) |>
  mutate(total_weight = sum(weight, na.rm = TRUE)) |>
  group_by(surveyyear_cat, sex, redmeat_category) |>
  summarise(
    weighted_count = sum(weight, na.rm = TRUE), 
    total_weight = first(total_weight),
    proportion = weighted_count / total_weight,
    .groups = "drop"
  )
rm_dist <- ggplot(rm_cat, aes(x = redmeat_category, y = proportion, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Red Meat Intake Category (g)") +
  ylab(" Count") +
  ggtitle(" Distribution of Red Meat Intake by Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Sex") +
  facet_wrap(~surveyyear_cat, ncol = 1)
#print(rm_dist)


#filling out the dataset so that there are datapoints for every year 
#MEN
all_redmeat_M <- rm_cat %>% 
  filter(sex == "Men") %>%
  rename(yr = surveyyear_cat) 
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
all_redmeat_W <- rm_cat %>% 
  filter(sex == "Women") %>%
  rename(yr = surveyyear_cat) 
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


####### PAF ##############
#pull RR
rr <- rr %>% 
  mutate(riskfactor = str_trim(riskfactor, side = "both"))
RR_redmeat <- rr %>% 
  filter(riskfactor == "redmeat") %>%
  pull(colorectalRR)
ERR_redmeat_perg <- (RR_redmeat -1)/100
#prep the data 
rm_ready <- all_redmeat %>% 
  mutate(midpoint =  case_when(
    redmeat_category == "100+" ~ 110,  # Set midpoint to 110 for "75+"
    TRUE ~ as.numeric(str_extract(redmeat_category, "(?<=-)[0-9]+")) - 5),
    rr = midpoint * ERR_redmeat_perg,
    calc = proportion * rr
  )
#paf_calculation
men <- rm_ready %>%
  filter(sex == "Men")
redmeat_paf_m <- men %>%
  group_by(yr) %>%            
  summarise(result = sum(calc, na.rm = TRUE) / (sum(calc, na.rm = TRUE) + 1), .groups = 'drop') %>%
  mutate(sex = "Men")

women <- rm_ready %>%
  filter(sex == "Women")
redmeat_paf_w <- women %>%
  group_by(yr) %>%            
  summarise(result = sum(calc, na.rm = TRUE) / (sum(calc, na.rm = TRUE) + 1), .groups = 'drop') %>%
  mutate(sex = "Women")
rm_paf <- rbind(redmeat_paf_m, redmeat_paf_w)

######################################
#cleaning the data 
rm_paf<- rm_paf %>%
  mutate(riskfactor ="redmeat", Lung = NA, Breast = NA, Pancreas = NA, SCC = NA, AC = NA, Prostate = NA) %>%
  rename(Colorectal = result) 
rm_paf$combined <- paste(rm_paf$riskfactor, rm_paf$yr, sep = "")


rmpaftrend <- ggplot(rm_paf, aes(x = yr, y = Colorectal, colour = sex)) +
  geom_line() +
  geom_point() +
  ylim(0, .1) +
  scale_x_continuous(breaks = 2008:2018, limits = c(2008, 2018)) +
  xlab("Year") +
  ylab("PAF") +
  ggtitle("PAF Red Meat Intake in England**") +
  labs(colour = "Country", caption = "**Guidelines are limit red meat to 0") +
  facet_wrap(~sex, scales = "fixed", dir = "v") +
  theme_minimal() 
#print(rmpaftrend)

all_redmeat<- rm_paf
