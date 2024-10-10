#this is code from Reuben 
#fibre_summary is the prevalence of meeting the fibre guidlines by year in England 

# Grab data on fibre and processed meat
# Read in data years 1-4
data_1_4 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr1-4a_personleveldietarydata_uk_v2.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr1-4a_indiv_uk.tab)") |>
  mutate(weight = wti_UKY1234) |>
  dplyr::select(seriali, weight)
data_1_4 <- merge(data_1_4, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = SurveyYear, totalredmeat, aoac_fibre = AOACFibreg, sex = Sex, age = Age, weight, country = Country, processed.poultry = ProcessedPoultryg, processed.redmeat = ProcessedRedMeatg, burgers = Burgersg, sausages = Sausagesg)

# Read in data years 5-6
data_5_6 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr5-6a_personleveldietarydata_v2.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr5-6a_indiv.tab)") |>
  mutate(weight = wti_Y56) |>
  dplyr::select(seriali, weight)
data_5_6 <- merge(data_5_6, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = Surveyyear, totalredmeat, aoac_fibre = AOACFibreg, sex = Sex, age = Age, weight, country = Country, processed.poultry = ProcessedPoultryg, processed.redmeat = ProcessedRedMeatg, burgers = Burgersg, sausages = Sausagesg)

# Read in data years 7-8
data_7_8 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr7-8a_personleveldietarydata.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr7-8a_indiv.tab)") |>
  mutate(weight = wti_Y78) |>
  dplyr::select(seriali, weight)
data_7_8 <- merge(data_7_8, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = SurveyYear, totalredmeat, aoac_fibre = AOACFibreg, sex = Sex, age = Age, weight, country = Country, processed.poultry = ProcessedPoultryg, processed.redmeat = ProcessedRedMeatg, burgers = Burgersg, sausages = Sausagesg)

# Read in data years 9-11
data_9_11 <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr9-11a_personleveldietarydata_uk_20210831.tab)")
data_weights <- read.delim(r"(C:\Users\zrichards.ICR\OneDrive - The Institute of Cancer Research\PAR paper 3\UKDS_NationalNutritionandDietSurvey\TAB\UKDA-6533-tab\tab\ndns_rp_yr9-11a_indiv_20211020.tab)") |>
  mutate(weight = wti_Y911) |>
  dplyr::select(seriali, weight)
data_9_11 <- merge(data_9_11, data_weights, by = "seriali") |>
  dplyr::select(surveyyear = SurveyYear, totalredmeat, aoac_fibre = AOACFibreg, sex = Sex, age = AgeR, weight, country = Country, processed.poultry = ProcessedPoultryg, processed.redmeat = ProcessedRedMeatg, burgers = Burgersg, sausages = Sausagesg)

# RBind all data together
data_ndhns <- rbind(data_1_4, data_5_6) |>
  rbind(data_7_8) |>
  rbind(data_9_11) |>
  # Filter to adults
  filter(age >= 19) |>
  mutate(country = if_else(country %in% c("NI", "Northern Ireland"), "N. Ireland", country)) |>
  # Create new weights
  # group_by(sex, country) |>
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

fibrerisk <- data_ndhns |>
  filter(country == "England") |>
  group_by(surveyyear_cat, sex, country) |>
  mutate(fibre_rec = weight*(aoac_fibre < 30)) |>
  summarise(mean_weight = mean(weight), fibre_rec = 100*sum(fibre_rec)/sum(weight)) |>
  ggplot(aes(x= surveyyear_cat, y = fibre_rec, colour = country)) +
  geom_line() +
  geom_point() +
  ylim(0,100) +
  xlab("Year") +
  ylab("% Not Meeting Fibre Intake Guidelines") +
  ggtitle("% Not Meeting Fibre Intake Guidelines in England**") +
  labs(colour = "Country", caption = "**Guidelines are 30g of Fibre per day") +
  facet_wrap(~sex, scales = "fixed", dir = "v") +
  theme_minimal()
#print(fibrerisk)

#new code from reubens :
################ CATEGORIES ##############
fibre_cat <- data_ndhns |>
  filter(country == "England") |>
  mutate(fibrecat = cut(
    aoac_fibre, 
    breaks = seq(0, 30, by = 1), 
    labels = paste(seq(0, 29, by = 1), seq(1, 30, by = 1), sep = "-"), 
    include.lowest = TRUE
  )) |>
  mutate(fibrecat = factor(fibrecat, levels = paste(seq(0, 29, by = 1), seq(1, 30, by = 1), sep = "-"))) |>
  group_by(surveyyear_cat, sex) |>
  mutate(total_weight = sum(weight, na.rm = TRUE)) |>
  group_by(surveyyear_cat, sex, fibrecat) |>
  summarise(
    weighted_count = sum(weight, na.rm = TRUE), 
    total_weight = first(total_weight),
    proportion = weighted_count / total_weight,
    .groups = "drop"
  ) |>
  complete(surveyyear_cat, sex, fibrecat, fill = list(weighted_count = 0, proportion = 0, total_weight=0))
#Graph it 
fibrebar <- ggplot(fibre_cat, aes(x = fibrecat, y = proportion, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Fibre Daily Intake Category (g)") +
  ylab(" Count") +
  ggtitle(" Distribution of Fibre Daily Intake") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Sex") +
  facet_wrap(~surveyyear_cat, ncol = 1)
#print(fibrebar)
###################################

#cleaning the dataset 
fibremen <- fibre_cat %>%
  filter(sex == "Men") %>% 
  rename(yr = surveyyear_cat)
#fixing the year rounding 
fibremen$yr <- floor(fibremen$yr)
#fill in all of the missing years
Mnew2008 <- fibremen %>%
  filter(yr == 2009) %>%      
  mutate(yr = 2008) 
Mnew2010 <- fibremen %>%
  filter(yr == 2011) %>%      
  mutate(yr = 2010) 
Mnew2012 <- fibremen %>%
  filter(yr == 2013) %>%      
  mutate(yr = 2012) 
Mnew2014 <- fibremen %>%
  filter(yr == 2015) %>%      
  mutate(yr = 2014) 
Mnew2016 <- fibremen %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2016) 
Mnew2018 <- fibremen %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2018) 
fibremenTRY <- bind_rows(fibremen,Mnew2008, Mnew2010, Mnew2012, Mnew2014, Mnew2016, Mnew2018)
fibremen<-fibremenTRY

#WOMEN
#cleaning the data
fibrewomen <- fibre_cat%>%
  filter(sex == "Women") %>% 
  rename(yr = surveyyear_cat)
#fixing the rounding of the years 
fibrewomen$yr <- floor(fibrewomen$yr)
#filling in the data gaps 
Wnew2008 <- fibrewomen %>%
  filter(yr == 2009) %>%      
  mutate(yr = 2008) 
Wnew2010 <- fibrewomen %>%
  filter(yr == 2011) %>%      
  mutate(yr = 2010) 
Wnew2012 <- fibrewomen %>%
  filter(yr == 2013) %>%      
  mutate(yr = 2012) 
Wnew2014 <- fibrewomen %>%
  filter(yr == 2015) %>%      
  mutate(yr = 2014) 
Wnew2016 <- fibrewomen %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2016) 
Wnew2018 <- fibrewomen %>%
  filter(yr == 2017) %>%      
  mutate(yr = 2018) 
fibrewomenTRY <- bind_rows(fibrewomen,Wnew2008, Wnew2010, Wnew2012, Wnew2014, Wnew2016, Wnew2018)
fibrewomen <- fibrewomenTRY

####### calculating the RR #######
# Given RR for 10g of fibre
RR_10g <- rr %>% 
  filter(riskfactor == "fibre") %>%
  pull(colorectalRR)
# Calculate RR for for risk per g deficient 
ERRfibre <- log(1/RR_10g) /10
#calculating RR per category 
fibremen <- fibremen %>%
  mutate(
    midpoint = as.numeric(str_extract(fibrecat, "(?<=-)[0-9]+")) - .5,
    deficit = 30 - midpoint, 
    rr = deficit * ERRfibre, 
    calc = rr * proportion
  )

fibrewomen <- fibrewomen %>%
  mutate(
    midpoint = as.numeric(str_extract(fibrecat, "(?<=-)[0-9]+")) - .5,
    deficit = 30 - midpoint, 
    rr = deficit * ERRfibre, 
    calc = rr * proportion
  )


####### PAF calculations ########
#calculating the PAF for each year 
fibre_paf_men <- fibremen %>%
  group_by(yr) %>%            
  summarise(result = sum(calc, na.rm = TRUE) / (sum(calc, na.rm = TRUE) + 1), .groups = 'drop') %>%
  mutate(sex = "Men")
fibre_paf_women <- fibrewomen %>%
  group_by(yr) %>%            
  summarise(result = sum(calc, na.rm = TRUE) / (sum(calc, na.rm = TRUE) + 1), .groups = 'drop') %>%
  mutate(sex = "Women")

#cleaning the fibre data into one dataset 
cleanfibre <- rbind(fibre_paf_men, fibre_paf_women)
cleanfibre <- cleanfibre %>% 
  mutate(riskfactor = "fibre", Lung = NA, Breast = NA, Pancreas = NA, SCC = NA, AC = NA, Prostate = NA) %>%
  rename(Colorectal = result)
cleanfibre$combined <- paste(cleanfibre$riskfactor, cleanfibre$yr, sep = "")

fibrepaf <- ggplot(cleanfibre, aes(x = yr, y = Colorectal, colour = sex)) +
  geom_line() +
  geom_point() +
  ylim(0, .1) +
  scale_x_continuous(breaks = 2008:2018, limits = c(2008, 2018)) +
  xlab("Year") +
  ylab("PAF") +
  ggtitle("PAF Fibre Intake in England**") +
  labs(colour = "Country", caption = "**Guidelines are 30g of Fibre per day") +
  facet_wrap(~sex, scales = "fixed", dir = "v") +
  theme_minimal() 
#print(fibrepaf)
