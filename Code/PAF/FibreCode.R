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


# Fibre
# figure3b_fibre <- data_ndhns |>
#   group_by(surveyyear_cat, sex, country) |>
#   mutate(fibre_rec = weight*(aoac_fibre  >= 30)) |>
#   summarise(mean_weight = mean(weight), fibre_rec = 100*sum(fibre_rec)/sum(weight)) |>
#   ggplot(aes(x= surveyyear_cat, y = fibre_rec, colour = country)) +
#   geom_line() +
#   geom_point() +
#   xlab("Year") +
#   ylab("% Meeting Fibre Intake Guidelines") +
#   ggtitle("% Meeting Fibre Intake Guidelines in UK**") +
#   labs(colour = "Country", caption = "**Guidelines are 30g of Fibre per day") +
#   facet_wrap(~sex, scales = "fixed", dir = "v")
# print(figure3b_fibre)

#new code from reubens 
#it is calculating the prevaelnce of eating enough fibre 
fibre_summary <- data_ndhns |>
  group_by(surveyyear_cat, sex, country) |>
  mutate(fibre_rec = weight * (aoac_fibre >= 30)) |>
  summarise(
    mean_weight = mean(weight),
    fibre_rec = 100 * sum(fibre_rec) / sum(weight),
    .groups = "drop"
  )

fibre_england <- fibre_summary %>%
  filter(country == "England")

# Plot it
figure3b_fibre <- ggplot(fibre_england, aes(x = surveyyear_cat, y = fibre_rec, colour = country)) +
  geom_line() +
  geom_point() +
  xlab("Year") +
  ylab("% Meeting Fibre Intake Guidelines") +
  ggtitle("% Meeting Fibre Intake Guidelines in England**") +
  labs(colour = "Country", caption = "**Guidelines are 30g of Fibre per day") +
  facet_wrap(~sex, scales = "fixed", dir = "v")

# Step 4: Print the plot
print(figure3b_fibre)

#MEN
#cleaning the dataset 
fibremen <- fibre_england %>%
  filter(sex == "Men") %>% 
  rename(yr = surveyyear_cat) %>%
  mutate(prev = (100-fibre_rec)/100) %>%
    dplyr::select (- mean_weight, -fibre_rec)

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
fibrewomen <- fibre_england%>%
  filter(sex == "Women") %>% 
  rename(yr = surveyyear_cat) %>%
  mutate(prev = (100-fibre_rec)/100) %>% #this is to create the exposed to the risk factor group (not eating enough fibre per the guidelines)
    dplyr::select (- mean_weight, -fibre_rec)

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

#calculating the RR using a multiplicative model because given is a dose - response relationship per 10/g of fibre a day
# Given RR for 10g of fibre
RR_10g <- rr %>% 
  filter(riskfactor == "fibre") %>%
  pull(colorectalRR)
# Number of times 10g fits into 30g
n <- 3
# Calculate RR for 30g
RR_30g <- RR_10g^n
RR_30g #0.804357
#now to calculate the the RR as a risk factor for the absence of fibre 
RRfibre <- 1/RR_30g # RR = 1.24
ERRfibre <- log(1/RR_30g) #excess relative risk = 0.217712

#calculating the PAF for each year 
fibremen$paf <- (ERRfibre*fibremen$prev)/(ERRfibre*fibremen$prev + 1)
fibrewomen$paf<- (ERRfibre*fibrewomen$prev)/(ERRfibre*fibrewomen$prev + 1)

#cleaning the fibre data into one dataset 
#fibremen$yr <- floor(fibremen$yr)
#fibrewomen$yr <- floor(fibrewomen$yr)
cleanfibre <- rbind(fibremen, fibrewomen)
cleanfibre <- cleanfibre %>% 
  mutate(riskfactor = "fibre")
cleanfibre$combined <- paste(cleanfibre$riskfactor, cleanfibre$yr, sep = "")

cleanfibre <- cleanfibre %>% 
  mutate(Lung = NA, Breast = NA, Pancreas = NA, SCC = NA, AC = NA, Prostate = NA) %>% 
  rename(Colorectal = paf) %>% 
    dplyr::select(combined, yr, riskfactor, sex, Lung, Colorectal, Breast, Pancreas, SCC, AC, Prostate)

