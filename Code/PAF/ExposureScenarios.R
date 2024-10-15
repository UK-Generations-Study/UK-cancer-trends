# this is looking at potential future scenarios of exposure change 

#SCENARIO ONE: Smoking relationship - If there was 100% smoking cessation in 2019 with no initiation
#MEN
#pulling the relevant 2019 prevalence 
malesmokers2019 <- cleanedprev %>%
  filter(variable == "smoker_men", datayear == 2019, country == "England") %>%
  pull(prev)
maleformersmokers2019 <- cleanedprev %>%
  filter(variable == "formersmoker_men", datayear == 2019, country == "England") %>%
  pull(prev)
maxformersmoker = maleformersmokers2019+malesmokers2019
RRformersmoke <- rr %>% 
  filter(riskfactor == "men_formersmoker") %>% 
  pull(lungRR)
RRcurrentsmoke <- rr %>% 
  filter(riskfactor == "men_currentsmoker") %>% 
  pull(lungRR)
#creating the range of possible formal smokers and corresponding current smokers across the range in 2019 
formersmokingrange2019 <- seq(maleformersmokers2019, maxformersmoker, by = 0.01)
smokingprevalence2019 <- data.frame(count = seq(1,19))
smokingprevalence2019$former <- formersmokingrange2019
smokingprevalence2019$current <- (maxformersmoker - smokingprevalence2019$former)
#calculating the PAFs across the time 
smokingprevalence2019$paf <- ((smokingprevalence2019$current * (RRcurrentsmoke-1))+ (smokingprevalence2019$former * (RRformersmoke-1))) / (1 + (smokingprevalence2019$current * (RRcurrentsmoke-1))+ (smokingprevalence2019$former * (RRformersmoke-1)))
#WOMEN 
#pulling the relevant 2019 prevalence 
wsmokers2019 <- cleanedprev %>%
  filter(variable == "smoker_women", datayear == 2019, country == "England") %>%
  pull(prev)
wformersmokers2019 <- cleanedprev %>%
  filter(variable == "formersmoker_women", datayear == 2019, country == "England") %>%
  pull(prev)
wmaxformersmoker = wformersmokers2019+wsmokers2019
wRRformersmoke <- rr %>% 
  filter(riskfactor == "women_formersmoker") %>% 
  pull(lungRR)
wRRcurrentsmoke <- rr %>% 
  filter(riskfactor == "women_currentsmoker") %>% 
  pull(lungRR)
#creating the range of possible formal smokers and corresponding current smokers across the range in 2019 
wformersmokingrange2019 <- seq(wformersmokers2019, wmaxformersmoker, by = 0.01)
wsmokingprevalence2019 <- data.frame(count = seq(1,15))
wsmokingprevalence2019$former <- wformersmokingrange2019
wsmokingprevalence2019$current <- (wmaxformersmoker - wsmokingprevalence2019$former)
#calculating the PAFs across the time 
wsmokingprevalence2019$paf <- ((wsmokingprevalence2019$current * (wRRcurrentsmoke-1))+ (wsmokingprevalence2019$former * (wRRformersmoke-1))) / (1 + (wsmokingprevalence2019$current * (wRRcurrentsmoke-1))+ (wsmokingprevalence2019$former * (wRRformersmoke-1)))
#plot the scenario graph 
# mpafformersmoker<- ggplot(data.frame(prevalence = formersmokingrange2019, PAF = smokingprevalence2019$paf), aes(x = prevalence, y = PAF)) +
#   geom_line(color = "blue", size = 1) +
#   labs(title = "Men",
#        x = "Former Smoker Prevalence",
#        y = "PAF") +
#   ylim(0, 1) +
#   xlim(0.2,0.5) +
#   theme_minimal() 
# wpafformersmoker<- ggplot(data.frame(prevalence = wformersmokingrange2019, PAF = wsmokingprevalence2019$paf), aes(x = prevalence, y = PAF)) +
#   geom_line(color = "blue", size = 1) +
#   labs(title = "Women",
#        x = "Former Smoker Prevalence",
#        y = "PAF") +
#   ylim(0, 1) +
#   xlim(0.2,0.5) +
#   theme_minimal() 
# smokingcessation <- grid.arrange(mpafformersmoker, wpafformersmoker, ncol = 2, top = "Lung Cancer PAF if all Current Smokers became Former Smokers in England 2019")

#######################################################################################################################################################
#SCENARIO TWO- if all heavy and medium drinkers became light drinkers in 2019 
RRlightalc <- rr %>% 
  filter(riskfactor == "lowalc") %>% 
  pull(SCCRR)
RRmedalc <- rr %>% 
  filter(riskfactor == "medalc") %>% 
  pull(SCCRR)
RRheavyalc <- rr %>% 
  filter(riskfactor == "highalc") %>% 
  pull(SCCRR)
#MEN
Mlightalc <- cleanedprev %>%
  filter(variable == "lightalcohol_men", datayear == 2019, country == "England") %>%
  pull(prev)
Mmedalc <- cleanedprev %>%
  filter(variable == "medalcohol_men", datayear == 2019, country == "England") %>%
  pull(prev)
Mheavyalc <- cleanedprev %>%
  filter(variable == "heavyalcohol_men", datayear == 2019, country == "England") %>%
  pull(prev)
maxlightalc = Mlightalc + Mmedalc + Mheavyalc
#now calculating the columns 
mlightalc_range <- seq((Mlightalc-Mheavyalc), maxlightalc, by = 0.01)
malcprev <- data.frame(count = seq(1,36))
malcprev <- malcprev %>% 
  mutate(
    heavy = Mheavyalc - count/100, 
    heavy = if_else(heavy < 0, 0, heavy),
    med_try = Mmedalc + (Mheavyalc - heavy),
    medium = if_else(heavy > 0, med_try, med_try - ((count - 5)/100)), 
    medium = if_else(medium < 0, 0, medium),
    light = if_else(heavy == 0, maxlightalc - medium, Mlightalc)
  ) %>%
  dplyr::select(-med_try)
#calculating the PAFs across the time 
malcprev$paf <- ((malcprev$light * (RRlightalc-1))+ (malcprev$medium * (RRmedalc-1)) + (malcprev$heavy * (RRheavyalc-1))) / (1 + ((malcprev$light * (RRlightalc-1))+ (malcprev$medium * (RRmedalc-1)) + (malcprev$heavy * (RRheavyalc-1))))
#WOMEN
Wlightalc <- cleanedprev %>%
  filter(variable == "lightalcohol_women", datayear == 2019, country == "England") %>%
  pull(prev)
Wmedalc <- cleanedprev %>%
  filter(variable == "medalcohol_women", datayear == 2019, country == "England") %>%
  pull(prev)
Wheavyalc <- cleanedprev %>%
  filter(variable == "heavyalcohol_women", datayear == 2019, country == "England") %>%
  pull(prev)
Wmaxlightalc = Wlightalc + Wmedalc + Wheavyalc
#now calculating the columns 
wlightalc_range <- seq((Wlightalc-Wheavyalc-.01), Wmaxlightalc, by = 0.01)
walcprev <- data.frame(count = seq(1,18))
walcprev <- walcprev %>% 
  mutate(
    heavy = Wheavyalc - (count-1)/100, 
    heavy = if_else(heavy < 0, 0, heavy),
    med_try = Wmedalc + (Wheavyalc - heavy),
    medium = if_else(heavy > 0, med_try, med_try - ((count - 2)/100)), 
    medium = if_else(medium < 0, 0, medium),
    light = if_else(heavy == 0, Wmaxlightalc - medium, Wlightalc)
  ) %>% 
  dplyr::select(-med_try)
#calculating the PAFs across the time 
walcprev$paf <- ((walcprev$light * (RRlightalc-1))+ (walcprev$medium * (RRmedalc-1)) + (walcprev$heavy * (RRheavyalc-1))) / (1 + ((walcprev$light * (RRlightalc-1))+ (walcprev$medium * (RRmedalc-1)) + (walcprev$heavy * (RRheavyalc-1))))
#visualize
# mpafalc<- ggplot(data.frame(prevalence = mlightalc_range, PAF = malcprev$paf), aes(x = prevalence, y = PAF)) +
#   geom_line(color = "blue", size = 1) +
#   labs(title = "Men",
#        x = "Light Drinking Prevalence",
#        y = "PAF") +
#   ylim(0, .5) +
#   xlim(.45,.9) +
#   theme_minimal() 
# wpafalc<- ggplot(data.frame(prevalence = wlightalc_range, PAF = walcprev$paf), aes(x = prevalence, y = PAF)) +
#   geom_line(color = "blue", size = 1) +
#   labs(title = "Women",
#        x = "Light Drinking Prevalence",
#        y = "PAF") +
#   ylim(0, .5) +
#   xlim(.45,.9) +
#   theme_minimal()
# alcohol_alllight <- grid.arrange(mpafalc, wpafalc, ncol=2, top= "OSCC PAF if all Medium and Heavy Drinkers became Light Smokers in England 2019")

#######################################################################################################################################################
#SCENARIO THREE: Obesity to overweight and OAC

