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
formersmokingrange2019 <- seq(maleformersmokers2019, maxformersmoker, by = 0.01)
smokingprevalence2019 <- data.frame(count = seq(1,19))
smokingprevalence2019$former <- formersmokingrange2019
smokingprevalence2019$current <- (maxformersmoker - smokingprevalence2019$former)
#LUNG
RRformersmoke <- rr %>% 
  filter(riskfactor == "men_formersmoker") %>% 
  pull(lungRR)
RRcurrentsmoke <- rr %>% 
  filter(riskfactor == "men_currentsmoker") %>% 
  pull(lungRR)
smokingprevalence2019$Lungpaf <- ((smokingprevalence2019$current * (RRcurrentsmoke-1))+ (smokingprevalence2019$former * (RRformersmoke-1))) / (1 + (smokingprevalence2019$current * (RRcurrentsmoke-1))+ (smokingprevalence2019$former * (RRformersmoke-1)))
#COLORECTAL
RRfs_colorectal <- rr %>% 
  filter(riskfactor == "men_formersmoker") %>% 
  pull(colorectalRR)
RRcs_colorectal <- rr %>% 
  filter(riskfactor == "men_currentsmoker") %>% 
  pull(colorectalRR)
smokingprevalence2019$Colorectalpaf <- ((smokingprevalence2019$current * (RRcs_colorectal-1))+ (smokingprevalence2019$former * (RRfs_colorectal-1))) / (1 + (smokingprevalence2019$current * (RRcs_colorectal-1))+ (smokingprevalence2019$former * (RRfs_colorectal-1)))
#PANCREAS
RRfs_pancreas <- rr %>% 
  filter(riskfactor == "men_formersmoker") %>% 
  pull(pancreasRR)
RRcs_pancreas <- rr %>% 
  filter(riskfactor == "men_currentsmoker") %>% 
  pull(pancreasRR)
smokingprevalence2019$Pancreaspaf <- ((smokingprevalence2019$current * (RRcs_pancreas-1))+ (smokingprevalence2019$former * (RRfs_pancreas-1))) / (1 + (smokingprevalence2019$current * (RRcs_pancreas-1))+ (smokingprevalence2019$former * (RRfs_pancreas-1)))
#SCC
RRfs_scc <- rr %>% 
  filter(riskfactor == "men_formersmoker") %>% 
  pull(SCCRR)
RRcs_scc <- rr %>% 
  filter(riskfactor == "men_currentsmoker") %>% 
  pull(SCCRR)
smokingprevalence2019$SCCpaf <- ((smokingprevalence2019$current * (RRcs_scc-1))+ (smokingprevalence2019$former * (RRfs_scc-1))) / (1 + (smokingprevalence2019$current * (RRcs_scc-1))+ (smokingprevalence2019$former * (RRfs_scc-1)))
#AC
RRfs_Ac <- rr %>% 
  filter(riskfactor == "men_formersmoker") %>% 
  pull(ACRR)
RRcs_Ac <- rr %>% 
  filter(riskfactor == "men_currentsmoker") %>% 
  pull(ACRR)
smokingprevalence2019$ACpaf <- ((smokingprevalence2019$current * (RRcs_Ac-1))+ (smokingprevalence2019$former * (RRfs_Ac-1))) / (1 + (smokingprevalence2019$current * (RRcs_Ac-1))+ (smokingprevalence2019$former * (RRfs_Ac-1)))


#WOMEN 
#pulling the relevant 2019 prevalence 
wsmokers2019 <- cleanedprev %>%
  filter(variable == "smoker_women", datayear == 2019, country == "England") %>%
  pull(prev)
wformersmokers2019 <- cleanedprev %>%
  filter(variable == "formersmoker_women", datayear == 2019, country == "England") %>%
  pull(prev)
wmaxformersmoker = wformersmokers2019+wsmokers2019
wformersmokingrange2019 <- seq(wformersmokers2019, wmaxformersmoker, by = 0.01)
wsmokingprevalence2019 <- data.frame(count = seq(1,15))
wsmokingprevalence2019$former <- wformersmokingrange2019
wsmokingprevalence2019$current <- (wmaxformersmoker - wsmokingprevalence2019$former)
#LUNG
wRRformersmoke <- rr %>% 
  filter(riskfactor == "women_formersmoker") %>% 
  pull(lungRR)
wRRcurrentsmoke <- rr %>% 
  filter(riskfactor == "women_currentsmoker") %>% 
  pull(lungRR)
wsmokingprevalence2019$Lungpaf <- ((wsmokingprevalence2019$current * (wRRcurrentsmoke-1))+ (wsmokingprevalence2019$former * (wRRformersmoke-1))) / (1 + (wsmokingprevalence2019$current * (wRRcurrentsmoke-1))+ (wsmokingprevalence2019$former * (wRRformersmoke-1)))
#COLORECTAL
wRRfs_colorectal <- rr %>% 
  filter(riskfactor == "women_formersmoker") %>% 
  pull(colorectalRR)
wRRcs_colorectal <- rr %>% 
  filter(riskfactor == "women_currentsmoker") %>% 
  pull(colorectalRR)
wsmokingprevalence2019$Colorectalpaf <- ((wsmokingprevalence2019$current * (wRRcs_colorectal-1))+ (wsmokingprevalence2019$former * (wRRfs_colorectal-1))) / (1 + (wsmokingprevalence2019$current * (wRRcs_colorectal-1))+ (wsmokingprevalence2019$former * (wRRfs_colorectal-1)))
#PANCREAS
wRRfs_pancreas <- rr %>% 
  filter(riskfactor == "women_formersmoker") %>% 
  pull(pancreasRR)
wRRcs_pancreas <- rr %>% 
  filter(riskfactor == "women_currentsmoker") %>% 
  pull(pancreasRR)
wsmokingprevalence2019$Pancreaspaf <- ((wsmokingprevalence2019$current * (wRRcs_pancreas-1))+ (wsmokingprevalence2019$former * (wRRfs_pancreas-1))) / (1 + (wsmokingprevalence2019$current * (wRRcs_pancreas-1))+ (wsmokingprevalence2019$former * (wRRfs_pancreas-1)))
#SCC
wRRfs_SCC <- rr %>% 
  filter(riskfactor == "women_formersmoker") %>% 
  pull(SCCRR)
wRRcs_SCC <- rr %>% 
  filter(riskfactor == "women_currentsmoker") %>% 
  pull(SCCRR)
wsmokingprevalence2019$SCCpaf <- ((wsmokingprevalence2019$current * (wRRcs_SCC-1))+ (wsmokingprevalence2019$former * (wRRfs_SCC-1))) / (1 + (wsmokingprevalence2019$current * (wRRcs_SCC-1))+ (wsmokingprevalence2019$former * (wRRfs_SCC-1)))
#AC
wRRfs_AC <- rr %>% 
  filter(riskfactor == "women_formersmoker") %>% 
  pull(ACRR)
wRRcs_AC <- rr %>% 
  filter(riskfactor == "women_currentsmoker") %>% 
  pull(ACRR)
wsmokingprevalence2019$ACpaf <- ((wsmokingprevalence2019$current * (wRRcs_AC-1))+ (wsmokingprevalence2019$former * (wRRfs_AC-1))) / (1 + (wsmokingprevalence2019$current * (wRRcs_AC-1))+ (wsmokingprevalence2019$former * (wRRfs_AC-1)))


#formatting the data 
mensmokingcessation <- smokingprevalence2019 %>%
  pivot_longer(cols = c(Lungpaf, Colorectalpaf, Pancreaspaf, SCCpaf, ACpaf),
               names_to = "cancersite", 
               values_to = "PAF") %>% 
  mutate(cancersite = str_replace(cancersite, "paf$", ""), 
         sex = "men", 
         scenario = "Actual", 
         scenario = if_else(count == 19, "Imagined", scenario), 
         PAF = PAF*100) %>% 
  filter(count %in% c(1, 19))

womensmokingcessation <- wsmokingprevalence2019 %>%
  pivot_longer(cols = c(Lungpaf, Colorectalpaf, Pancreaspaf, SCCpaf, ACpaf),
               names_to = "cancersite", 
               values_to = "PAF") %>% 
  mutate(cancersite = str_replace(cancersite, "paf$", ""), 
         sex = "women", 
         scenario = "Actual", 
         scenario = if_else(count == 15, "Imagined", scenario), 
         PAF = PAF*100) %>% 
  filter(count %in% c(1, 15))

#plot the barplots
# mensmokingcessation_bp <- ggplot(mensmokingcessation, aes(x = cancersite, y = PAF, fill = factor(scenario))) +
#   geom_bar(stat = "identity", position = "dodge") +  
#   geom_text(aes(label = round(PAF, 2)),              
#             position = position_dodge(width = 0.9),  
#             vjust = -0.3,                           
#             size = 3) +
#   labs(title = "Men",
#        x = "Cancer Site",
#        y = "PAF",
#        fill = "Scenarios") +
#   scale_fill_manual(values = c("Actual" = "#6b9080", "Imagined" = "#cce3de")) +
#   theme_minimal()
# #plot(mensmokingcessation_bp)
# 
# womensmokingcessation_bp <- ggplot(womensmokingcessation, aes(x = cancersite, y = PAF, fill = factor(scenario))) +
#   geom_bar(stat = "identity", position = "dodge") +  
#   geom_text(aes(label = round(PAF, 2)),              
#             position = position_dodge(width = 0.9),  
#             vjust = -0.3,                           
#             size = 3) +
#   labs(title = "Women",
#        x = "Cancer Site",
#        y = "PAF",
#        fill = "Scenarios") +
#   scale_fill_manual(values = c("Actual" = "#6b9080", "Imagined" = "#cce3de")) +
#   theme_minimal()
# #plot(womensmokingcessation_bp)
# smokingcessation <- grid.arrange(mensmokingcessation_bp,womensmokingcessation_bp, ncol = 2, top = "PAFs 2019: Actual vs. Smoking Cessation Scenario" )

#plot the scenario graph 
# mpafformersmoker<- ggplot(data.frame(prevalence = formersmokingrange2019, PAF = smokingprevalence2019$Lungpaf), aes(x = prevalence, y = PAF)) +
#   geom_line(color = "blue", size = 1) +
#   labs(title = "Men",
#        x = "Former Smoker Prevalence",
#        y = "PAF") +
#   ylim(0, 1) +
#   xlim(0.2,0.5) +
#   theme_minimal()
# wpafformersmoker<- ggplot(data.frame(prevalence = wformersmokingrange2019, PAF = wsmokingprevalence2019$Lungpaf), aes(x = prevalence, y = PAF)) +
#   geom_line(color = "blue", size = 1) +
#   labs(title = "Women",
#        x = "Former Smoker Prevalence",
#        y = "PAF") +
#   ylim(0, 1) +
#   xlim(0.2,0.5) +
#   theme_minimal()
#smokingcessation <- grid.arrange(mpafformersmoker, wpafformersmoker, ncol = 2, top = "Lung Cancer PAF if all Current Smokers became Former Smokers in England 2019")

#######################################################################################################################################################
#SCENARIO TWO- if all heavy and medium drinkers became light drinkers in 2019 
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
#SCC 
RRlightalc_SCC <- rr %>% 
  filter(riskfactor == "lowalc") %>% 
  pull(SCCRR)
RRmedalc_SCC <- rr %>% 
  filter(riskfactor == "medalc") %>% 
  pull(SCCRR)
RRheavyalc_SCC <- rr %>% 
  filter(riskfactor == "highalc") %>% 
  pull(SCCRR)
#calculating the PAFs across the time 
malcprev$SCCpaf <- ((malcprev$light * (RRlightalc_SCC-1))+ (malcprev$medium * (RRmedalc_SCC-1)) + (malcprev$heavy * (RRheavyalc_SCC-1))) / (1 + ((malcprev$light * (RRlightalc_SCC-1))+ (malcprev$medium * (RRmedalc_SCC-1)) + (malcprev$heavy * (RRheavyalc_SCC-1))))
#Colorectal
RRlightalc_colorectal <- rr %>% 
  filter(riskfactor == "lowalc") %>% 
  pull(colorectalRR)
RRmedalc_colorectal <- rr %>% 
  filter(riskfactor == "medalc") %>% 
  pull(colorectalRR)
RRheavyalc_colorectal <- rr %>% 
  filter(riskfactor == "highalc") %>% 
  pull(colorectalRR)
#calculating the PAFs across the time 
malcprev$Colorectalpaf <- ((malcprev$light * (RRlightalc_colorectal-1))+ (malcprev$medium * (RRmedalc_colorectal-1)) + (malcprev$heavy * (RRheavyalc_colorectal-1))) / (1 + ((malcprev$light * (RRlightalc_colorectal-1))+ (malcprev$medium * (RRmedalc_colorectal-1)) + (malcprev$heavy * (RRheavyalc_colorectal-1))))
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
#SCC
#calculating the PAFs across the time 
walcprev$SCCpaf <- ((walcprev$light * (RRlightalc_SCC-1))+ (walcprev$medium * (RRmedalc_SCC-1)) + (walcprev$heavy * (RRheavyalc_SCC-1))) / (1 + ((walcprev$light * (RRlightalc_SCC-1))+ (walcprev$medium * (RRmedalc_SCC-1)) + (walcprev$heavy * (RRheavyalc_SCC-1))))
#Colorectal 
walcprev$Colorectalpaf <- ((walcprev$light * (RRlightalc_colorectal-1))+ (walcprev$medium * (RRmedalc_colorectal-1)) + (walcprev$heavy * (RRheavyalc_colorectal-1))) / (1 + ((walcprev$light * (RRlightalc_colorectal-1))+ (walcprev$medium * (RRmedalc_colorectal-1)) + (walcprev$heavy * (RRheavyalc_colorectal-1))))
#Breast
RRlightalc_breast <- rr %>% 
  filter(riskfactor == "lowalc") %>% 
  pull(breastRR)
RRmedalc_breast <- rr %>% 
  filter(riskfactor == "medalc") %>% 
  pull(breastRR)
RRheavyalc_breast <- rr %>% 
  filter(riskfactor == "highalc") %>% 
  pull(breastRR)
#calculating the PAFs across the time 
walcprev$Breastpaf <- ((walcprev$light * (RRlightalc_breast-1))+ (walcprev$medium * (RRmedalc_breast-1)) + (walcprev$heavy * (RRheavyalc_breast-1))) / (1 + ((walcprev$light * (RRlightalc_breast-1))+ (walcprev$medium * (RRmedalc_breast-1)) + (walcprev$heavy * (RRheavyalc_breast-1))))

#formatting the data 
menalclimit <- malcprev %>%
  pivot_longer(cols = c(SCCpaf, Colorectalpaf),
               names_to = "cancersite", 
               values_to = "PAF") %>% 
  mutate(cancersite = str_replace(cancersite, "paf$", ""), 
         sex = "men", 
         scenario = "Actual", 
         scenario = if_else(count == 36, "Imagined", scenario), 
         PAF = if_else(PAF <0, 0, PAF),
         PAF = PAF*100) %>% 
  filter(count %in% c(1, 36))

womenalclimit <- walcprev %>%
  pivot_longer(cols = c(Colorectalpaf, Breastpaf, SCCpaf),
               names_to = "cancersite", 
               values_to = "PAF") %>% 
  mutate(cancersite = str_replace(cancersite, "paf$", ""), 
         sex = "women", 
         scenario = "Actual", 
         scenario = if_else(count == 18, "Imagined", scenario), 
         PAF = if_else(PAF <0, 0, PAF),
         PAF = PAF*100) %>% 
  filter(count %in% c(1, 18))

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
# alcohol_alllight <- grid.arrange(mpafalc, wpafalc, ncol=2, top= "OSCC PAF if all Medium and Heavy Drinkers became Light Drinkers in England 2019")


#plot the barplots
# menalclimit_bp <- ggplot(menalclimit, aes(x = cancersite, y = PAF, fill = factor(scenario))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label = round(PAF, 2)),
#             position = position_dodge(width = 0.9),
#             vjust = -0.3,
#             size = 3) +
#   labs(title = "Men",
#        x = "Cancer Site",
#        y = "PAF",
#        fill = "Scenarios") +
#   scale_fill_manual(values = c("Actual" = "#6b9080", "Imagined" = "#cce3de")) +
#   theme_minimal()
# #plot(mensmokingcessation_bp)
# 
# womenalclimit_bp <- ggplot(womenalclimit, aes(x = cancersite, y = PAF, fill = factor(scenario))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label = round(PAF, 2)),
#             position = position_dodge(width = 0.9),
#             vjust = -0.3,
#             size = 3) +
#   labs(title = "Women",
#        x = "Cancer Site",
#        y = "PAF",
#        fill = "Scenarios") +
#   scale_fill_manual(values = c("Actual" = "#6b9080", "Imagined" = "#cce3de")) +
#   theme_minimal()
# #plot(womensmokingcessation_bp)
# alclimit <- grid.arrange(menalclimit_bp, womenalclimit_bp, ncol = 2, top = "PAFs 2019: Actual vs. Alcohol Limiting Scenario" )

#######################################################################################################################################################
#SCENARIO THREE: Obesity to overweight and OAC
#there is no obesity in the population meaning that the population BMI maximum is 30
#MEN
Mobese <- cleanedprev %>%
  filter(variable == "obese_men", datayear == 2019, country == "England") %>%
  pull(prev)
Moverweight <- cleanedprev %>%
  filter(variable == "overweight_men", datayear == 2019, country == "England") %>%
  pull(prev)
mmaxbmirisk = Mobese + Moverweight 
moverweightrange2019 <- seq(Moverweight, mmaxbmirisk, by = 0.01)
mbmiprev2019 <- data.frame(count = seq(1,27))
mbmiprev2019$overweight <- moverweightrange2019
mbmiprev2019$obese <- (mmaxbmirisk - mbmiprev2019$overweight)
#AC
RRobese_AC <- rr %>% 
  filter(riskfactor == "obese") %>% 
  pull(ACRR)
RRoverweight_AC <- rr %>% 
  filter(riskfactor == "overweight") %>% 
  pull(ACRR)
#calculating the PAFs across the time 
mbmiprev2019$ACpaf <- ((mbmiprev2019$obese * (RRobese_AC-1))+ (mbmiprev2019$overweight * (RRoverweight_AC-1))) / (1 + (mbmiprev2019$obese * (RRobese_AC-1))+ (mbmiprev2019$overweight * (RRoverweight_AC-1)))
#Colorectal
RRobese_colorectal <- rr %>% 
  filter(riskfactor == "obese") %>% 
  pull(colorectalRR)
RRoverweight_colorectal <- rr %>% 
  filter(riskfactor == "overweight") %>% 
  pull(colorectalRR)
#calculating the PAFs across the time 
mbmiprev2019$Colorectalpaf <- ((mbmiprev2019$obese * (RRobese_colorectal-1))+ (mbmiprev2019$overweight * (RRoverweight_colorectal-1))) / (1 + (mbmiprev2019$obese * (RRobese_colorectal-1))+ (mbmiprev2019$overweight * (RRoverweight_colorectal-1)))
#Pancreas
RRobese_pancreas <- rr %>% 
  filter(riskfactor == "obese") %>% 
  pull(pancreasRR)
RRoverweight_pancreas <- rr %>% 
  filter(riskfactor == "overweight") %>% 
  pull(pancreasRR)
#calculating the PAFs across the time 
mbmiprev2019$Pancreaspaf <- ((mbmiprev2019$obese * (RRobese_pancreas-1))+ (mbmiprev2019$overweight * (RRoverweight_pancreas-1))) / (1 + (mbmiprev2019$obese * (RRobese_pancreas-1))+ (mbmiprev2019$overweight * (RRoverweight_pancreas-1)))

#WOMEN
Wobese <- cleanedprev %>%
  filter(variable == "obese_women", datayear == 2019, country == "England") %>%
  pull(prev)
Woverweight <- cleanedprev %>%
  filter(variable == "overweight_women", datayear == 2019, country == "England") %>%
  pull(prev)
wmaxbmirisk = Wobese + Woverweight 
#creating the range of possible bmi 
woverweightrange2019 <- seq(Woverweight, wmaxbmirisk, by = 0.01)
wbmiprev2019 <- data.frame(count = seq(1,30))
wbmiprev2019$overweight <- woverweightrange2019
wbmiprev2019$obese <- (wmaxbmirisk - wbmiprev2019$overweight)
#AC
wbmiprev2019$ACpaf <- ((wbmiprev2019$obese * (RRobese_AC-1))+ (wbmiprev2019$overweight * (RRoverweight_AC-1))) / (1 + (wbmiprev2019$obese * (RRobese_AC-1))+ (wbmiprev2019$overweight * (RRoverweight_AC-1)))
#Colorectal
wbmiprev2019$Colorectalpaf <- ((wbmiprev2019$obese * (RRobese_colorectal-1))+ (wbmiprev2019$overweight * (RRoverweight_colorectal-1))) / (1 + (wbmiprev2019$obese * (RRobese_colorectal-1))+ (wbmiprev2019$overweight * (RRoverweight_colorectal-1)))
#Pancreas
wbmiprev2019$Pancreaspaf <- ((wbmiprev2019$obese * (RRobese_pancreas-1))+ (wbmiprev2019$overweight * (RRoverweight_pancreas-1))) / (1 + (wbmiprev2019$obese * (RRobese_pancreas-1))+ (wbmiprev2019$overweight * (RRoverweight_pancreas-1)))
#Breast
RRobese_breast<- rr %>% 
  filter(riskfactor == "obese") %>% 
  pull(breastRR)
RRoverweight_breast <- rr %>% 
  filter(riskfactor == "overweight") %>% 
  pull(breastRR)
wbmiprev2019$Breastpaf <- ((wbmiprev2019$obese * (RRobese_breast-1))+ (wbmiprev2019$overweight * (RRoverweight_breast-1))) / (1 + (wbmiprev2019$obese * (RRobese_breast-1))+ (wbmiprev2019$overweight * (RRoverweight_breast-1)))

#formatting the data 
menbmilimit <- mbmiprev2019 %>%
  pivot_longer(cols = c(ACpaf, Colorectalpaf, Pancreaspaf),
               names_to = "cancersite", 
               values_to = "PAF") %>% 
  mutate(cancersite = str_replace(cancersite, "paf$", ""), 
         sex = "men", 
         scenario = "Actual", 
         scenario = if_else(count == 27, "Imagined", scenario), 
         PAF = if_else(PAF <0, 0, PAF),
         PAF = PAF*100) %>% 
  filter(count %in% c(1, 27))

womenbmilimit <- wbmiprev2019 %>%
  pivot_longer(cols = c(Colorectalpaf, Breastpaf, ACpaf, Pancreaspaf),
               names_to = "cancersite", 
               values_to = "PAF") %>% 
  mutate(cancersite = str_replace(cancersite, "paf$", ""), 
         sex = "women", 
         scenario = "Actual", 
         scenario = if_else(count == 30, "Imagined", scenario), 
         PAF = if_else(PAF <0, 0, PAF),
         PAF = PAF*100) %>% 
  filter(count %in% c(1, 30))

#visualize
 # mpaf_bmi<- ggplot(data.frame(prevalence = moverweightrange2019, PAF = mbmiprev2019$paf), aes(x = prevalence, y = PAF)) +
 #   geom_line(color = "blue", size = 1) +
 #   labs(title = "Men",
 #        x = "Overweight Prevalence",
 #        y = "PAF") +
 #   ylim(0, .5) +
 #   xlim(0.3,.7) +
 #   theme_minimal()
 # wpaf_bmi<- ggplot(data.frame(prevalence = woverweightrange2019, PAF = wbmiprev2019$paf), aes(x = prevalence, y = PAF)) +
 #   geom_line(color = "blue", size = 1) +
 #   labs(title = "Women",
 #        x = "Overweight Prevalence",
 #        y = "PAF") +
 #   ylim(0, .5) +
 #   xlim(0.3,.7) +
 #   theme_minimal()
# bmi_paf <- grid.arrange(mpaf_bmi,wpaf_bmi, ncol = 2, top = "OAC PAF if all People with Obesity became Overweight in England 2019" )

#plot the barplots
# menbmilimit_bp <- ggplot(menbmilimit, aes(x = cancersite, y = PAF, fill = factor(scenario))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label = round(PAF, 2)),
#             position = position_dodge(width = 0.9),
#             vjust = -0.3,
#             size = 3) +
#   labs(title = "Men",
#        x = "Cancer Site",
#        y = "PAF",
#        fill = "Scenarios") +
#   scale_fill_manual(values = c("Actual" = "#6b9080", "Imagined" = "#cce3de")) +
#   theme_minimal()
# #plot(menbmilimit_bp)
# 
# womenbmilimit_bp <- ggplot(womenbmilimit, aes(x = cancersite, y = PAF, fill = factor(scenario))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label = round(PAF, 2)),
#             position = position_dodge(width = 0.9),
#             vjust = -0.3,
#             size = 3) +
#   labs(title = "Women",
#        x = "Cancer Site",
#        y = "PAF",
#        fill = "Scenarios") +
#   scale_fill_manual(values = c("Actual" = "#6b9080", "Imagined" = "#cce3de")) +
#   theme_minimal()
# #plot(womenbmilimit_bp)
# bmilimit <- grid.arrange(menbmilimit_bp, womenbmilimit_bp, ncol = 2, top = "PAFs 2019: Actual vs. BMI Limiting Scenario" )
