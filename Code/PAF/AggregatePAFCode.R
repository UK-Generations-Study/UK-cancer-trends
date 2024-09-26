library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)

#cleaning the total PAF dataset 
PAF_all <- PAF_all %>%
  mutate(Breast = if_else(sex == "Men", NA_real_, Breast))

#total paf calculation:
calculate_cumulative_paf <- function(paf_values) {
  cumulative_paf <- 0
  remaining_whole <- 1
  
  for (paf in paf_values) {
    if (!is.na(paf)) {
      cumulative_paf <- cumulative_paf + paf * remaining_whole
      remaining_whole <- remaining_whole * (1 - paf)
    }
  }
  
  return(cumulative_paf)
}
#total PAF calcultaions 
aggregate_paf_men <- data.frame(
  cancersite = c("Lung", "Pancreas", "AC", "SCC", "Colorectal")
)

aggregate_paf_men[paste0(2005:2019)] <- NA

#fill in the data frame 
for (year in 2005:2019) {
  # Filter PAF_all for the given year and sex
  yearsub <- PAF_all %>%
    filter(yr == year, sex == "Men") %>%
   dplyr:: select(Lung, Colorectal, AC, SCC, Breast, Pancreas)
  
  cumulative_lung <- calculate_cumulative_paf(yearsub$Lung)
  cumulative_colorectal <- calculate_cumulative_paf(yearsub$Colorectal)
  cumulative_ac <- calculate_cumulative_paf(yearsub$AC)
  cumulative_scc <- calculate_cumulative_paf(yearsub$SCC)
  cumulative_breast <- calculate_cumulative_paf(yearsub$Breast)
  cumulative_pancreas <- calculate_cumulative_paf(yearsub$Pancreas)
  
  # Store the cumulative PAF in aggregate_paf
  aggregate_paf_men[aggregate_paf_men$cancersite == "Lung", as.character(year)] <- cumulative_lung
  aggregate_paf_men[aggregate_paf_men$cancersite == "Colorectal", as.character(year)] <- cumulative_colorectal
  aggregate_paf_men[aggregate_paf_men$cancersite == "AC", as.character(year)] <- cumulative_ac
  aggregate_paf_men[aggregate_paf_men$cancersite == "SCC", as.character(year)] <- cumulative_scc
  aggregate_paf_men[aggregate_paf_men$cancersite == "Breast", as.character(year)] <- cumulative_breast
  aggregate_paf_men[aggregate_paf_men$cancersite == "Pancreas", as.character(year)] <- cumulative_pancreas
}

print(aggregate_paf_men)
#################################################################################
#for women
aggregate_paf_women <- data.frame(
  cancersite = c("Lung", "Pancreas", "AC", "SCC", "Breast", "Colorectal")
)

aggregate_paf_women[paste0(2005:2019)] <- NA

#fill in the data frame 
for (year in 2005:2019) {
  # Filter PAF_all for the given year and sex
  yearsub <- PAF_all %>%
    filter(yr == year, sex == "Women") %>%
    dplyr::select(Lung, Colorectal, AC, SCC, Breast, Pancreas)
  
  cumulative_lung <- calculate_cumulative_paf(yearsub$Lung)
  cumulative_colorectal <- calculate_cumulative_paf(yearsub$Colorectal)
  cumulative_ac <- calculate_cumulative_paf(yearsub$AC)
  cumulative_scc <- calculate_cumulative_paf(yearsub$SCC)
  cumulative_breast <- calculate_cumulative_paf(yearsub$Breast)
  cumulative_pancreas <- calculate_cumulative_paf(yearsub$Pancreas)
  
  # Store the cumulative PAF in aggregate_paf
  aggregate_paf_women[aggregate_paf_women$cancersite == "Lung", as.character(year)] <- cumulative_lung
  aggregate_paf_women[aggregate_paf_women$cancersite == "Colorectal", as.character(year)] <- cumulative_colorectal
  aggregate_paf_women[aggregate_paf_women$cancersite == "AC", as.character(year)] <- cumulative_ac
  aggregate_paf_women[aggregate_paf_women$cancersite == "SCC", as.character(year)] <- cumulative_scc
  aggregate_paf_women[aggregate_paf_women$cancersite == "Breast", as.character(year)] <- cumulative_breast
  aggregate_paf_women[aggregate_paf_women$cancersite == "Pancreas", as.character(year)] <- cumulative_pancreas
}

############################################################################
#graphing the aggregate PAFs

# Reshape aggregate_paf from wide to long format
aggregate_paf_long <- aggregate_paf_men%>%
  pivot_longer(cols = `2005`:`2019`, 
               names_to = "Year", 
               values_to = "PAF")
#need to remove 2019 for colorectal cancer because there isnt nutrition data for 2019
aggregate_paf_long <- aggregate_paf_long %>%
  mutate(PAF = if_else(cancersite == "Colorectal" & Year %in% c(2005, 2006, 2007, 2019), NA_real_, PAF))
# Convert Year to numeric for plotting
aggregate_paf_long$Year <- as.numeric(aggregate_paf_long$Year)
aggregate_paf_men <- aggregate_paf_long
# Create the plot
aggregatepaf_plot_men <- ggplot(aggregate_paf_men, aes(x = Year, y = PAF, color = cancersite, group = cancersite)) +
  geom_line() +
  geom_point() +
  labs(title = "Aggregate PAF for Men (2005-2019)",
       x = "Year",
       y = "Cumulative PAF") +
  theme_minimal() +
  ylim(0,.75) +
  scale_x_continuous(breaks = 2005:2019, limits = c(2005, 2019)) +
  scale_color_brewer(palette = "Dark2") 
print(aggregatepaf_plot_men)

#do it again for women
aggregate_paf_long <- aggregate_paf_women%>%
  pivot_longer(cols = `2005`:`2019`, 
               names_to = "Year", 
               values_to = "PAF")
aggregate_paf_long <- aggregate_paf_long %>%
  mutate(PAF = if_else(cancersite == "Colorectal" & Year %in% c(2005, 2006, 2007, 2019), NA_real_, PAF))
aggregate_paf_long$Year <- as.numeric(aggregate_paf_long$Year)
aggregate_paf_women <- aggregate_paf_long
# Create the plot
aggregatepaf_plot_women <- ggplot(aggregate_paf_women, aes(x = Year, y = PAF, color = cancersite, group = cancersite)) +
  geom_line() +
  geom_point() +
  labs(title = "Aggregate PAF for Women(2005-2019)",
       x = "Year",
       y = "Cumulative PAF") +
  theme_minimal() +
  ylim(0,.75) +
  scale_x_continuous(breaks = 2005:2019, limits = c(2005, 2019)) +
  scale_color_brewer(palette = "Dark2") 
print(aggregatepaf_plot_women)

##save the functions 
aggregate_paf_women <- aggregate_paf_women %>% 
  mutate(sex = "Women")
aggregate_paf_men <- aggregate_paf_men %>% 
  mutate(sex = "Men")
aggregate_PAF_all <- bind_rows(aggregate_paf_women, aggregate_paf_men) %>%
  dplyr::select(cancersite, Year, sex, PAF)
#write.csv(aggregate_PAF_all, "Findings/AggregatePAF.csv", row.names= FALSE)
