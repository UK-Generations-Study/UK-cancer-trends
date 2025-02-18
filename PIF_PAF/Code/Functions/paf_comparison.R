### PAF TREND EVALUATION ###

# This code is intended to test whether the first PAF and 2019 PAF for a risk factor/cancer combination are significantly different.
# It does this by using MC sampling.
# The RR will be sampled from a log-normal distribution, assuming log(RR) is normally distributed. - THIS HAS NOT BEEN DONE. This is because they are using the same RR so would just add variance
# The prevelances will be sampled using the prevelances and N values from the surveys.

## Packages
necessary_packages <- c("dplyr", "tidyr", "ggplot2")
suppressMessages(
  for (p in necessary_packages) {
    if (!require(p, character.only = TRUE)){
      install.packages(p)
    }
    library(p, character.only = TRUE)
  }
)

## Set working directory
# Setting up wd for relative file paths
# This sets wd to wherever the document is saved - this should be the github desktop folder
if(Sys.getenv("RSTUDIO") == '1' & !knitr::is_html_output()) { # If using Rstudio and not rendering
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else if(Sys.getenv("RSTUDIO") != '1'){ # If using Rscript
  initial.options <- commandArgs(trailingOnly = FALSE)
  file.arg.name <- "--file="
  script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
  script.basename <- dirname(script.name)
  setwd(file.path(getwd(), script.basename))
}


## Read in RF data
data_rf <- read.csv("../../../../Data/Cleaned_Data/clean_rf_data.csv")

# Need to fill in missing categories for the redmeat and processed meat consumption variables as well as fibre consumption
processed_meat_categories <- data_rf |>
  filter(variable == "processed_meat_consumption") |>
  pull(level) |>
  unique()

data_rf_processed_meat <- data_rf |>
  filter(variable == "processed_meat_consumption") |>
  complete(age_group, sex, year, level = processed_meat_categories) |>
  group_by(age_group, sex, year) |>
  mutate(
    
    year = if_else(is.na(year), first(na.omit(year)), year),
    age_group = if_else(is.na(age_group), first(na.omit(age_group)), age_group),
    sex = if_else(is.na(sex), first(na.omit(sex)), sex),
    value = replace_na(value, 0),
    N = if_else(is.na(N), first(na.omit(N)), N),
    variable = if_else(is.na(variable), first(na.omit(variable)), variable),
    
  ) |>
  ungroup()
  
  
redmeat_categories <- data_rf |>
  filter(variable == "redmeat_consumption") |>
  pull(level) |>
  unique()

data_rf_redmeat <- data_rf |>
  filter(variable == "redmeat_consumption") |>
  complete(age_group, sex, year, level = redmeat_categories) |>
  group_by(age_group, sex, year) |>
  mutate(
    
    year = if_else(is.na(year), first(na.omit(year)), year),
    age_group = if_else(is.na(age_group), first(na.omit(age_group)), age_group),
    sex = if_else(is.na(sex), first(na.omit(sex)), sex),
    value = replace_na(value, 0),
    N = if_else(is.na(N), first(na.omit(N)), N),
    variable = if_else(is.na(variable), first(na.omit(variable)), variable),
    
  ) |>
  ungroup()

fibre_categories <- data_rf |>
  filter(variable == "fibre_consumption") |>
  pull(level) |>
  unique()

data_rf_fibre <- data_rf |>
  filter(variable == "fibre_consumption") |>
  complete(age_group, sex, year, level = fibre_categories) |>
  group_by(age_group, sex, year) |>
  mutate(
    
    year = if_else(is.na(year), first(na.omit(year)), year),
    age_group = if_else(is.na(age_group), first(na.omit(age_group)), age_group),
    sex = if_else(is.na(sex), first(na.omit(sex)), sex),
    value = replace_na(value, 0),
    N = if_else(is.na(N), first(na.omit(N)), N),
    variable = if_else(is.na(variable), first(na.omit(variable)), variable),
    
  ) |>
  ungroup()

# Now add back in
data_rf <- data_rf |>
  filter(!variable %in% c("redmeat_consumption", "processed_meat_consumption", "fibre_consumption")) |>
  rbind(data_rf_processed_meat) |>
  rbind(data_rf_redmeat) |>
  rbind(data_rf_fibre)


## Read in RR estimates
data_rr_u50 <- read.csv("../../Data/relativerisk_under50.csv", na.strings = "")
data_rr_oe50 <- read.csv("../../Data/relativerisk_over50.csv", na.strings = "")

# # Filter RF data to just oldest and 2009 (or closest) estimate
# data_rf <- data_rf |>
#   filter(year <= 2019) |>
#   group_by(variable, sex, age_group) |>
#   mutate(
#     
#     dist_to_2009 = abs(year - 2009)
#     
#   ) |>
#   # Taking all closest to 2009 to calculated current PAFs but taking only BMI at specific time points for PIFs
#   mutate(
#     
#     # youngest_indicator = year == max(year),
#     # oldest_indicator = year == min(year),
#     close_to_2009 = dist_to_2009 == min(dist_to_2009)
#     
#   ) |>
#   filter(close_to_2009 | (variable == "bmi" & year %in% c(1995, 2005, 2019))) |>
#   ungroup() |>
#   select(-close_to_2009, -dist_to_2009)

# Clean up RR estimates
data_rr_u50 <- data_rr_u50 |>
  pivot_longer(cols = -Cancer_sites) |>
  mutate(age_group = "20-49")

data_rr_oe50 <- data_rr_oe50 |>
  pivot_longer(cols = -Cancer_sites) |>
  mutate(age_group = "50+")

# Combine to one RR data source
data_rr <- rbind(data_rr_oe50, data_rr_u50) |>
  mutate(
    
    RR = as.numeric(gsub("\\(.*", "", value)),
    CI = gsub(".*\\(", "", value),
    CI_lower = as.numeric(gsub("\\,.*", "", CI)),
    CI_higher = as.numeric(gsub("\\)|.*,\\s|.*,", "", CI))
    
  ) |>
  mutate(
    
    sex = case_when(
      grepl("\\_men", name) ~ "Men",
      grepl("\\_women", name) ~ "Women",
      TRUE ~ "All"
    ),
    
    variable = case_when(
      grepl("\\_Smoking", name) ~ "smoking_status",
      grepl("\\_alcohol", name) ~ "alcohol_amt",
      grepl("^BMI", name) ~ "bmi",
      name == "Processed_Meat" ~ "processed_meat_consumption",
      name == "Red_Meat" ~ "redmeat_consumption",
      name == "Fibre" ~ "fibre_consumption",
      name == "PhysicalActivity" ~ "physical_activity_old",
      TRUE ~ NA # More will need to be added as and when needed
    ),
    
    level = case_when(
      variable == "alcohol_amt" & grepl("^Light", name) ~ "Light Drinker",
      variable == "alcohol_amt" & grepl("^Medium", name) ~ "Moderate Drinker",
      variable == "alcohol_amt" & grepl("^Heavy", name) ~ "Heavy Drinker",
      variable == "smoking_status" & grepl("^Former", name) ~ "Previously",
      variable == "smoking_status" & grepl("^Current", name) ~ "Current",
      variable == "bmi" & grepl("\\_obese", name) ~ "Obese",
      variable == "bmi" & grepl("\\_overweight", name) ~ "Overweight",
      variable == "physical_activity_old" ~ "Below Recommendations",
      grepl("\\_consumption$", variable) ~ "dose_response",
      TRUE ~ NA
    )
    
  ) |>
  filter(!is.na(variable)) |>
  filter(!is.na(RR))

# Splitting "all" into men and women
data_rr_men <- data_rr |>
  filter(sex == "All") |>
  mutate(sex = "Men")

data_rr_women <- data_rr |>
  filter(sex == "All") |>
  mutate(sex = "Women")

data_rr <- data_rr |>
  filter(sex != "All") |>
  rbind(data_rr_men) |>
  rbind(data_rr_women)


# Need to grab midpoints for levels in the risk factors data
midpoints_inf <- data_rf |>
  filter(variable %in% c("redmeat_consumption_cat_mean", "processed_meat_consumption_cat_mean")) |>
  filter(grepl("\\,Inf\\)$|\\,Inf\\]$", level)) |>
  mutate(variable = gsub("\\_cat\\_mean", "", variable)) |>
  select(-N) |>
  rename(level_midpoint = value)
  
# Now adding in midpoints and adjusting level to be these midpoints
data_rf <- data_rf |>
  merge(midpoints_inf, by = c("year", "age_group", "sex", "level", "variable"), all.x = T) |>
  mutate(
    
    level_midpoint = case_when(
      !grepl("\\_consumption$", variable) ~ 1,
      !grepl("\\,Inf\\)$|\\,Inf\\]$", level) ~ (as.numeric(gsub("\\)|.*\\,|\\]", "", level)) + as.numeric(gsub("\\[|\\(|\\,.*", "", level)))/2,
      grepl("\\,Inf\\)$|\\,Inf\\]$", level) & variable == "fibre_consumption" ~ 30,
      TRUE ~ level_midpoint
    ),
    
    # Flipping fibre_consumption
    level_midpoint = case_when(
      variable == "fibre_consumption" ~ 30 - level_midpoint,
      TRUE ~ level_midpoint
    ),
    
  ) |>
  mutate(
    
    # Adding level_label
    level_label = level,
    
    # Recoding level variable so that we can merge with RR data
    level = case_when(
      grepl("\\_consumption$", variable) ~ "dose_response",
      TRUE ~ level
    )
    
  ) |>
  rename(perc = value)

# Combine RR and RF data together
data_complete <- merge(data_rr, data_rf, by = c("age_group", "sex", "level", "variable"), all.y = T) |>
  filter(!is.na(RR)) |>
  # Arrange so all arguments are in the right order when extracting p-values
  arrange(Cancer_sites, age_group, sex, year, variable, level)


# # Calculate PAF
# data_complete_paf <- data_complete |>
#   mutate(
#     
#     
#     ERR_calc = case_when(
#       variable == "physical_activity_old" ~ log(1/RR),
#       variable == "fibre_consumption" ~ log(1/RR)/10,
#       variable %in% c("redmeat_consumption", "processed_meat_consumption") ~ (RR-1)/100,
#       TRUE ~ RR - 1
#     ),
#     
#     # Correct for preventative RF
#     ERR_calc = pmax(ERR_calc, 0)
#     
#   ) |>
#   group_by(age_group, sex, variable, year, Cancer_sites) |>
#   summarise(
#     
#     PAF = sum(ERR_calc * level_midpoint * perc)/(1 + sum(ERR_calc * level_midpoint * perc))
#     
#   )

# Filter to closest to 2009 - but keep copy with other years as we need to sample their prevalences
data_complete_all <- data_complete

data_complete <- data_complete |>
    filter(year <= 2019) |>
    group_by(variable, sex, age_group) |>
    mutate(

      dist_to_2009 = abs(year - 2009)

    ) |>
    # Taking all closest to 2009 to calculated current PAFs but taking only BMI at specific time points for PIFs
    mutate(

      # youngest_indicator = year == max(year),
      # oldest_indicator = year == min(year),
      close_to_2009 = dist_to_2009 == min(dist_to_2009)

    ) |>
    filter(close_to_2009 | (variable == "bmi" & year %in% c(1995, 2005, 2019))) |>
    ungroup() |>
    select(-close_to_2009, -dist_to_2009) |>
    # Arrange so all arguments are in the right order when extracting p-values
    arrange(Cancer_sites, age_group, sex, year, variable, level)

# Set up for loop
no_groups <- data_complete |>
  group_by(variable, level, year, sex, age_group, Cancer_sites) |>
  summarise() |>
  nrow()


# Loop for N times
for(i in 1:5){
  
  # Get copy of data
  data_complete_sample <- data_complete
  
  # Resample RR based on CI
  norms <- rnorm(no_groups)
  
  data_complete_sample <- data_complete_sample |>
    group_by(variable, level, year, sex, age_group, Cancer_sites) |>
    mutate(
      
      variance = (log(CI_higher) - log(RR))/qnorm(0.975),
      
      RR = exp(log(RR) + variance*norms[cur_group_id()])
      
    ) |>
    ungroup() |>
    group_by(variable, year, sex, age_group, Cancer_sites) |>
    mutate(group_id = cur_group_id())
  
  # Resample prevelances from a binomial - adjusting for rolling averages
  for(id in unique(data_complete_sample$group_id)){
    
    # Extract year and variable of id
    cur_year <- unique(data_complete_sample$year[data_complete_sample$group_id == id])
    cur_variable <- unique(data_complete_sample$variable[data_complete_sample$group_id == id])
    cur_sex <- unique(data_complete_sample$sex[data_complete_sample$group_id == id])
    cur_age_group <- unique(data_complete_sample$age_group[data_complete_sample$group_id == id])
    cur_Cancer_sites <- unique(data_complete_sample$Cancer_sites[data_complete_sample$group_id == id])
    
    # Resample for current year
    N <- data_complete_sample |>
      filter(group_id == id) |>
      pull(N) |>
      unique() |>
      round()
    
    p_values <- data_complete_sample |>
      filter(group_id == id) |>
      pull(perc)
    
    # If all categories are represented - simple sample, otherwise create 'other' category and remove afterwards
    if(sum(p_values)<1){
      
      resampled_p_values <- rmultinom(N, 1, c(p_values, 1-sum(p_values))) |>
        t() |>
        as.data.frame() |>
        colMeans()
      
      resampled_p_values <- resampled_p_values[-length(resampled_p_values)]
      
    } else {
      
      resampled_p_values <- rmultinom(N, 1, p_values) |>
        t() |>
        as.data.frame() |>
        colMeans()
      
    }
    
    # Check for a year before in the sample - if there is one then sample p-value from that, if not just output vector of NAs
    if(nrow(data_complete_all |> filter(year == cur_year-1) |> filter(variable == cur_variable)) > 0){
      
      N_below <- data_complete_all |>
        filter(year == cur_year - 1, variable == cur_variable, sex == cur_sex, age_group == cur_age_group, Cancer_sites == cur_Cancer_sites) |>
        pull(N) |>
        unique() |>
        round()
      
      p_values_below <- data_complete_all |>
        filter(year == cur_year - 1, variable == cur_variable, sex == cur_sex, age_group == cur_age_group, Cancer_sites == cur_Cancer_sites) |>
        pull(perc)
      
      # If all categories are represented - simple sample, otherwise create 'other' category and remove afterwards
      if(sum(p_values_below)<1){
        
        resampled_p_values_below <- rmultinom(N_below, 1, c(p_values_below, 1-sum(p_values_below))) |>
          t() |>
          as.data.frame() |>
          colMeans()
        
        resampled_p_values_below <- resampled_p_values_below[-length(resampled_p_values_below)]
        
      } else {
        
        resampled_p_values_below <- rmultinom(N_below, 1, p_values_below) |>
          t() |>
          as.data.frame() |>
          colMeans()
        
      }
      
      
    } else {
      
      resampled_p_values_below <- rep(NA, length(resampled_p_values))
      
    }
    
    # Check for a year after in the sample - if there is one then sample p-value from that, if not just output vector of NAs
    if(nrow(data_complete_all |> filter(year == cur_year+1) |> filter(variable == cur_variable)) > 0){
      
      N_above <- data_complete_all |>
        filter(year == cur_year + 1, variable == cur_variable, sex == cur_sex, age_group == cur_age_group, Cancer_sites == cur_Cancer_sites) |>
        pull(N) |>
        unique() |>
        round()
      
      p_values_above <- data_complete_all |>
        filter(year == cur_year + 1, variable == cur_variable, sex == cur_sex, age_group == cur_age_group, Cancer_sites == cur_Cancer_sites) |>
        pull(perc)
      
      # If all categories are represented - simple sample, otherwise create 'other' category and remove afterwards
      if(sum(p_values_above)<1){
        
        resampled_p_values_above <- rmultinom(N_above, 1, c(p_values_above, 1-sum(p_values_above))) |>
          t() |>
          as.data.frame() |>
          colMeans()
        
        resampled_p_values_above <- resampled_p_values_above[-length(resampled_p_values_above)]
        
      } else {
        
        resampled_p_values_above <- rmultinom(N_above, 1, p_values_above) |>
          t() |>
          as.data.frame() |>
          colMeans()
        
      }
      
      
    } else {
      
      resampled_p_values_above <- rep(NA, length(resampled_p_values))
      
    }
    
    
    # Now combine all estimates of p_values together and take the mean
    resampled_perc <- colMeans(rbind(resampled_p_values, resampled_p_values_below, resampled_p_values_above), na.rm = T)
    
    # Combine into 
    data_complete_sample_new <- data_complete_sample |>
      filter(group_id == id) |>
      mutate(perc = resampled_perc)
      
    # Now add into data_complete sample
    data_complete_sample <- data_complete_sample |>
      filter(group_id != id) |>
      rbind(data_complete_sample_new)

  }
  
  # # Resample prevelances from a binomial
  # for(id in unique(data_complete_sample$group_id)){
  #   
  #   N <- data_complete_sample |>
  #     filter(group_id == id) |>
  #     pull(N) |>
  #     unique() |>
  #     round()
  #   
  #   p_values <- data_complete_sample |>
  #     filter(group_id == id) |>
  #     pull(perc)
  #   
  #   if(sum(p_values)<1){
  #     
  #     resampled_p_values <- rmultinom(N, 1, c(p_values, 1-sum(p_values))) |>
  #       t() |>
  #       as.data.frame() |>
  #       colMeans()
  #     
  #     data_complete_sample_new <- data_complete_sample |>
  #       filter(group_id == id) |>
  #       mutate(perc = resampled_p_values[-length(resampled_p_values)])
  #     
  #   } else {
  #     
  #     resampled_p_values <- rmultinom(N, 1, p_values) |>
  #       t() |>
  #       as.data.frame() |>
  #       colMeans()
  #     
  #     data_complete_sample_new <- data_complete_sample |>
  #       filter(group_id == id) |>
  #       mutate(perc = resampled_p_values)
  #     
  #   }
  #   
  #   data_complete_sample <- data_complete_sample |>
  #     filter(group_id != id) |>
  #     rbind(data_complete_sample_new)
  #   
  # }
  
  
  # Now calculate PAFs
  
  data_complete_paf_sample <- data_complete_sample |>
    mutate(
      
      
      ERR_calc = case_when(
        variable == "physical_activity_old" ~ log(1/RR),
        variable == "fibre_consumption" ~ log(1/RR)/10,
        variable %in% c("redmeat_consumption", "processed_meat_consumption") ~ (RR-1)/100,
        TRUE ~ RR - 1
      ),
      
      # Correct for preventative RF
      ERR_calc = pmax(ERR_calc, 0)
      
    ) |>
    group_by(age_group, sex, variable, year, Cancer_sites) |>
    summarise(
      
      PAF = sum(ERR_calc * level_midpoint * perc)/(1 + sum(ERR_calc * level_midpoint * perc))
      
    )
  
  
  # Now if i = 1 initialise dataframe, otherwise rbind to it
  if(i == 1){
    data_complete_paf_analysis <- data_complete_paf_sample
  } else {
    data_complete_paf_analysis <- rbind(data_complete_paf_analysis, data_complete_paf_sample)
  }
  
}

## Output data
# write.csv(data_complete_paf_analysis, file = "../../Data/paf_comparison.csv", row.names = F)


# # Now compare empirically between the years
# data_complete_paf_analysis_test <- data_complete_paf_analysis |>
#   group_by(Cancer_sites, age_group, sex, variable) |>
#   mutate(
#     
#     type = case_when(
#       year == min(year) ~ "Current",
#       TRUE ~ "Future"
#     ),
#     
#   ) |>
#   ungroup() |>
#   group_by(Cancer_sites, age_group, sex, variable, type) |>
#   mutate(
#     
#     in_type_no = row_number()
#     
#   ) |>
#   ungroup() |>
#   pivot_wider(id_cols = c("Cancer_sites", "age_group", "sex", "variable", "in_type_no"), names_from = type, values_from = PAF) |>
#   group_by(Cancer_sites, age_group, sex, variable) |>
#   summarise(PAF_diff = sum(Current > Future)/n())
#   
# # Plotting differences
# plot <- ggplot(data_complete_paf_analysis |> mutate(label = paste0(variable, " | ", sex, " | ", age_group)) |> merge(data_complete_paf_analysis_test), aes(x = PAF)) +
#   geom_density(alpha=0.3, aes(fill = as.character(year)), colour = "black") +
#   geom_label(aes(label = PAF_diff, x = Inf, y = Inf),
#              hjust = 1, vjust = 1, fill = "white", color = "black", size = 3,
#              position = position_nudge(y = -0.02)) +
#   theme_minimal() +
#   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
#             colour = "black", fill = NA, inherit.aes = FALSE) +
#   theme(strip.text = element_text(face = "bold")) +
#   facet_wrap(Cancer_sites~label, scales = "free", ncol = 8)
# 
# ggsave(plot, filename = "../../Output/test.png", bg = "white", width = 20, height = 20)
