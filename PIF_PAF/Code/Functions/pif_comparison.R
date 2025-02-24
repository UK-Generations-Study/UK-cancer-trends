### PAF TREND EVALUATION ###

# This code is intended to test whether the first PAF and 2019 PAF for a risk factor/cancer combination are significantly different.
# It does this by using MC sampling.
# The RR will be sampled from a log-normal distribution, assuming log(RR) is normally distributed. - THIS HAS NOT BEEN DONE. This is because they are using the same RR so would just add variance
# The prevelances will be sampled using the prevelances and N values from the surveys.

## Options
N_iterations <- 1000

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

## Read in RR estimates
data_rr_u50 <- read.csv("../../Data/relativerisk_under50.csv", na.strings = "")
data_rr_oe50 <- read.csv("../../Data/relativerisk_over50.csv", na.strings = "")


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


# Filter to closest to 2009 - but keep copy with other years as we need to sample their prevalences
data_complete_all <- data_complete

data_complete <- data_complete |>
  filter(year <= 2019) |>
  filter(variable == "bmi" & year %in% c(1995, 2005, 2009, 2019)) |>
  # Create indicator for year groupings
  mutate(
    year_group = case_when(
      year %in% c(1995, 2009) ~ 1,
      year %in% c(1995, 2009) ~ 2,
      TRUE ~ NA
    )
  ) |>
  # Arrange so all arguments are in the right order when extracting p-values
  arrange(Cancer_sites, age_group, sex, year, variable, level)

# Set up for loop
no_groups <- data_complete |>
  group_by(variable, level, year_group, sex, age_group, Cancer_sites) |>
  summarise() |>
  nrow()

# Get start time for total time estimation
start.time = Sys.time()

# Loop for N times
for(i in 1:N_iterations){
  
  # Print counter
  cat(paste0("Iteration ", i, "...\n"))
  
  # Get copy of data
  data_complete_sample <- data_complete
  
  # Resample RR based on CI
  norms <- rnorm(no_groups)
  
  data_complete_sample <- data_complete_sample |>
    # Group by year_group to resample RRs
    group_by(variable, level, year_group, sex, age_group, Cancer_sites) |>
    mutate(
      
      variance = (log(CI_higher) - log(RR))/qnorm(0.975),
      
      RR = exp(log(RR) + variance*norms[cur_group_id()])
      
    ) |>
    ungroup() |>
    group_by(variable, year, sex, age_group, Cancer_sites) |>
    # Use new grouping as PAF should be defined on a variable by variable basis
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
      
    ) |>
    suppressMessages() |>
    ungroup() |>
    mutate(iteration = i)
  
  
  # Now if i = 1 initialise dataframe, otherwise rbind to it
  if(i == 1){
    data_complete_paf_analysis <- data_complete_paf_sample
  } else {
    data_complete_paf_analysis <- rbind(data_complete_paf_analysis, data_complete_paf_sample)
  }
  
  # Estimate time remaining
  end.time <- Sys.time()
  elapsed.time <- difftime(end.time, start.time, units = "mins")
  total.time.est <- N_iterations*(elapsed.time/i)
  total.time.remaining <- total.time.est - elapsed.time
  cat(paste0("Estimated time remaining: ", round(total.time.remaining, digits = 2), " minutes\n"))
  
  
  
}

## Output data
write.csv(data_complete_paf_analysis, file = "../../Data/pif_comparison.csv", row.names = F)
