### CANCER SITE SELECTION ###

# This code is designed to find the list of cancer sites that meet the criteria of having a significantly increasing AAPC in young people (20-49)

# Packages
library(dplyr)
library(tidyr)

# Read in data
data <- read.delim(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\PAF\UK-cancer-trends\PIF_PAF\Data\Joinpoint_Results\Cancerrates_All\joinpoint_cancerrates_all.Export.AAPC.txt)", sep = "\t")

# Filter data by criteria and view
data |>
  filter(age_group == "20-49") |>
  filter(Statistically.Significant..0.No..1.Yes. == 1) |>
  filter(AAPC > 0) |>
  select(cancer_site) |>
  distinct() |>
  View()

# Comparing statistically between older and younger 
data_aapc <- data |>
  filter(cancer_site %in% c("Oral", "Endometrium", "Pancreas", "Gallbladder", "Colorectum", "Liver", "Kidney", "MultipleMyeloma", "Breast", "Thyroid")) |>
  mutate(cancer_site = if_else(cancer_site == "MultipleMyeloma", "Multiple Myeloma", cancer_site)) |>
  dplyr::mutate(cancer = cancer_site,
                agegrp = age_group,
                JPmodel = Joinpoint.Model,
                aapc_index = AAPC.Index,
                start_obs = Start.Obs,
                end_obs = End.Obs,
                aapc = AAPC,
                CIlow = AAPC.C.I..Low,
                CIhigh = AAPC.C.I..High
                ) %>%
  dplyr::mutate(agegrp = case_when(agegrp == "20-49" ~ "u50",
                                             agegrp == "50+" ~ "o50",
                                             T ~ agegrp),
                degfreedom = case_when( # This part is new - based on joinpoint definition of degrees of freedom
                  JPmodel != 0 ~ NA,
                  TRUE ~ end_obs - start_obs + 1 - 2
                )
  ) %>% 
  dplyr::select(cancer, sex, agegrp, JPmodel, aapc_index, start_obs, end_obs, aapc, CIlow, CIhigh, degfreedom)

data_results <- data_aapc |>
  process_data_new() |>
  compute_pvalues_N(N = 10000) |>
  mutate(
    
    p_value_one_sided = case_when(
      aapc_u50 > aapc_o50 ~ p_value/2,
      TRUE ~ 1-p_value/2
    )
    
  )
