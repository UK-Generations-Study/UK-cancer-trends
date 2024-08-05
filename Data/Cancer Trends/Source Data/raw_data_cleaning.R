### Raw Incidence Data CLEANING ###

# This code is designed to clean the raw cancer incidence and mortality data drawn from national sources into one .csv that can be used for input into joinpoint and plotting.

# Output columns:
#   - Year: year of the incidence rates
#   - Country: country the data is from
#   - ASR: age standardised rate
#   - Sex: sex the rate applies to


## PACKAGES
library(dplyr)
library(readxl)
library(zoo)

### INCIDENCE

## ENGLAND DATA
cancer_england <- read_excel(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Incidence Data\Cancer_England.xlsx)") |>
  mutate(Country = "England") |> 
  filter(`Site description` %in% c("All malignant cancers excluding non-melanoma skin cancer (NMSC)",
                                   "Malignant neoplasm of breast",
                                   "Malignant neoplasm of colon and rectum",
                                   "Malignant neoplasm of trachea, bronchus and lung",
                                   "Malignant neoplasm of oesophagus",
                                   "Malignant neoplasm of pancreas",
                                   "Malignant neoplasm of prostate")) |>
  select(Year, Country, Cancer_Site = `Site description`, Sex = Gender, Incidences = Count, ASR = `Rate (per 100,000 population)`) # Sex == Gender comparison not accurate, but necessary for statistical comparisons between countries.

## SCOTLAND DATA
cancer_scotland <- read_excel(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Incidence Data\Cancer_Scotland.xlsx)") |>
  mutate(Country = "Scotland") |>
  filter(CancerSite %in% c("All cancer types",
                           "Breast",
                           "Trachea, bronchus and lung",
                           "Colorectal cancer",
                           "Oesophagus",
                           "Pancreas",
                           "Prostate")) |>
  select(Year, Country, Cancer_Site = CancerSite, Sex, Incidences = IncidencesAllAges, ASR = EASR)

## WALES DATA 
cancer_wales <- data.frame(Year = numeric(0), Country = character(0), Sex = character(0), ASR = numeric(0))
cancer_wales_sheets <- excel_sheets(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Incidence Data\Cancer_Wales.xlsx)")

# Loop through sheets, adding data to dataframe
for(sheet in cancer_wales_sheets){
  
  temp_data <- read_excel(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Incidence Data\Cancer_Wales.xlsx)", sheet = sheet) |>
    select(Year, Country = Geography, Cancer_Site = Cancer_type, Sex = Sex, ASR = Value) |>
    # Need to add Wales raw incidence data
    mutate(Incidences = NA)
    
  cancer_wales <- rbind(cancer_wales, temp_data)
  
}

## NORTHERN IRELAND DATA
cancer_nireland <- data.frame(Year = numeric(0), Country = character(0), Sex = character(0), ASR = numeric(0))
cancer_nireland_sheets <- excel_sheets(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Incidence Data\Cancer_NIreland.xlsx)")

# Loop through sheets, adding data to dataframe
for(sheet in cancer_nireland_sheets){
  
  temp_data <- read_excel(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Incidence Data\Cancer_NIreland.xlsx)", sheet = sheet)
  
  # Get sexes that this applies to
  groups <- unique(temp_data[4,])[!is.na(unique(temp_data[4,]))]
  
  # Filter dataframe to useful data
  colnames(temp_data) <- temp_data[5,]
  temp_data <- temp_data[-(1:5),]
  
  # Get ASR data
  ASR_data <- as.vector(as.matrix(temp_data[,grepl(colnames(temp_data), pattern = "European age-standardised incidence rate per 100,000")]))
  
  # Get raw incidence rates
  Incidence_data <- as.vector(as.matrix(temp_data[,grepl(colnames(temp_data), pattern = "Total number of")]))
  
  # Combine into one dataframe
  temp_data_2 <- data.frame(Year = rep(temp_data$`Year of diagnosis`, length(groups)),
                            Country = "Nothern Ireland",
                            Cancer_Site = sheet,
                            Sex = rep(groups, each = nrow(temp_data)),
                            Incidences = Incidence_data,
                            ASR = ASR_data)

  cancer_nireland <- rbind(cancer_nireland, temp_data_2)
  
}


## COMBINE DATA
cancer_data <- rbind(cancer_england, cancer_scotland) |>
  rbind(cancer_wales) |>
  rbind(cancer_nireland)


## CLEAN DATA
cancer_data <- cancer_data |>
  mutate(
    
    Year = as.numeric(Year),
    
    # Cleaning and standardising site of cancer
    Cancer_Site = case_when(
      grepl(Cancer_Site, pattern = "All") ~ "All sites excl. NMSC",
      grepl(Cancer_Site, pattern = "Breast", ignore.case = T) ~ "Breast",
      grepl(Cancer_Site, pattern = "Colo|colo") ~ "Colorectal",
      grepl(Cancer_Site, pattern = "Lung|lung") ~ "Lung",
      grepl(Cancer_Site, pattern = "Oesophag", ignore.case = T) ~ "Oesophageal",
      grepl(Cancer_Site, pattern = "Pancrea", ignore.case = T) ~ "Pancreatic",
      grepl(Cancer_Site, pattern = "Prostate", ignore.case = T) ~ "Prostate",
      TRUE ~ NA),
    
    # Standardising Sex variable
    Sex = case_when(
      grepl(Sex, pattern = "female", ignore.case = T) ~ "Women",
      grepl(Sex, pattern = "male", ignore.case = T) ~ "Men",
      grepl(Sex, pattern = "All|Persons", ignore.case = T) ~ "All",
      TRUE ~ Sex),
    
    ASR = as.numeric(ASR)
    
    
  ) |>
  # Filtering for specified years
  # filter(between(Year, 2000, 2019)) |>
  # Filtering out male breast cancer
  filter(!(Sex == "Men" & Cancer_Site == "Breast")) |>
  # England doesn't have rates for men and women combined so have to filter this out
  filter(Sex != "All") |>
  # Arrange for joinpoint
  arrange(Cancer_Site, Country, Sex)



# Write output
write.csv(cancer_data, file = r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Cancer_Incidence_Data.csv)", row.names = F)


### MORTALITY

## ENGLAND DATA
cancer_england <- read_excel(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Mortality Data\Cancer_England.xlsx)") |>
  mutate(Country = "England") |> 
  filter(`Site description` %in% c("All malignant cancers excluding non-melanoma skin cancer (NMSC)",
                                   "Malignant neoplasm of breast",
                                   "Malignant neoplasm of colon and rectum",
                                   "Malignant neoplasm of trachea, bronchus and lung",
                                   "Malignant neoplasm of oesophagus",
                                   "Malignant neoplasm of pancreas",
                                   "Malignant neoplasm of prostate")) |>
  select(Year, Country, Cancer_Site = `Site description`, Sex = Gender, Mortalities = Count, ASR = `Rate (per 100,000 population)`) # Sex == Gender comparison not accurate, but necessary for statistical comparisons between countries.

## SCOTLAND DATA
cancer_scotland <- read_excel(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Mortality Data\Cancer_Scotland.xlsx)") |>
  mutate(Country = "Scotland") |>
  filter(CancerSite %in% c("All cancer types",
                           "Breast",
                           "Trachea, bronchus and lung",
                           "Colorectal cancer",
                           "Oesophagus",
                           "Pancreas",
                           "Prostate")) |>
  select(Year, Country, Cancer_Site = CancerSite, Sex, Mortalities = DeathsAllAges, ASR = EASR)

## WALES DATA 
cancer_wales <- data.frame(Year = numeric(0), Country = character(0), Sex = character(0), ASR = numeric(0))
cancer_wales_sheets <- excel_sheets(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Mortality Data\Cancer_Wales.xlsx)")

# Loop through sheets, adding data to dataframe
for(sheet in cancer_wales_sheets){
  
  temp_data <- read_excel(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Mortality Data\Cancer_Wales.xlsx)", sheet = sheet) |>
    select(Year, Country = Geography, Cancer_Site = Cancer_type, Sex = Sex, ASR = Value) |>
    # Need to add Wales raw Mortality data
    mutate(Mortalities = NA)
  
  cancer_wales <- rbind(cancer_wales, temp_data)
  
}

## NORTHERN IRELAND DATA
cancer_nireland <- data.frame(Year = numeric(0), Country = character(0), Sex = character(0), ASR = numeric(0))
cancer_nireland_sheets <- excel_sheets(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Mortality Data\Cancer_NIreland.xlsx)")

# Loop through sheets, adding data to dataframe
for(sheet in cancer_nireland_sheets){
  
  temp_data <- read_excel(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Raw Mortality Data\Cancer_NIreland.xlsx)", sheet = sheet)
  
  # Get sexes that this applies to
  groups <- unique(temp_data[4,])[!is.na(unique(temp_data[4,]))]
  
  # Filter dataframe to useful data
  colnames(temp_data) <- temp_data[5,]
  temp_data <- temp_data[-(1:5),]
  
  # Get ASR data
  ASR_data <- as.vector(as.matrix(temp_data[,grepl(colnames(temp_data), pattern = "European age-standardised mortality rate per 100,000")]))
  
  # Get raw Mortality rates
  Mortality_data <- as.vector(as.matrix(temp_data[,grepl(colnames(temp_data), pattern = "Total number of")]))
  
  # Combine into one dataframe
  temp_data_2 <- data.frame(Year = rep(temp_data$`Year of death`, length(groups)),
                            Country = "Nothern Ireland",
                            Cancer_Site = sheet,
                            Sex = rep(groups, each = nrow(temp_data)),
                            Mortalities = Mortality_data,
                            ASR = ASR_data)
  
  cancer_nireland <- rbind(cancer_nireland, temp_data_2)
  
}


## COMBINE DATA
cancer_data <- rbind(cancer_england, cancer_scotland) |>
  rbind(cancer_wales) |>
  rbind(cancer_nireland)


## CLEAN DATA
cancer_data <- cancer_data |>
  mutate(
    
    Year = as.numeric(Year),
    
    # Cleaning and standardising site of cancer
    Cancer_Site = case_when(
      grepl(Cancer_Site, pattern = "All") ~ "All sites excl. NMSC",
      grepl(Cancer_Site, pattern = "Breast", ignore.case = T) ~ "Breast",
      grepl(Cancer_Site, pattern = "Colo|colo") ~ "Colorectal",
      grepl(Cancer_Site, pattern = "Lung|lung") ~ "Lung",
      grepl(Cancer_Site, pattern = "Oesophag", ignore.case = T) ~ "Oesophageal",
      grepl(Cancer_Site, pattern = "Pancrea", ignore.case = T) ~ "Pancreatic",
      grepl(Cancer_Site, pattern = "Prostate", ignore.case = T) ~ "Prostate",
      TRUE ~ NA),
    
    # Standardising Sex variable
    Sex = case_when(
      grepl(Sex, pattern = "female", ignore.case = T) ~ "Women",
      grepl(Sex, pattern = "male", ignore.case = T) ~ "Men",
      grepl(Sex, pattern = "All|Persons", ignore.case = T) ~ "All",
      TRUE ~ Sex),
    
    ASR = as.numeric(ASR)
    
    
  ) |>
  # Filtering for specified years
  # filter(between(Year, 2000, 2019)) |>
  # Filtering out male breast cancer
  filter(!(Sex == "Men" & Cancer_Site == "Breast")) |>
  # England doesn't have rates for men and women combined so have to filter this out
  filter(Sex != "All") |>
  # Arrange for joinpoint
  arrange(Cancer_Site, Country, Sex)



# Write output
write.csv(cancer_data, file = r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Cancer_Mortality_Data.csv)", row.names = F)

## 3 YEAR ROLLING AVERAGES
cancer_data <- cancer_data |>
  arrange(Year) |>
  group_by(Country, Sex, Cancer_Site) |>
  mutate(
    
    ASR_rolling = rollmean(ASR, k = 3, fill = NA)
    
    ) |>
  ungroup() |>
  filter(!is.na(ASR_rolling)) |>
  arrange(Country, Cancer_Site, Sex, Year)
    
# Write output
write.csv(cancer_data, file = r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\UK-cancer-trends\Data\Cancer Trends\Source Data\Cancer_Mortality_Data_Rolling_Avg.csv)", row.names = F)
