README_Internal_Data


***
UNUSED DATASETS I THINK
riskfactor_lineartrends.csv
paf_comparison.csv 
incidence_data_clean.csv 
***

***
UNREGISTERED DATASETS
all_incidence_joinpoint.csv 
***

This README is designed to describe the internal data sources being used for this project.
As many of them are UKDS-based, that data is stored externally from this GitHub repo.
Each internal one is described here as read in from the code, including a URL, date of access and description.

Incidence_data_for_England_2024-10-30.csv
	URL - https://www.cancerdata.nhs.uk/incidence_and_mortality
		- Incidence
		- Geography break down: England
		- Combine year combinations: No
		- Single year options: Select All
		- 3 year rolling options: None selected
		- 5 year rolling options: None selected
		- Gender: Select All
		- Combine age groups: Yes
		- Age group: 20 to 24, 25 to 29, 30 to 34, 35 to 39, 40 to 44, 45 to 49
		- Combine site groups: No
		- Tumour sites: Select All
	Date Accessed - 30/10/2024
	Description - Data on cancer incidence in England across a range of years, cancers and sexes for people age 20-49.

Incidence_data_for_England_2024-10-30 (1).csv
	URL - https://www.cancerdata.nhs.uk/incidence_and_mortality
		- Incidence
		- Geography break down: England
		- Combine year combinations: No
		- Single year options: Select All
		- 3 year rolling options: None selected
		- 5 year rolling options: None selected
		- Gender: Select All
		- Combine age groups: Yes
		- Age group: 50 to 54, 55 to 59, 60 to 64, 65 to 69, 70 to 74, 75 to 79, 80 to 84, 85 to 89, 90 and over
		- Combine site groups: No
		- Tumour sites: Select All
	Date Accessed - 30/10/2024
	Description - Data on cancer incidence in England across a range of years, cancers and sexes for people age 50+.
	
Incidence_data_for_England_2025-01-31.csv
	URL - https://nhsd-ndrs.shinyapps.io/incidence_and_mortality/
		- Incidence by ICD10 code
		- Geography break down: England
		- Combine year combinations: No
		- Single year options: Select All
		- 3 year rolling options: None selected
		- 5 year rolling options: None selected
		- Gender: Select All
		- Combine age groups: Yes
		- Age group: 20 to 24, 25 to 29, 30 to 34
		- Combine site groups: No
		- Tumour sites: Select All
	Date Accessed - 31/1/2025
	Description - Data on cancer incidence in England across a range of years, cancers and sexes for people age 20-34.
	
Incidence_data_for_England_2025-01-31 (1).csv
	URL - https://nhsd-ndrs.shinyapps.io/incidence_and_mortality/
		- Incidence by ICD10 code
		- Geography break down: England
		- Combine year combinations: No
		- Single year options: Select All
		- 3 year rolling options: None selected
		- 5 year rolling options: None selected
		- Gender: Select All
		- Combine age groups: Yes
		- Age group: 35 to 39, 40 to 44, 45 to 49
		- Combine site groups: No
		- Tumour sites: Select All
	Date Accessed - 31/1/2025
	Description - Data on cancer incidence in England across a range of years, cancers and sexes for people age 35-49.
	
Incidence_data_for_England_2025-01-31 (2).csv
	URL - https://nhsd-ndrs.shinyapps.io/incidence_and_mortality/
		- Incidence by ICD10 code
		- Geography break down: England
		- Combine year combinations: No
		- Single year options: Select All
		- 3 year rolling options: None selected
		- 5 year rolling options: None selected
		- Gender: Select All
		- Combine age groups: Yes
		- Age group: 50 to 54, 55 to 59, 60 to 64
		- Combine site groups: No
		- Tumour sites: Select All
	Date Accessed - 31/1/2025
	Description - Data on cancer incidence in England across a range of years, cancers and sexes for people age 50-64.
	
Incidence_data_for_England_2025-01-31 (4).csv
	URL - https://nhsd-ndrs.shinyapps.io/incidence_and_mortality/
		- Incidence by ICD10 code
		- Geography break down: England
		- Combine year combinations: No
		- Single year options: Select All
		- 3 year rolling options: None selected
		- 5 year rolling options: None selected
		- Gender: Select All
		- Combine age groups: Yes
		- Age group: 65 to 69, 70 to 74, 75 to 79, 80 to 84, 85 to 89, 90 and over
		- Combine site groups: No
		- Tumour sites: Select All
	Date Accessed - 31/1/2025
	Description - Data on cancer incidence in England across a range of years, cancers and sexes for people age 65+.
	
HSE-2022-Overweight-and-obesity-tables.xlsx
	URL - https://digital.nhs.uk/data-and-information/publications/statistical/health-survey-for-england/2022-part-2/health-survey-for-england-hse-2022-part-2-data-tables
		- HSE 2022 Adult and children's overweight and obesity tables
	Date Accessed - 29/11/2024
	Description - HSE data on BMI in England

childhoodbmi_forjoinpoint.csv
	Source - This is cleaned data from HSE-2022-Overweight-and-obesity-tables.xlsx
	Date Accessed - 08/01/2025
	Description - This is the data from the HSE 2022 dataset but is arranged for Joinpoint input 

UPFprev_Rauber.csv
	Source - https://pmc.ncbi.nlm.nih.gov/articles/PMC7194406/#ad93
	Date Accessed - 09/01/2025
	Description - UPF energy prevalence extracted from Ultra-processed food consumption and indicators of obesity in the United Kingdom population (2008-2016) by Rauber et al. 2020

**DERIVED DATASETS 
RiskFactorInclusion.xls
	Description - This is an excel spreadsheet that contains all of the sources for the RR data 

relativerisk_under50.csv
	Source - Sources logged in RiskFactorInclusion.xls
	Date Accessed - 15/01/2025
	Description - This is the RR for modifiable risk factors for the relevant cancer sites for the population under 50

relativerisk_over50.csv
	Source - Sources logged in RiskFactorInclusion.xls
	Date Accessed - 15/01/2025
	Description - This is the RR for modifiable risk factors for the relevant cancer sites for the population over 50

PAFs_byriskfactor_AllAges.csv
	Description - These are calculated Population Attributable Fraction (PAFs) by risk factor for each cancer site divided by sex but not by age (i.e. all ages men and women) calculated in the "../Analysis.qmd"

AggregatePAFs_AllAges.csv
	Description - These are calculated aggregate PAFs for each cancer site divided by sex but not by age (i.e. all ages men and women) calculated in the "../Analysis.qmd"

PAF_by_riskfactor.csv
	Description - These are calculated PAFs by risk factor for each cancer site divided by sex and by age (i.e. split by 20-49, 50+ and men,women) calculated in the "../Analysis.qmd"

AggregatePAFs_BestEstimates.csv 
	Description - These are calculated aggregate PAFs for each cancer site divided by sex and by age (i.e. split by 20-49, 50+ and men,women) calculated in the "../Analysis.qmd"

PIF_allages.csv
	Description - These are calculated Population Impact Fraction (PIFs) for all of the increasing PAFs for each cancer site divided by sex but not by age (i.e. all ages men and women) calculated in the "../Analysis.qmd"

PIF.csv
	Description - These are calculated Population Impact Fraction (PIFs) for all of the increasing PAFs for each cancer site divided by sex and by age (i.e. split by 20-49, 50+ and men,women) calculated in the "../Analysis.qmd"
	
all_incidence_joinpoint.csv 
	Description - These are incidence numbers and rates for all cancers considered in the UK/US section of the early onset analysis. It is derived from "../Functions/initial_cancer_jp_generation.R"


**FOLDERS
PotentialExposures
	Description - These are data from expected exposures that are documented in "../Analysis.qmd"

Joinpoint_Results 
	Description - This is the output from JoinPoint trend analysis software, the joinpoint workplaces are saved within each folder
	Souce - https://surveillance.cancer.gov/joinpoint/
