README - Position Paper Data sourcing.

Cancer
- England/Wales Data sourced from source 1.
- Scotland Data sourced from source 1.
- N. Ireland Data sourced from source 1.

BMI 
- England Data sourced from source 1, Overweight and obesity, Table 8. Years extracted were 2003-2019 due to concerns about comparing other years due to methodology.
- England Children Data sourced from source 2, Overweight and obesity, Table 16. Years extracted were 2003-2019 due to concerns about comparing other years due to methodology.
- Scotland Data sourced from source 1, Overweight section. Full years extracted as no change in methodology noted.
- Wales Data sourced from source 1, Body mass index section. Year range taken as 2016/17 to 2019/20 as concerns about differing methodology from that point onwards.
- N. Ireland Data sourced from source 1, Health Survey NI Trend Tables, Table BMI - Adults, Sub-tables Obese & Overweight. Full years extracted as no change in methodology noted.

BMI - Obesity
- Scotland Data sourced from source 1, Obese section. Full years extracted as no change in methodology noted.
- N. Ireland Data sourced from source 1, Health Survey NI Trend Tables, Table BMI - Adults, Sub-tables Obese. Full years extracted as no change in methodology noted.

BMI - Children [An alternative children BMI source was desired]
- England Data sourced from the National Child Measurement Programme, Table 2 https://digital.nhs.uk/data-and-information/publications/statistical/national-child-measurement-programme/2022-23-school-year#resources
	- NOTE - Data from 2020 was noted as weighted due to a small sample size, but no indication of lack of ability to compare between years was given.

Alcohol
- England Data sourced from source 1, Adults' health-related behaviours, Table 15. Removed year 2021 due to differing methodology.
- Scotland Data sourced from source 1, Alcohol consumption (guidelines) section. Full years extracted as no change in methodology noted.
- Wales Data sourced from source 1, Alcohol section. Year range taken as 2016/17 to 2019/20 as concerns about differing methodology from that point onwards.
- N. Ireland Data sourced from source 1, Health Survey NI Trend Tables, Table "Alcohol Prevalance and Limits", Sub-tables Drinking prevalence by sex and age-group. Full years extracted as no change in methodology noted.

Alcohol_Alt
- N. Ireland Data was sourced from source 1, Table "Alcohol Prevalance and Limits", Sub-tables Drinking above weekly limits by sex and age-group. Years were removed when the question was not asked.

Smoking
- England Data sourced from source 1, Adults' health-related behaviours,  Table 6. Years extracted were 2003-2019 due to concerns about comparing other years due to methodology.
- Scotland Data sourced from source 1, Smoking status section. Full years extracted as no change in methodology noted.
- Wales Data sourced from source 1, Smoking and e-cigarette use section. Year range taken as 2016/17 to 2019/20 as concerns about differing methodology from that point onwards.
- N. Ireland Data sourced from source 1, Table "Cigarette Smoking", Sub-tables Cigarette smoking by sex and age-group. Full years extracted as no change in methodology noted. Year 2020/21 removed from Men and Women as not enough data.

E-Cigarettes
- England Data sourced from source 1, Adults' health-related behaviours,  Table 9. Years extracted were 2016-2019 due to concerns about comparing other years due to methodology.
- Scotland Data sourced from source 1, E-cigarette or vaping device section. Full years extracted as no change in methodology noted.
- Wales Data sourced from source 1, Smoking and e-cigarette use section. Year range taken as 2016/17 to 2019/20 as concerns about differing methodology from that point onwards. Added dummy in the first column to maintain structure continuity.
- N. Ireland Data sourced from source 1, Table "Electronic Cigarettes", Sub-tables Currently using electronic cigarettes by sex and age-group. Full years extracted as no change in methodology noted. Year 2020/21 removed from Men and Women as not enough data.
- Great Britain Data sourced from https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/drugusealcoholandsmoking/datasets/ecigaretteuseingreatbritain, Table 1

Activity
- England Data sourced from source 1, Physical activity, Table 5. Years extracted were 2016-2019 due to concerns about comparing other years due to methodology.
- Scotland Data sourced from source 1, Summary activity levels section. Full years extracted as no change in methodology noted.
- Wales Data sourced from source 1, Physical activity section. Year range taken as 2016/17 to 2019/20 as concerns about differing methodology from that point onwards.
- N. Ireland Data sourced from source 1, Table "Physical Activity". Full years extracted as no change in methodology noted. Years removed where question not asked.

Activity_Alt
- England Data sourced from source 3. Table 1 contains categories for all people, Table 2 stratifies by sex and age and measures % Active.

Age at First Birth
- UK Data sourced from source 2 which contained UK wide data. Continuous year range selected.

Parity
- England and Wales data sourced from source 1, Table 3.

Fertility
- England and Wales data sourced from source 3 for parity, Table 2.

Population
- England and Wales data sourced from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales, Table 9.

HRT
- England data sourced from source 2, Table "National_Population"

Diabetes
- England data sourced from source 1, Adult health, Table 3. Years extracted were 2003-2019 due to concerns about comparing other years due to methodology.
- Scotland data sourced from source 1, Doctor diagnosed diabetes section. Should be noted that pregnant women were deducted from this count, which may not be the case for other sources.
- N. Ireland data sourced from source 1. First year selected 2007/08 as changes to register definitions before then.

Screening
- Breast data https://fingertips.phe.org.uk/search/screening#page/4/gid/1938132830/pat/159/par/K02000001/ati/15/are/E92000001/iid/91340/age/265/sex/2/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1
	-Uptake data was used as coverage data was discontinuous. Could use coverage data up to 2021/22.
- Bowel data https://fingertips.phe.org.uk/search/screening#page/4/gid/1938133365/pat/159/par/K02000001/ati/15/are/E92000001/iid/92600/age/280/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1
- Cervical data https://digital.nhs.uk/data-and-information/publications/statistical/cervical-screening-annual/england-2022-2023#resources, Table 1

Cancer_Projection_Data(_All)
- UK wide data from Globocan

Cancer_Incident/Mortality_Data(_All)
- UK wide data edited from Globocan output for Joinpoint input

Cancer_Incident/Mortality_Joinpoint(_All)
- UK wide data outputted from Joinpoint

Cancer_Projection_Data_CRUK
- UK wide data from CRUK
- https://www.cancerresearchuk.org/health-professional/cancer-statistics/incidence/all-cancers-combined#heading-Two
- https://www.cancerresearchuk.org/health-professional/cancer-statistics/mortality/all-cancers-combined#heading-Three

Cancer_Incident/Mortality_Data_CRUK
- UK wide data edited from CRUK output for Joinpoint input

Cancer_Incident/Mortality_Joinpoint_CRUK
- UK wide data outputted from Joinpoint