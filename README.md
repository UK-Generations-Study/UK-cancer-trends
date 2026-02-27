# Temporal Trends in Behavioural Risk Factors for Early-Onset Cancer in England

  Code and publicly available data for:

  > García-Closas M, Richards Z, Frost R, Gunter MJ, Berrington de Gonzalez A.
  > *Temporal Trends in Behavioural Risk Factors for Cancers with Rising Incidence in Younger Adults: An Analysis of Population-Based Data in England.*
  > medRxiv 2025. https://doi.org/10.1101/2025.08.21.25333984

  ## Overview

  This repository contains the code and publicly available data for a
  descriptive study examining behavioural risk factors (smoking, alcohol, BMI,
  physical activity, diet) associated with 11 cancer sites showing increasing
  incidence in adults aged 20–49 in England (2001–2019). The analysis
  calculates Population Attributable Fractions (PAF) and evaluates their temporal trends using Joinpoint regression.

  ## Key Files
  ### [🔗 PIF_PAF/Code/Final_Figures.html](https://uk-generations-study.github.io/UK-cancer-trends/PIF_PAF/Code/Final_Figures.html)
  * Reproduction of manuscript figures and tables

  ## Repository Structure

```text
PIF_PAF/
├── Code/                                           # Analysis scripts (Quarto)
│   ├── Final_Figures.qmd                           # Reproduces all manuscript figures and tables
│   ├── RF_Data_Generation.qmd                      # Risk factor data preparation
│   └── Functions/                                  # Functions used in analysis
│       ├── Archive/                                # Archived functions
│       ├── Data_Generation/                        # HSE/NDNS survey data processing and NDRS data processing
│       ├── PAF_Calculations/                       # PAF computation functions
│       ├── Plotting_Functions/                     # Figure generation utilities
│       └── Sub_Analysis/                           # Supporting statistical routines
│   └── Initial_Data_Evaluation/                    # Initial data explorations
│
├── Data/                                           # Input and derived datasets
│   ├── README_External_Data.txt                    # Publicly available data sources
│   ├── README_Internal_Data.txt                    # UKDS-restricted data sources
│   └── [derived CSV files]                         # Pre-computed outputs included in repository
│
├── Documentation/                                  # Supporting documentation and metadata
│   ├── hse_variable_documentation.yaml             # Variable documentation for HSE data
│   ├── hse_variable_documentation_ages_all.yaml    # Further variable documentation for HSE data
│   ├── WCRF_IARC_Risk_Factor.xlsx                  # RR estimates used in analysis
│   └── UKDS_Dictionary.csv                         # Dictionary of UKDS IDs
│
└── Output/                                         # Output tables and figures
```

## Data Availability

### Included in This Repository

(Located in `PIF_PAF/Data/`)

* Cancer incidence data: National Disease Registration Service (NDRS), England
* Derived PAF estimates and aggregated risk factor summaries

### Restricted Data (Not Included)

Access to the following datasets requires a UK Data Service (UKDS) End User Licence:

* Health Survey for England (HSE), 1995–2019
* National Diet and Nutrition Survey (NDNS)
* General Household Survey (GHS), 2005

See `PIF_PAF/Data/README_External_Data.txt` for UKDS study numbers and dates of access.

## Software Requirements

* R (version 4.4.1 or later recommended)
* Joinpoint Regression Software (National Cancer Institute, version 5.3)

  * Required for trend analysis
  * Available as a free download: [https://surveillance.cancer.gov/joinpoint/](https://surveillance.cancer.gov/joinpoint/)

## Citation

If you use this repository in your work, please cite:

  > García-Closas M, Richards Z, Frost R, Gunter MJ, Berrington de Gonzalez A.
  > *UK-cancer-trends*.
  > (2025).
  > https://doi.org/10.5281/zenodo.18789718


## License

This project is licensed under the MIT License.

