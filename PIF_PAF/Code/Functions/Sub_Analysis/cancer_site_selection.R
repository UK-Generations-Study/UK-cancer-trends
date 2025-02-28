### CANCER SITE SELECTION ###

# This code is designed to find the list of cancer sites that meet the criteria of having a significantly increasing AAPC in young people (20-49)

# Read in data
data <- read.delim(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\PAF\UK-cancer-trends\PIF_PAF\Data\Joinpoint_Results\Cancerrates_All\joinpoint_cancerrates_all.Export.AAPC.txt)", sep = "\t")

# Filter data by criteria and view
data |>
  filter(age_group == "20-49") |>
  filter(Statistically.Significant..0.No..1.Yes. == 1) |>
  filter(AAPC > 0) |>
  View()
