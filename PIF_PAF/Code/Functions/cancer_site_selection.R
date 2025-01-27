### CANCER SITE SELECTION ###

data <- read.delim(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\PAF\UK-cancer-trends\PIF_PAF\Data\Joinpoint_Results\Cancerrates_All\joinpoint_cancerrates_all.Export.AAPC.txt)", sep = "\t")

data |>
  filter(age_group == "20-49") |>
  filter(Statistically.Significant..0.No..1.Yes. == 1) |>
  filter(AAPC > 0) |>
  View()
