#This code is to prepare the childhood BMI data from the Health Survey England for APC analysis using Joinpoint 

#Read in the data 
data_childhood_bmi <- suppressMessages(read_excel("../Data/HSE-2022-Overweight-and-obesity-tables.xlsx", sheet = "Table 16"))

colnames(data_childhood_bmi) <- c("group", data_childhood_bmi[3,-1])
data_childhood_bmi <- data_childhood_bmi[-(1:3),]

# Clean data
data_childhood_bmi <- data_childhood_bmi |>
  filter(!is.na(group)) |>
  filter(row_number()<40)

# Get sex groupings
childhood_bmi_groups <- data_childhood_bmi |>
  filter(is.na(`1995`)) |>
  filter(!grepl("[0-9]", group)) |>
  pull(group) |>
  unique()

# Get bmi age groupings
childhood_bmi_age_groups <- data_childhood_bmi |>
  filter(is.na(`1995`)) |>
  filter(grepl("[0-9]", group)) |>
  pull(group) |>
  unique()

# Add sex column
data_childhood_bmi$sex <- rep(childhood_bmi_groups, each = nrow(data_childhood_bmi)/length(childhood_bmi_groups))

# Remove sex indicator in group column
data_childhood_bmi <- data_childhood_bmi |>
  filter(group != sex)

# Now add in age groups
data_childhood_bmi$age_group <- rep(rep(childhood_bmi_age_groups, each = 4), length(childhood_bmi_groups))

# Now filter out so the group column is just level
data_childhood_bmi <- data_childhood_bmi |>
  filter(group != age_group) |>
  rename(level = group)

# Now get year columns and pivot longer into one year column
year_cols <- colnames(data_childhood_bmi)[grepl("[0-9]", colnames(data_childhood_bmi))]

data_childhood_bmi <- data_childhood_bmi |>
  pivot_longer(cols = all_of(year_cols))

# Now remove the unwanted years and ensure the value column is numeric
data_childhood_bmi <- data_childhood_bmi |>
  mutate(name = as.numeric(name)) |>
  filter(name <= 2019) |>
  mutate(value = as.numeric(value))


# Format the data to be able to put it in Joinpoint
data_childhood_bmi <- data_childhood_bmi |>
  rename(year = name) |>
  select (year, age_group, sex, level, value) |>
  arrange(age_group, sex, level)

#save the data for joinpoint
write.csv(data_childhood_bmi, "../Data/childhoodbmi_forjoinpoint.csv", row.names = F)
