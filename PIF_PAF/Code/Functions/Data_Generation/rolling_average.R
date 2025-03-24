## ROLLING AVERAGE CODE ##
# This code is designed to process the cleaned survey data and apply a rolling average to it to smooth out variability

## FUNCTION
rolling_average <- function(data){
  
  ## Packages needed
  necessary_packages <- c("dplyr", "slider")
  suppressMessages(
    for (p in necessary_packages) {
      if (!require(p, character.only = TRUE)){
        install.packages(p)
      }
      library(p, character.only = TRUE)
    }
  )
  
  # Check the needed columns are in the data
  if(!"value" %in% names(data) | !"year" %in% names(data)){
    stop("Needed columns are not in data")
  }
  
  # Group by other columns
  data <- data |>
    group_by(across(any_of(c("age_group", "sex", "level", "variable", "imd")))) |>
    arrange(year) |>
    mutate(
      
      # Group into sequential groups - rolling average should only be applied to sequential years
      chain = cumsum(c(1,diff(year) != 1))
      
    ) |>
    ungroup() |>
    group_by(across(any_of(c("age_group", "sex", "level", "variable", "chain", "imd")))) |>
    mutate(
      
      # Apply length 3 mean window - at endpoints just take endpoint and the one before/after
      value = case_when(
        n() > 2 ~ slider::slide_mean(value, before = 1, after = 1),
        TRUE ~ value)
      
    ) |>
    ungroup() |>
    select(-chain)
  
  # Output dataframe
  return(data)
  
}
