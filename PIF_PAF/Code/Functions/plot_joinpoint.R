### PLOT_JOINTPOINT FUNCTION ###
#
# This code is designed to create plots showing the fit of joinpoint models on a dataset

## INPUTS
#
# data - data.frame with columns:
#   year - year of datapoint
#   value - value of variable of interest at that point
#
# data.model - data.frame from APC analysis of joinpoint
#
# facet.x/facet.y - variables to optionally facet on
#
# colour_var - if plotting multiple categories and distinguishing by colour, variable name for this

## OUTPUT
# ggplot object to optionally edit manually/print

## MODEL
plot_joinpoint <- function(data, data.model, facet.x = character(0), facet.y = character(0), colour_var = NA){
  
  # Getting variable list to group by when formatting data
  group_by_vars <- c(facet.x, facet.y, colour_var)
  
  # If colour_var is NA - make a dummy variable
  if(is.na(colour_var)){
    
    data$colour_var_manual <- "PLACEMENT VALUE"
    colour_var <- "colour_var_manual"
    
  }
  
  # Get start/end years by group
  start_end_years <- data |>
    group_by(across(all_of(group_by_vars))) |>
    summarise(start.year.data = min(year),
              end.year.data = max(year)) |>
    suppressMessages() |>
    ungroup()
  
  # Formatting joinpoint data to get start and end points
  data.model <- data.model |>
    merge(start_end_years, by = group_by_vars) |>
    group_by(across(all_of(group_by_vars))) |>
    arrange(Segment) |>
    mutate(
      
      start.year = case_when(
        Segment == 0 ~ start.year.data,
        TRUE ~ Joinpoint),
      
      end.year = ifelse(Segment == max(Segment), end.year.data, lead(start.year))
      
    )
  
  # Creating start/end points for plotting
  data_joinpoint_plot <- data.model |>
    group_by(across(all_of(group_by_vars))) |>
    arrange(Segment) |>
    mutate(
      
      start.y = exp(Intercept.Estimate + start.year*Slope.Estimate),
      end.y = ifelse(Segment == max(Segment), exp(Intercept.Estimate + end.year*Slope.Estimate), lead(start.y))
      
    ) |>
    ungroup() |>
    select(all_of(c(group_by_vars, "start.year", "end.year", "start.y", "end.y"))) |>
    pivot_longer(cols = c("start.year", "end.year", "start.y", "end.y"), names_sep = "\\.", names_to = c("name", "test")) |>
    pivot_wider(names_from = "test", values_from = "value") |>
    unnest(c(year, y)) |>
    select(-name) |>
    unique()
  
  # Now start constructing plot
  plot <- ggplot(data, aes(x = year, y = value, colour = !!sym(colour_var))) +
    geom_point() +
    geom_line(data = data_joinpoint_plot, aes(x = year, y = y)) +
    theme_minimal() +
    theme(legend.position="bottom") +
    scale_y_log10(breaks = scales::breaks_log(n = 6)) +
    scale_x_continuous(
      breaks = seq(1995, 2019, by = 3),
      limits = c(1995, 2019)
    )+
    labs(
      x = "Year"
    ) +
    guides(
      linetype = "none"
    ) +
    geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
              colour = "black", fill = NA, inherit.aes = FALSE) +
    theme(strip.text = element_text(face = "bold"), 
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_grid(as.formula(paste0(facet.y, " ~ ", facet.x)))
  
  # If colour_var is NA - remove legend from plot
  if(colour_var == "colour_var_manual"){
    
    plot <- plot |>
      theme(
        legend.position = "none"
      )
    
  }
  
  
  # Return plot
  return(plot)
  
}