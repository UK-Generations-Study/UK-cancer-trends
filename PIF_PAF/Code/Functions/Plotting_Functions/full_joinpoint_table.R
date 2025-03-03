### FULL JOINPOINT TABLE FUNCTION ###

# This function is designed to produce a joinpoint summary table

## INPUTS
#
# data_apc - APC output from joinpoint
#
# data_aapc - AAPC output from joinpoint
#
# group_var - variable to group by within tables
#
# table_var - variable to plot tables by, with a seperate table made for each level of the variable
#
# straification_vars - remaining variables that the trends are stratified for

## OUTPUT
# gt table object to optionally edit manually/print

## FUNCTION
full_joinpoint_table <- function(data_apc, data_aapc, group_var, table_var, stratification_vars){
  
  # Combine all variables to group by when creating summary statistics
  full_grouping_vars <- c(group_var, table_var, stratification_vars)
  
  # Extract needed information out of data_apc
  data_apc <- data_apc |> 
    group_by(across(all_of(full_grouping_vars))) |>
    # Filtering to last segment
    filter(Segment == max(Segment)) |>
    ungroup() |>
    mutate(
      
      time.period.APC = paste0("(", Segment.Start, ", ", Segment.End, ")"),
      APC = round(APC, digits = 2),
      CI.APC = paste0("(", round(APC.95..LCL, digits = 2), ", ", round(APC.95..UCL, digits = 2), ")"),
      P.Value.APC = if_else(P.Value < 0.01, "<0.01", as.character(round(P.Value, digits = 2)))
      
    ) |>
    select(all_of(c(full_grouping_vars, "time.period.APC", "APC", "CI.APC", "P.Value.APC")))
  
  # Extract needed information out of data_aapc
  data_aapc <- data_aapc |>
    mutate(
      
      time.period.AAPC = paste0("(", Start.Obs, ", ", End.Obs, ")"),
      AAPC = round(AAPC, digits = 2),
      CI.AAPC = paste0("(", round(AAPC.C.I..Low, digits = 2), ", ", round(AAPC.C.I..High, digits = 2), ")"),
      P.Value.AAPC = if_else(P.Value < 0.01, "<0.01", as.character(round(P.Value, digits = 2)))
      
    ) |>
    select(all_of(c(full_grouping_vars, "time.period.AAPC", "AAPC", "CI.AAPC", "P.Value.AAPC")))
  
  # Combine together
  data_complete <- merge(data_aapc, data_apc, by = full_grouping_vars)
  
  # Initialise output list
  output <- list()
  
  # Loop through table_var and create table for that
  for(table_value in unique(data_complete[[table_var]])){

    data_complete_table <- data_complete |>
      filter(!!sym(table_var) == table_value) |>
      select(-sym(table_var)) |>
      arrange(across(all_of(stratification_vars))) |>
      gt(groupname_col = group_var) |>
      # Make column header bold
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) |>
      # Formatting group rows
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = "lightgray"),
          cell_borders(sides = "top"),
          cell_borders(sides = "bottom")
        ),
        locations = cells_row_groups(groups = everything())
      ) |>
      # Adding spanners to differentiate between AAPC and APC analysis
      tab_spanner(
        id = "AAPC_spanner",
        label = "AAPC",
        columns = c("AAPC", ends_with("AAPC"))
      ) |>
      tab_spanner(
        id = "APC_spanner",
        label = "Most Recent APC",
        columns = c("APC", ends_with(".APC"))
      ) |>
      # Changing column labels to pretty labels
      cols_label(
        time.period.AAPC = "Time Period",
        CI.AAPC = "CI 95%",
        P.Value.AAPC = "P Value",
        time.period.APC = "Time Period",
        CI.APC = "CI 95%",
        P.Value.APC = "P Value",
      ) |>
      # Reodering columns
      cols_move_to_start(columns = c(stratification_vars, "time.period.AAPC","AAPC", "CI.AAPC", "P.Value.AAPC" , "time.period.APC","APC",  "CI.APC", "P.Value.APC" )) |>
      cols_align(
        columns = everything(),
        align = "right"
      ) |>
      cols_align(
        columns = stratification_vars,
        align = "center"
      ) |>
      # Manually assigning column widths
      cols_width(
        starts_with("time.period") ~ px(120),
        starts_with("CI") ~ px(110),
        "AAPC" ~ px(70),
        "APC" ~ px(70),
        starts_with("P.Value") ~ px(70),
        everything() ~ px(100)
      ) |>
      # Adding border between stratification_vars and AAPC, and AAPC and APC
      tab_style(
        style = cell_borders(
          sides = c("left"),
          weight = px(2)
        ),
        locations = cells_body(
          columns = starts_with("time.period")
        )
      ) |>
      # Adding footnotes defining AAPC and APC
      tab_footnote(
        footnote = "Average Annual Percentage Change",
        locations = cells_column_labels(columns = AAPC)
      ) |>
      tab_footnote(
        footnote = "Annual Percentage Change",
        locations = cells_column_labels(columns = APC)
      )
    
    output[[table_value]] <- data_complete_table

  }
  
  # Output list
  return(output)
  
  
}
