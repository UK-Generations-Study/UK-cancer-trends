

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
      
      AAPC = round(AAPC, digits = 2),
      CI.AAPC = paste0("(", round(AAPC.C.I..Low, digits = 2), ", ", round(AAPC.C.I..High, digits = 2), ")"),
      P.Value.AAPC = if_else(P.Value < 0.01, "<0.01", as.character(round(P.Value, digits = 2)))
      
    ) |>
    select(all_of(c(full_grouping_vars, "AAPC", "CI.AAPC", "P.Value.AAPC")))
  
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
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = "lightgray"),
          cell_borders(sides = "top"),
          cell_borders(sides = "bottom")
        ),
        locations = cells_row_groups(groups = everything())
      ) |>
      tab_spanner(
        id = "AAPC_spanner",
        label = "AAPC",
        columns = ends_with("AAPC")
      ) |>
      tab_spanner(
        id = "APC_spanner",
        label = "Most Recent APC",
        columns = c("APC", ends_with(".APC"))
      ) |>
      cols_label(
        CI.AAPC = "CI 95%",
        P.Value.AAPC = "P Value",
        time.period.APC = "Time Period",
        CI.APC = "CI 95%",
        P.Value.APC = "P Value",
      )

    output[[table_value]] <- data_complete_table

  }
  
  # Output list
  return(output)
  
  
}

# test <- full_joinpoint_table(data_apc, data_aapc, group_var, table_var, stratification_vars)
# 
# for(i in names(test)){
#   
#   test[[i]] <- test[[i]] |>
#     cols_label(
#       age_group = "Age Group"
#     )
#   
#   print(test[[i]])
#     
# }
