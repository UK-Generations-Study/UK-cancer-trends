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
#
# test_difference - testing difference between stratified AAPCs (THIS IS ONLY IMPLEMENTED FOR SPECIFIC VARIABLES)

## OUTPUT
# gt table object to optionally edit manually/print

## FUNCTION
full_joinpoint_table <- function(data_apc, data_aapc, group_var, table_var, stratification_vars, test_difference = F){
  
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
      APC = format(round(APC, digits = 2), nsmall = 2, trim = T),
      CI.APC = paste0("(", format(round(APC.95..LCL, digits = 2), nsmall = 2, trim = T), ", ", format(round(APC.95..UCL, digits = 2), nsmall = 2, trim = T), ")"),
      P.Value.APC = if_else(P.Value < 0.0001, "<0.0001", as.character(sub("\\.?0+$", "", format(signif(P.Value, digits = 2), scientific = FALSE))))
      
    ) |>
    select(all_of(c(full_grouping_vars, "time.period.APC", "APC", "CI.APC", "P.Value.APC")))
  
  # Extract needed information out of data_aapc
  data_aapc_new <- data_aapc |>
    mutate(
      
      time.period.AAPC = paste0("(", Start.Obs, ", ", End.Obs, ")"),
      AAPC = format(round(AAPC, digits = 2), nsmall = 2, trim = T),
      CI.AAPC = paste0("(", format(round(AAPC.C.I..Low, digits = 2), nsmall = 2, trim = T), ", ", format(round(AAPC.C.I..High, digits = 2), nsmall = 2, trim = T), ")"),
      P.Value.AAPC = if_else(P.Value < 0.0001, "<0.0001", as.character(sub("\\.?0+$", "", format(signif(P.Value, digits = 2), scientific = FALSE))))
      
    ) |>
    select(all_of(c(full_grouping_vars, "time.period.AAPC", "AAPC", "CI.AAPC", "P.Value.AAPC")))
  
  if(test_difference){
    
    data_aapc_diff <- data_aapc |>
      dplyr::mutate(cancer = cancer_site,
                    agegrp = age_group,
                    JPmodel = Joinpoint.Model,
                    aapc_index = AAPC.Index,
                    start_obs = Start.Obs,
                    end_obs = End.Obs,
                    aapc = AAPC,
                    CIlow = AAPC.C.I..Low,
                    CIhigh = AAPC.C.I..High
      ) %>%
      dplyr::mutate(agegrp = case_when(agegrp == "20-49" ~ "u50",
                                       agegrp == "50+" ~ "o50",
                                       T ~ agegrp),
                    degfreedom = case_when( # This part is new - based on joinpoint definition of degrees of freedom
                      JPmodel != 0 ~ NA,
                      TRUE ~ end_obs - start_obs + 1 - 2
                    )
      ) %>% 
      dplyr::select(cancer, sex, agegrp, JPmodel, aapc_index, start_obs, end_obs, aapc, CIlow, CIhigh, degfreedom) |>
      process_data_new() |>
      compute_pvalues_N(N = 10000) |>
      mutate(
        
        p_value_one_sided = case_when(
          aapc_u50 > aapc_o50 ~ p_value/2,
          TRUE ~ 1-p_value/2
        ),
        
        p_value_one_sided = if_else(p_value_one_sided < 0.0001, "<0.0001", as.character(sub("\\.?0+$", "", format(signif(p_value_one_sided, digits = 2), scientific = FALSE))))
        
      ) |>
      select(cancer_site = cancer, sex, P.Value_one_sided_AAPC = p_value_one_sided)
    
    data_aapc <- merge(data_aapc_new, data_aapc_diff, by = c("cancer_site", "sex")) |>
      mutate(P.Value_one_sided_AAPC = if_else(age_group == "20-49", "-", P.Value_one_sided_AAPC))
  
  } else {
    
    data_aapc <- data_aapc_new
    
  }
  
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
        starts_with("time.period") ~ px(70),
        starts_with("CI") ~ px(80),
        "AAPC" ~ px(40),
        "APC" ~ px(40),
        starts_with("P.Value") ~ px(55),
        everything() ~ px(65)
      ) |>
      # Adding border between stratification_vars and AAPC, and AAPC and APC
      tab_style(
        style = cell_borders(
          sides = c("left"),
          weight = px(1)
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
      ) |>
      # LANCET FORMATTING
      tab_style(
        style = cell_text(font = "Times New Roman", size = px(8*1.333)),
        locations = cells_footnotes()
      ) |>
      tab_style(
        style = cell_text(font = "Times New Roman", size = px(8*1.333)),
        locations = cells_body(columns = everything())
      ) |>
      tab_style(
        style = cell_text(font = "Times New Roman", size = px(8*1.333), weight = "bold"),
        locations = cells_column_labels()
      ) |>
      tab_style(
        style = cell_text(font = "Times New Roman", size = px(10*1.333), weight = "bold"),
        locations = cells_title()
      ) |>
      tab_style(
        style = cell_text(font = "Times New Roman", size = px(8*1.333), weight = "bold"),
        locations = cells_column_spanners()
      ) |>
      tab_style(
        style = cell_text(font = "Times New Roman", size = px(8*1.333), weight = "bold"),
        locations = cells_row_groups(groups = everything())
      )
    
    # Changes dependent on testing for differences
    if(test_difference){
      data_complete_table <- data_complete_table |>
      cols_move_to_start(columns = c(stratification_vars, "time.period.AAPC","AAPC", "CI.AAPC", "P.Value.AAPC" , "P.Value_one_sided_AAPC", "time.period.APC","APC",  "CI.APC", "P.Value.APC" )) |>
      cols_label(
        P.Value_one_sided_AAPC ~ "P Value Difference"
      )
    }
    
    output[[table_value]] <- data_complete_table

  }
  
  # Output list
  return(output)
  
  
}
