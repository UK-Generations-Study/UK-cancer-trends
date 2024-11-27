library(dplyr)
library(ggplot2)
library(purrr)
library(cowplot)
library(tidyr)

data_paf_comparison <- read.csv(r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\PAF\UK-cancer-trends\PIF_PAF\Data\paf_comparison.csv)") |>
  # TEMPORARY CODE
  mutate(Cancer_sites = if_else(Cancer_sites == "Breast ", "Breast", Cancer_sites)) |>
  filter(!(Cancer_sites == "Breast" & sex == "Men"))

# Calculate p-values
# Now compare empirically between the years
data_complete_paf_test <- data_paf_comparison |>
  group_by(Cancer_sites, age_group, sex, variable) |>
  mutate(

    type = case_when(
      year == min(year) ~ "Past",
      year == max(year) ~ "Future",
      TRUE ~ "Current"
    ),

  ) |>
  ungroup() |>
  group_by(Cancer_sites, age_group, sex, variable, type) |>
  mutate(

    in_type_no = row_number()

  ) |>
  ungroup() |>
  pivot_wider(id_cols = c("Cancer_sites", "age_group", "sex", "variable", "in_type_no"), names_from = type, values_from = PAF) |>
  group_by(Cancer_sites, age_group, sex, variable) |>
  summarise(PAF_diff_Past = sum(Past > Current)/n(),
            PAF_diff_Future = sum(Current > Future)/n()
  )


# Calculate mean and CI for each point
data_complete_paf_plot <- data_paf_comparison |>
  group_by(Cancer_sites, age_group, sex, variable) |>
  mutate(
    
    year_cat = case_when(
      year == min(year) ~ "Past",
      year == max(year) ~ "Future",
      TRUE ~ "Current"
    )
    
  ) |>
  ungroup() |>
  group_by(Cancer_sites, age_group, sex, variable, year_cat) |>
  summarise(
    
    mean = mean(PAF),
    
    CI_low = quantile(PAF, 0.025),
    
    CI_high = quantile(PAF, 0.975),
    
    category = paste0(variable[1], " | ", sex[1], " | ", age_group[1]),
    
  ) |>
  merge(data_complete_paf_test)

pdf(file=r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\PAF\UK-cancer-trends\PIF_PAF\Output\test_past.pdf)", width = 15, height = 40)

plot <- data_complete_paf_plot |>
  group_by(year_cat) |>
  arrange(variable, Cancer_sites, sex, age_group) |>
  forestplot(
    labeltext = c(variable, Cancer_sites, sex, age_group, PAF_diff_Past, PAF_diff_Future),
    boxsize = 0.2,
    mean = mean,
    lower = CI_low,
    upper = CI_high
  ) |>
  fp_set_style(box = "black",
               line = "black") |>
  fp_add_header(variable = c("", "Variable"),
                Cancer_sites = c("", "Cancer Site"),
                sex = c("", "Sex"),
                age_group = c("", "Age Group"),
                PAF_diff_Past = c("Past vs Present", "P Value"),
                PAF_diff_Future = c("Present vs Future", "P Value")) |>
  fp_decorate_graph(graph.pos = 5) |>
  fp_set_zebra_style("#EFEFEF") |>
  fp_set_style(box = c("red", "blue", "yellow") |> lapply(function(x) gpar(fill = x, col = "#555555")),
               default = gpar(vertices = TRUE))

start <- 3
for(var in sort(unique(data_complete_paf_plot$variable))){
  
  # Add row
  plot <- plot |>
    fp_insert_row(variable = var,
                  position = start,
                  Cancer_sites = NULL,
                  sex = NULL,
                  age_group = NULL,
                  PAF_diff_Past = NULL,
                  PAF_diff_Future = NULL,
                  mean = NULL,
                  lower = NULL,
                  upper = NULL,
                  is.summary = TRUE,
                  boxsize = NA
    )
  
  # Count number of instances
  no_instances <- sum(data_complete_paf_plot$variable == var)/3
  
  # Now add to start
  start <- start + no_instances + 1
  
}

# Plot
plot

dev.off()



















# # Plotting past-current PAFs
# data_paf_plot <- data_paf_comparison |>
#   group_by(Cancer_sites, age_group, sex, variable) |>
#   mutate(
#     
#     year_cat = case_when(
#       year == min(year) ~ "Past",
#       year == max(year) ~ "Future",
#       TRUE ~ "Current"
#     )
#     
#   ) |>
#   ungroup() |>
#   mutate(
#     
#     category = paste0(variable, " | ", sex, " | ", age_group)
#     
#   ) |>
#   merge(data_complete_paf_test, all.x = T)
# 
# plot <- data_paf_plot |>
#   filter(year_cat != "Future") |>
#   ggplot(aes(x = PAF)) +
#   geom_density(aes(fill = year_cat), alpha = 0.3, colour = "black") +
#   geom_label(aes(label = PAF_diff_Past, x = Inf, y = Inf),
#              hjust = 1, vjust = 1, fill = "white", color = "black", size = 3,
#              position = position_nudge(y = -0.02)) +
#   theme_minimal() +
#   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
#             colour = "black", fill = NA, inherit.aes = FALSE) +
#   theme(strip.text = element_text(face = "bold")) +
#   facet_grid(Cancer_sites~category)
# 
# 
# 
# 
# 
# ggsave(plot, filename = r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\PAF\UK-cancer-trends\PIF_PAF\Output\test_past.png)", bg = "white", width = 30, height = 16)
# 
# 
# plot <- data_paf_plot |>
#   filter(year_cat != "Past") |>
#   ggplot(aes(x = PAF)) +
#   geom_density(aes(fill = year_cat), alpha = 0.3, colour = "black") +
#   geom_label(aes(label = PAF_diff_Future, x = Inf, y = Inf),
#              hjust = 1, vjust = 1, fill = "white", color = "black", size = 3,
#              position = position_nudge(y = -0.02)) +
#   theme_minimal() +
#   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
#             colour = "black", fill = NA, inherit.aes = FALSE) +
#   theme(strip.text = element_text(face = "bold")) +
#   facet_grid(Cancer_sites~category)
# 
# 
# 
# 
# 
# ggsave(plot, filename = r"(C:\Users\rfrost\OneDrive - The Institute of Cancer Research\Documents\PAF\UK-cancer-trends\PIF_PAF\Output\test_future.png)", bg = "white", width = 30, height = 16)
