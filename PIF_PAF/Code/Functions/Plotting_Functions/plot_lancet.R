theme_minimal_lancet <- function(){
  
  theme_minimal(base_size = 8, base_family = "Times New Roman") +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 10, face = "bold"),
      plot.caption = element_text(size = 10)
    )
  
}