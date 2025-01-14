# This is the aggregate PAF calculation function 

# Function: 

# Inputs:

# Outputs:

calculate_cumulative_paf <- function(paf_values) {
  #setting the base values    
  cumulative_paf <- 0
  remaining_whole <- 1
  
  for (paf in paf_values) {
    if (!is.na(paf)) {
      cumulative_paf <- cumulative_paf + paf * remaining_whole
      remaining_whole <- remaining_whole * (1 - paf)
    }
  }
  
  return(cumulative_paf)
}