# This is the aggregate PAF calculation function 

# Function: This function iterates through a column to add the PAFs by cancer site to create an aggregate PAF. This method follows the methodology for aggregate PAFs from Brown et. al. 

# Inputs: 
    # a data frame that has the paf by cancer site and risk factor for the aforementioned risk factors. 
    # This can be calculated using the function "paf_byrf_function.R" that can be found in this same folder. 
    #Thisfunction is iterated on the column itself  


# Outputs:
    # This function will iterate through the column and add the pafs by risk factor and return a singlular cummulative paf

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