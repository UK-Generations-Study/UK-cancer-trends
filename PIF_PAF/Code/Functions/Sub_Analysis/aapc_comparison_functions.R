

# MB notes - for global switch cancer and country in pivoting. In global country is ID column and in uk/us cancer is ID. 






## FUNCTIONS FOR MARTINA


# Z-test

z_test <- function(data) {
  if(length(unique(lapply(list(beta1, beta2, sd2, sd2), length)))>1){
    stop("Improper Input")
  }
  
  p_values <- numeric(length(beta1))
  
  for(i in seq_along(beta1)){
    
    z_stat <- (beta1[i] - beta2[i])/sqrt(sd1[i]^2 + sd2[i]^2)
    p_values[i] <- 2*(1-pnorm(abs(z_stat)))
    
  }
  
  p_values <- signif(p_values, 2)
  return(p_values)
  
}


# Processing Data
process_data_new <- function(data) {
  
  data %>% 
    pivot_wider(names_from = agegrp,
                values_from = c(aapc, CIlow, CIhigh, JPmodel, degfreedom),
                id_cols = c(sex, cancer)
    ) %>% 
    dplyr::select(cancer,
                  sex,
                  JPmodel_u50, JPmodel_o50,
                  aapc_u50, CIlow_u50, CIhigh_u50,
                  aapc_o50, CIlow_o50, CIhigh_o50,
                  degfreedom_u50, degfreedom_o50
    ) %>% 
    mutate(logAAPC_u50 = log((aapc_u50/100)+1),
           logCIlow_u50 = log((CIlow_u50/100)+1),
           logCIhigh_u50 = log((CIhigh_u50/100)+1),
           logAAPC_o50 = log((aapc_o50/100)+1),
           logCIlow_o50 = log((CIlow_o50/100)+1),
           logCIhigh_o50 = log((CIhigh_o50/100)+1),
           logVAR_u50 = case_when(
             JPmodel_u50 != 0 ~ ((logCIhigh_u50 - logCIlow_u50) / (2 * 1.96))^2,
             TRUE ~ ((logCIhigh_u50 - logCIlow_u50) / (2 * qt(0.975, df = degfreedom_u50)))^2 # When 0 joins, it is based on a t distribution.
           ),
           logVAR_o50 = case_when(
             JPmodel_o50 != 0 ~ ((logCIhigh_o50 - logCIlow_o50) / (2 * 1.96))^2,
             TRUE ~ ((logCIhigh_o50 - logCIlow_o50) / (2 * qt(0.975, df = degfreedom_o50)))^2 # When 0 joins, it is based on a t distribution.
           )
    ) %>% 
    select(cancer, 
           sex,
           JPmodel_u50, JPmodel_o50,
           aapc_u50, CIlow_u50, CIhigh_u50,
           aapc_o50, CIlow_o50, CIhigh_o50,
           logAAPC_u50, logCIlow_u50, logCIhigh_u50, logVAR_u50,
           logAAPC_o50, logCIlow_o50, logCIhigh_o50, logVAR_o50,
           degfreedom_u50, degfreedom_o50
           
           
    ) 
}

# Master p value generation
compute_pvalues_N <- function(data, N){
  
  data_t_t <- data |> # Both 0 joins, t-t test
    filter(JPmodel_u50 == 0 & JPmodel_o50 == 0) |>
    mutate(p_value = t_t_ineq_test_batch(beta1 = logAAPC_u50, beta2 = logAAPC_o50, sd1 = sqrt(logVAR_u50), sd2 = sqrt(logVAR_o50), d1 = degfreedom_u50, d2 = degfreedom_o50, N = N))
  
  data_t_n <- data |> # One 0 joins, t-norm test
    filter(JPmodel_u50 == 0 & JPmodel_o50 != 0) |>
    mutate(p_value = normal_t_ineq_test_batch(betanorm = logAAPC_o50, betat = logAAPC_u50, sdnorm = sqrt(logVAR_o50), sdt = sqrt(logVAR_u50), d = degfreedom_u50, N = N))
  
  data_n_t <- data |> # One 0 joins, t-norm test
    filter(JPmodel_u50 != 0 & JPmodel_o50 == 0) |>
    mutate(p_value = normal_t_ineq_test_batch(betanorm = logAAPC_u50, betat = logAAPC_o50, sdnorm = sqrt(logVAR_u50), sdt = sqrt(logVAR_o50), d = degfreedom_o50, N = N))
  
  data_n_n <- data |> # Both >0 joins, z test
    filter(JPmodel_u50 != 0 & JPmodel_o50 != 0) |>
    mutate(p_value = z_test(beta1 = logAAPC_u50, beta2 = logAAPC_o50, sd1 = sqrt(logVAR_u50), sd2 = sqrt(logVAR_o50)))
  
  data_else <- data |> # If NA joins, cannot compare but data should be preserved.
    filter(is.na(JPmodel_u50) | is.na(JPmodel_o50)) |>
    mutate(p_value = NA)
  
  output_data <- data_t_t |>
    rbind(data_t_n) |>
    rbind(data_n_t) |>
    rbind(data_n_n) |>
    rbind(data_else) |>
    arrange(desc(aapc_u50))
  
  return(output_data)
  
  
}


# Normal vs T dist test
normal_t_ineq_test_batch <- function(betanorm, betat, sdnorm, sdt, d, N){
  
  if(length(unique(lapply(list(betanorm, betat, sdnorm, sdt, d), length))) > 1){
    stop("Improper Input")
  }
  
  # Initialise p value vector
  p_values <- numeric(length(betanorm))
  
  for(i in seq_along(betanorm)){
    
    # Generate rvs
    norm_values <- rnorm(N)
    t_values <- rt(N, d[i])
    
    # Get value from mixture dist
    diff <- betanorm[i] - betat[i] - sdnorm[i]*norm_values + sdt[i]*t_values
    
    # Prob dist > 0
    p1 <- sum(diff>0)/N
    
    # Prob dist != 0
    p_values[i] <- 2*min(p1, 1-p1)
    
  }
  
  return(p_values)
  
}

# T vs T test
t_t_ineq_test_batch <- function(beta1, beta2, sd1, sd2, d1, d2, N){
  
  if(length(unique(lapply(list(beta1, beta2, sd1, sd2, d1, d2), length)))>1){
    stop("Improper Input")
  }
  
  # Initialise p value vector
  p_values <- numeric(length(beta1))
  
  for(i in seq_along(beta1)){
    
    # Generate rvs
    t1_values <- rt(N, d1[i])
    t2_values <- rt(N, d2[i])
    
    # Get value from mixture dist
    diff <- beta1[i] - beta2[i] - sd1[i]*t1_values + sd2[i]*t2_values
    
    # Prob dist > 0
    p1 <- sum(diff>0)/N
    
    # Prob dist != 0
    p_values[i] <- 2*min(p1, 1-p1)
    
  }
  
  return(p_values)
  
}

# Vectorised z test
z_test <- function(beta1, beta2, sd1, sd2){
  
  if(length(unique(lapply(list(beta1, beta2, sd2, sd2), length)))>1){
    stop("Improper Input")
  }
  
  p_values <- numeric(length(beta1))
  
  for(i in seq_along(beta1)){
    
    z_stat <- (beta1[i] - beta2[i])/sqrt(sd1[i]^2 + sd2[i]^2)
    p_values[i] <- 2*(1-pnorm(abs(z_stat)))
    
  }
  
  return(p_values)
  
}