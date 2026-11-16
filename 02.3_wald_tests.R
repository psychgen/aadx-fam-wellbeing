library(tidyverse)

load("./output/results/results_h4.RData")
load("./output/results/results_h5.RData")
load("./output/results/results_h6.RData")
load("./output/results/results_h7.RData")

# Specifying function for performing wald test

wald_test <- function(data) {
  data |> 
    
    # Filter out only aadx terms
    filter(grepl("Age at autism diagnosis", term))  |> 
    group_by(person, Outcome, term) |>
    summarise(
      
      # Extract coefficients and standard errors for both models
      coef_unadj = results[model == "Unadjusted"],
      se_unadj = se[model == "Unadjusted"],
      coef_adj = results[model == "Adjusted"],
      se_adj = se[model == "Adjusted"],
      perc_diff = ((coef_unadj-coef_adj)/coef_unadj)*100,
      
      # Calculate Wald statistic
      wald_stat = ((coef_adj - coef_unadj) / sqrt(se_unadj^2 + se_adj^2))^2,
      
      # Calculate p-value
      p_value = 1 - pchisq(wald_stat, df = 1),
      .groups = 'drop'
    ) |> 
    ungroup() |> 
    group_by(person, term) |> 
    mutate(adjusted_p = p.adjust(p_value, method = "BH"),
           .groups = 'drop') |> 
    ungroup() |> 
    dplyr::select(term, person, Outcome, coef_unadj, coef_adj, perc_diff, wald_stat, p_value, adjusted_p)
}


#######################
# Wald test childhood #
#######################

wald_results_h4 <- wald_test(results_h4)

wald_results_h5 <- wald_test(results_h5)

wald_results_h6 <- wald_test(results_h6)

wald_results_h7 <- wald_test(results_h7)

save(wald_results_h4, file = "./output/wald_results/wald_results_h4.RData")

save(wald_results_h5, file = "./output/wald_results/wald_results_h5.RData")

save(wald_results_h6, file = "./output/wald_results/wald_results_h6.RData")

save(wald_results_h7, file = "./output/wald_results/wald_results_h7.RData")

