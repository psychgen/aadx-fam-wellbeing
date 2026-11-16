library(tidyverse)
library(MASS)  # For ordinal logistic regression
library(miceadds)
library(mice)
library(broom)
library(mitools)
library(paletteer)

load("./data/MI_datlist_new.RData")
load("./data/MI_datlist_siblings.RData")

source("./scripts/00_replace_terms.R")
source("./scripts/02.1_analysis_covs.R")
source("./scripts/02.2_analysis_functions.R")


# Create overview table
source("./scripts/02.3_overview_table.R")

# Create descriptive statistics
source("./scripts/02.4_descriptive_statistics.R")

######
# H1 #
######

# Specific regression function for H1
regress_exposure_univariate <- function(datasets, outcome, hypothesis, covariates) {
  
  pooled_results_univariate <- map(covariates, function(covariate) {
    formula <- as.formula(paste(outcome, covariate, sep = " ~ "))
    results <- map(datasets, ~ run_regression(.x, formula))
    pooled_result <- pool_results(results, hypothesis, covariate, outcome)
  })
  
  pooled_results_univariate <- bind_rows(pooled_results_univariate)
  
  pooled_results_univariate$Model <- "Simple regression"
  
  formula <- as.formula(paste(outcome, paste(covariates, collapse = " + "), sep = " ~ "))
  
  # For each dataset in datlist, run the regression, save results to list
  results <- map(datasets, ~ run_regression(.x, formula))
  
  # Pool the results of each analysis
  pooled_results_multivariate <- pool_results(results, hypothesis, covariates, outcome)
  
  pooled_results_multivariate$Model <- "Multiple regression"
  
  pooled_results <- bind_rows(list(univariate = pooled_results_univariate, multivariate = pooled_results_multivariate))
  
  return(pooled_results)
}

# Specific regression function for H1
regress_exposure <- function(datasets, outcome, hypothesis, covariates) {
  
  # Define formula
  formula <- as.formula(paste(outcome, paste(covariates, collapse = " + "), sep = " ~ "))
  
  # For each dataset in datlist, run the regression, save results to list
  results <- map(datasets, ~ run_regression(.x, formula))
  
  # Pool the results of each analysis
  pooled_results <- pool_results(results, hypothesis, covariates, outcome)
  return(pooled_results)
}


# Running H1 analysis function
results_h1 <- regress_exposure_univariate(datasets=datlist, 
                               outcome="aadx", 
                               hypothesis = "H1",
                               covariates=unname(unlist(covariates)))

# Add covariate group names
results_h1 <- results_h1 |> 
  mutate(cov_group = sapply(term, function(t) assign_cov_group(t, covariates)),
         term = sapply(term, function(t) replace_terms(t, map)),
         across(where(is.numeric), function(x) round(x, 3)))


save(results_h1, file = "./output/results/results_h1.RData")


###########
# H2 & H3 #
###########

# Specify analysis function for H2 and H3
regress_outcomes <- function(datasets, outcomes, hypothesis, covariates, cutoff_age = NULL, person) {
  
  # Iterate over each outcome per person
  pooled_results <- map(outcomes, function(outcome) {
    
    # Define formula for each outcome
    formula <- as.formula(paste(outcome, paste(covariates, collapse = " + "), sep = " ~ "))
    
    # For each dataset, run the analysis for that outcome
    results <- map(datasets, ~ run_regression(.x, formula, cutoff_age))
    
    # Pool the results
    pooled_results_outcome <- pool_results(results, hypothesis, covariates, outcome)
    
    # Add the person to all results
    pooled_results_outcome$person <- person
    return(pooled_results_outcome)
  })
  return(pooled_results)
}

# Create empty list
results_list_h2 <- list()

# For each person, run the analysis and save all results to the list.
for (person in names(outcomes_childhood)) {
  
  # Define a group of outcomes per person
  ind_outcomes <- outcomes_childhood[[person]]
  
  # Run the analysis function
  ind_results <- regress_outcomes(datasets = datlist, 
                                  ind_outcomes, 
                                  hypothesis = "H2",
                                  covariates=unname(unlist(covariates)),
                                  person = person)
  
  names(ind_results) <- ind_outcomes
  
  # Save results to the list.
  results_list_h2[[person]] <- bind_rows(ind_results)
}

# Run sibling analysises, save to results list
results_list_h2[["Sibling"]] <- regress_outcomes(datasets = datlist_siblings,
                                                 sibling_outcomes_childhood,
                                                 hypothesis = "H2",
                                                 covariates = unname(unlist(covariates_sibling)),
                                                 person = "Sibling")

names(results_list_h2[["Sibling"]]) <- sibling_outcomes_childhood

results_list_h2[["Sibling"]] <- bind_rows(results_list_h2[["Sibling"]])



# Make all person, covariate, and outcome names clearer
results_h2 <- bind_rows(results_list_h2) |> 
  mutate(person = case_when(
    grepl("^child_", person) ~ "Child",
    grepl("^father_", person) ~ "Father",
    grepl("^mother_", person) ~ "Mother",
    TRUE ~ person),
    cov_group = sapply(term, function(t) assign_cov_group(t, covariates_sibling)),
    Outcome = sapply(Outcome, function(outcome) rename_outcome(outcome, outcome_map_childhood)),
    term = sapply(term, function(t) replace_terms(t, map))) |> 
  
  # Apply p-value adjustment across outcomes for each term and family member
  group_by(person, term) |> 
  mutate(adjusted_p = p.adjust(p, method = "BH")) |> 
  ungroup() |> 
  dplyr::select(term, results, se, CIL, CIU, p, adjusted_p, Outcome, Hypothesis, person, cov_group) |> 
  mutate(across(where(is.numeric), function(x) round(x, 3)))


save(results_h2, file = "./output/results/results_h2.RData")

######
# H3 #
######

# Create empty list
results_list_h3 <- list()

# For each person, run the analysis and save the results to the list.
for (person in names(outcomes_adolescence)) {
  ind_outcomes <- outcomes_adolescence[[person]]
  ind_results <- regress_outcomes(datasets = datlist, 
                                  ind_outcomes, 
                                  hypothesis = "H3",
                                  covariates=unname(unlist(covariates)),
                                  person = person)
  names(ind_results) <- ind_outcomes
  results_list_h3[[person]] <- bind_rows(ind_results)
}

# Run sibling analysises, save to results list
results_list_h3[["Sibling"]] <- regress_outcomes(datasets = datlist_siblings,
                                                 sibling_outcomes_adolescence,
                                                 hypothesis = "H3",
                                                 covariates = unname(unlist(covariates_sibling)),
                                                 person = "Sibling")

names(results_list_h3[["Sibling"]]) <- sibling_outcomes_adolescence

results_list_h3[["Sibling"]] <- bind_rows(results_list_h3[["Sibling"]])

# Make all person, covariate, and outcome names clearer
results_h3 <- bind_rows(results_list_h3) |> 
  mutate(person = case_when(
    grepl("^child_", person) ~ "Child",
    grepl("^father_", person) ~ "Father",
    grepl("^mother_", person) ~ "Mother",
    TRUE ~ person),
    cov_group = sapply(term, function(t) assign_cov_group(t, covariates_sibling)),
    Outcome = sapply(Outcome, function(outcome) rename_outcome(outcome, outcome_map_adolescence)),
    term = sapply(term, function(t) replace_terms(t, map))) |> 
  
  # Apply p-value adjustment across outcomes for each term and family member
  group_by(person, term) |> 
  mutate(adjusted_p = p.adjust(p, method = "BH")) |> 
  ungroup() |> 
  dplyr::select(term, results, se, CIL, CIU, p, adjusted_p, Outcome, Hypothesis, person, cov_group) |> 
  mutate(across(where(is.numeric), function(x) round(x, 3)))


save(results_h3, file = "./output/results/results_h3.RData")


#############
# H4 and H5 #
#############

# Specific function for comparing a baseline Model to a Model adjusted for confounding.
regress_full <- function(datasets, exposure, outcomes, baseline_covariate, covariates, cutoff_age = NULL, hypothesis, person) {
  # Iterating over the list of outcomes supplied as an argument.
  pooled_results <- map(outcomes, function(outcome){
    
    # Creating baseline formula
    baseline_covariates <- c(exposure, baseline_covariate)
    baseline_formula <- as.formula(paste(outcome, paste(c(baseline_covariate, exposure), collapse = " + "), sep = " ~ "))
    
    # Creating adjusted formula
    full_covariates <- c(exposure, covariates)
    full_formula <- as.formula(paste(outcome, paste(c(covariates, exposure), collapse = " + "), sep = " ~ "))
    
    # Iterating over the list of datasets, running baseline regression on each dataset.
    results_baseline <- map(datasets, function(data) {
      run_regression(data, baseline_formula, cutoff_age)
    })
    
    # Iterating over the list of datasets, running adjusted regression on each dataset.
    results_full <- map(datasets, function(data) {
      run_regression(data, full_formula, cutoff_age)
    })
    
    # Pooling the results of the baseline regressions
    pooled_results_baseline <- pool_results(results_baseline, hypothesis, baseline_covariates, outcome)
    pooled_results_baseline$person <- person
    pooled_results_baseline$Model <- "Unadjusted"
    
    # Pooling the results of the adjusted Model
    pooled_results_full <- pool_results(results_full, hypothesis, full_covariates, outcome)
    pooled_results_full$person <- person
    pooled_results_full$Model <- "Adjusted"
    
    # Combining results of baseline and adjusted regressions into a single df
    bind_rows(list(baseline = pooled_results_baseline, full = pooled_results_full))
  

  })
  return(pooled_results)
}



######
# H4 #
######

# Create empty results list
results_list_h4 <- list()

# For each person, run the analysis and save the results to the list.
for (person in names(outcomes_childhood)) {
  
  # Save the list of the individual's outcomes (life satisfaction, anxiety, depression) as a vector
  ind_outcomes <- outcomes_childhood[[person]]
  
  # Run the specific regression function with the list of individual's outcomes
  ind_results <- regress_full(datasets = datlist, 
                              exposure = "aadx",
                              outcomes = ind_outcomes,
                              baseline_covariate = "birth_yr",
                              covariates=unname(unlist(covariates)),
                              hypothesis = "H4",
                              person = person)
  
  # Assign names to each set of outcomes
  names(ind_results) <- ind_outcomes

  # Combine results and store in list
  results_list_h4[[person]] <- bind_rows(ind_results)
}

# Run sibling analysises, save to results list
results_list_h4[["Sibling"]] <- regress_full(datasets = datlist_siblings, 
                                             exposure = "aadx",
                                             outcomes = sibling_outcomes_childhood,
                                             baseline_covariate = c("birth_yr", "age_diff"),
                                             covariates=unname(unlist(covariates_sibling)),
                                             hypothesis = "H4",
                                             person = "Sibling")

names(results_list_h4[["Sibling"]]) <- sibling_outcomes_childhood

results_list_h4[["Sibling"]] <- bind_rows(results_list_h4[["Sibling"]])

# Make all person, covariate, and outcome names clearer
results_h4 <- bind_rows(results_list_h4) |> 
  mutate(person = case_when(
    grepl("^child_", person) ~ "Child",
    grepl("^father_", person) ~ "Father",
    grepl("^mother_", person) ~ "Mother",
    TRUE ~ person),
    cov_group = sapply(term, function(t) assign_cov_group(t, covariates_sibling)),
    Outcome = sapply(Outcome, function(outcome) rename_outcome(outcome, outcome_map_childhood)),
    term = sapply(term, function(t) replace_terms(t, map))) |> 
  
  # Apply p-value adjustment across outcomes for each term, family member, and Model
  group_by(term, person, Model) |> 
  mutate(adjusted_p = p.adjust(p, method = "BH")) |> 
  ungroup() |> 
  dplyr::select(term, results, se, CIL, CIU, p, adjusted_p, vif, Outcome, Hypothesis, person, Model, cov_group) |> 
  mutate(across(where(is.numeric), function(x) round(x, 3)))

save(results_h4, file = "./output/results/results_h4.RData")

######
# H5 # 
######

results_list_h5 <- list()
# For each person, run the analysis and save the results to the list.
for (person in names(outcomes_adolescence)) {
  ind_outcomes <- outcomes_adolescence[[person]]
  ind_results <- regress_full(datasets = datlist, 
                              exposure = "aadx",
                              outcomes = ind_outcomes,
                              baseline_covariate = "birth_yr",
                              covariates=unname(unlist(covariates)),
                              hypothesis = "H5",
                              person = person)
  names(ind_results) <- ind_outcomes
  print(ind_results)
  results_list_h5[[person]] <- bind_rows(ind_results)
}


# Run sibling analysises, save to results list
results_list_h5[["Sibling"]] <- regress_full(datasets = datlist_siblings, 
                                             exposure = "aadx",
                                             outcomes = sibling_outcomes_adolescence,
                                             baseline_covariate = c("birth_yr", "age_diff"),
                                             covariates=unname(unlist(covariates_sibling)),
                                             hypothesis = "H5",
                                             person = "Sibling")

names(results_list_h5[["Sibling"]]) <- sibling_outcomes_adolescence

results_list_h5[["Sibling"]] <- bind_rows(results_list_h5[["Sibling"]])

# Make all person, covariate, and outcome names clearer
results_h5 <- bind_rows(results_list_h5) |> 
  mutate(person = case_when(
    grepl("^child_", person) ~ "Child",
    grepl("^father_", person) ~ "Father",
    grepl("^mother_", person) ~ "Mother",
    TRUE ~ person),
    cov_group = sapply(term, function(t) assign_cov_group(t, covariates_sibling)),
    Outcome = sapply(Outcome, function(outcome) rename_outcome(outcome, outcome_map_adolescence)),
    term = sapply(term, function(t) replace_terms(t, map))) |> 
  
  # Apply p-value adjustment across outcomes for each term, family member, and Model
  group_by(term, person, Model) |> 
  mutate(adjusted_p = p.adjust(p, method = "BH")) |> 
  ungroup() |> 
  dplyr::select(term, results, se, CIL, CIU, p, adjusted_p, vif, Outcome, Hypothesis, person, Model, cov_group) |> 
  mutate(across(where(is.numeric), function(x) round(x, 3)))

save(results_h5, file = "./output/results/results_h5.RData")


#############
# H6 and H7 #
#############

regress_interactions <- function(datasets, exposure, outcomes, baseline_covariate, covariates, interaction_term, cutoff_age = NULL, hypothesis, person) {
  pooled_results <- map(outcomes, function(outcome){
    
    
    baseline_covariates <- c(exposure, baseline_covariate, interaction_term)
    baseline_formula <- as.formula(paste(outcome, paste(c(paste(interaction_term, baseline_covariate, sep = "*"), paste(interaction_term, exposure, sep = "*"), interaction_term), collapse = " + "), sep = " ~ "))
    
    full_covariates <- c(exposure, covariates, interaction_term)
    full_formula <- as.formula(paste(outcome, paste(c(paste(interaction_term, covariates, sep = "*"), paste(interaction_term, exposure, sep = "*"), interaction_term), collapse = " + "), sep = " ~ "))
    
    results_baseline <- map(datasets, function(data) {
      run_regression(data, baseline_formula, cutoff_age)
    })
    
    results_full <- map(datasets, function(data) {
      run_regression(data, full_formula, cutoff_age)
    })
    
    pooled_results_baseline <- pool_results(results_baseline, hypothesis, baseline_covariates, outcome)
    pooled_results_baseline$person <- person
    pooled_results_baseline$Model <- "Unadjusted"
    
    pooled_results_full <- pool_results(results_full, hypothesis, full_covariates, outcome)
    pooled_results_full$person <- person
    pooled_results_full$Model <- "Adjusted"
    
    bind_rows(list(baseline = pooled_results_baseline, full = pooled_results_full))
    
    
  })
  return(pooled_results)
}


######
# H6 # 
######

results_list_h6 <- list()
# For each person, run the analysis and save the results to the list.
for (person in names(outcomes_childhood)) {
  ind_outcomes <- outcomes_childhood[[person]]
  ind_results <- regress_interactions(datasets = datlist, 
                                      exposure = "aadx",
                                      outcomes = ind_outcomes,
                                      baseline_covariate = "birth_yr",
                                      covariates= setdiff(unname(unlist(covariates)), "sex"),
                                      interaction_term = "sex",
                                      hypothesis = "H6",
                                      person = person)
  names(ind_results) <- ind_outcomes
  print(ind_results)
  results_list_h6[[person]] <- bind_rows(ind_results)
}



# Run sibling analysises, save to results list
results_list_h6[["Sibling"]] <- regress_interactions(datasets = datlist_siblings, 
                                                     exposure = "aadx",
                                                     outcomes = sibling_outcomes_childhood,
                                                     baseline_covariate = c("birth_yr", "age_diff"),
                                                     covariates= setdiff(unname(unlist(covariates_sibling)), "sex"),
                                                     interaction_term = "sex",
                                                     hypothesis = "H6",
                                                     person = "Sibling")

names(results_list_h6[["Sibling"]]) <- sibling_outcomes_childhood

results_list_h6[["Sibling"]] <- bind_rows(results_list_h6[["Sibling"]])

# Make all person, covariate, and outcome names clearer
results_h6 <- bind_rows(results_list_h6) |> 
  mutate(person = case_when(
    grepl("^child_", person) ~ "Child",
    grepl("^father_", person) ~ "Father",
    grepl("^mother_", person) ~ "Mother",
    TRUE ~ person),
    cov_group = sapply(term, function(t) assign_cov_group(t, covariates_sibling)),
    Outcome = sapply(Outcome, function(outcome) rename_outcome(outcome, outcome_map_childhood)),
    term = sapply(term, function(t) replace_terms(t, map))) |> 
  
  # Apply p-value adjustment across outcomes for each term, family member, and Model
  group_by(term, person, Model) |> 
  mutate(adjusted_p = p.adjust(p, method = "BH")) |> 
  ungroup() |> 
  dplyr::select(term, results, se, CIL, CIU, p, adjusted_p, vif, Outcome, Hypothesis, person, Model, cov_group) |> 
  mutate(across(where(is.numeric), function(x) round(x, 3)))

save(results_h6, file = "./output/results/results_h6.RData")

######
# H7 #
######

results_list_h7 <- list()
# For each person, run the analysis and save the results to the list.
for (person in names(outcomes_adolescence)) {
  ind_outcomes <- outcomes_adolescence[[person]]
  ind_results <- regress_interactions(datasets = datlist, 
                                      exposure = "aadx",
                                      outcomes = ind_outcomes,
                                      baseline_covariate = "birth_yr",
                                      covariates=setdiff(unname(unlist(covariates)), "sex"),
                                      interaction_term = "sex",
                                      hypothesis = "H7",
                                      person = person)
  names(ind_results) <- ind_outcomes
  print(ind_results)
  results_list_h7[[person]] <- bind_rows(ind_results)
}

# Run sibling analysises, save to results list
results_list_h7[["Sibling"]] <- regress_interactions(datasets = datlist_siblings, 
                                                     exposure = "aadx",
                                                     outcomes = sibling_outcomes_adolescence,
                                                     baseline_covariate = c("birth_yr", "age_diff"),
                                                     covariates=setdiff(unname(unlist(covariates_sibling)), "sex"),
                                                     interaction_term = "sex",
                                                     hypothesis = "H7",
                                                     person = "Sibling")

names(results_list_h7[["Sibling"]]) <- sibling_outcomes_adolescence

results_list_h7[["Sibling"]] <- bind_rows(results_list_h7[["Sibling"]])

# Make all person, covariate, and outcome names clearer
results_h7 <- bind_rows(results_list_h7) |> 
  mutate(person = case_when(
    grepl("^child_", person) ~ "Child",
    grepl("^father_", person) ~ "Father",
    grepl("^mother_", person) ~ "Mother",
    TRUE ~ person),
    cov_group = sapply(term, function(t) assign_cov_group(t, covariates_sibling)),
    Outcome = sapply(Outcome, function(outcome) rename_outcome(outcome, outcome_map_adolescence)),
    term = sapply(term, function(t) replace_terms(t, map))) |> 
  
  # Apply p-value adjustment across outcomes for each term, family member, and Model
  group_by(term, person, Model) |> 
  mutate(adjusted_p = p.adjust(p, method = "BH")) |> 
  ungroup() |> 
  dplyr::select(term, results, se, CIL, CIU, p, adjusted_p, vif, Outcome, Hypothesis, person, Model, cov_group) |> 
  mutate(across(where(is.numeric), function(x) round(x, 3)))


save(results_h7, file = "./output/results/results_h7.RData")


write.csv(results_h1, file = "./output/results/csv/results_h1.csv", row.names = FALSE)
write.csv(results_h2, file = "./output/results/csv/results_h2.csv", row.names = FALSE)
write.csv(results_h3, file = "./output/results/csv/results_h3.csv", row.names = FALSE)
write.csv(results_h4, file = "./output/results/csv/results_h4.csv", row.names = FALSE)
write.csv(results_h5, file = "./output/results/csv/results_h5.csv", row.names = FALSE)
write.csv(results_h6, file = "./output/results/csv/results_h6.csv", row.names = FALSE)
write.csv(results_h7, file = "./output/results/csv/results_h7.csv", row.names = FALSE)


# Perform wald tests
source("./scripts/02.5_wald_tests.R")
