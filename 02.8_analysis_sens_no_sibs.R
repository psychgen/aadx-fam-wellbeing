library(dplyr)
library(purrr)

load("./data/MI_datlist_new.RData")
load("./data/full_df_asd.RData")

source("./scripts/02.1_analysis_covs.R")
source("./scripts/02.2_analysis_functions.R")


# Add IDs to datlist
id_df <- full_df_asd |> 
  dplyr::select(ind_id, m_id, f_id)

datlist <- lapply(datlist, function(df) {
  cbind(id_df, df)
})

# Group by m_id, randomly select one autistic sibling to be the index child
datlist_no_sibs <- lapply(datlist, function(df) {
  df |> 
    # Selecting by m_id to get all autistic children within one family
    group_by(m_id) |> 
    
    # select one of these children to be the index child
    slice_sample(n=1)|> 
    ungroup()

})

######
# H4 #
######

# Create empty results list
results_list_h4_sens_ns <- list()

# For each person, run the analysis and save the results to the list.
for (person in names(outcomes_childhood)) {
  
  # Save the list of the individual's outcomes (life satisfaction, anxiety, depression) as a vector
  ind_outcomes <- outcomes_childhood[[person]]
  
  # Run the specific regression function with the list of individual's outcomes
  ind_results <- regress_full(datasets = datlist_no_sibs, 
                              exposure = "aadx",
                              outcomes = ind_outcomes,
                              baseline_covariate = "birth_yr",
                              covariates=unname(unlist(covariates)),
                              hypothesis = "H4",
                              person = person)
  
  # Assign names to each set of outcomes
  names(ind_results) <- ind_outcomes
  
  # Combine results and store in list
  results_list_h4_sens_ns[[person]] <- bind_rows(ind_results)
}


# Make all person, covariate, and outcome names clearer
results_h4_sens_ns <- bind_rows(results_list_h4_sens_ns) |> 
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

save(results_h4_sens_ns, file = "./output/results/results_h4_sens_ns.RData")

######
# H5 #
######

results_list_h5_sens_ns <- list()
# For each person, run the analysis and save the results to the list.
for (person in names(outcomes_adolescence)) {
  ind_outcomes <- outcomes_adolescence[[person]]
  ind_results <- regress_full(datasets = datlist_no_sibs, 
                              exposure = "aadx",
                              outcomes = ind_outcomes,
                              baseline_covariate = "birth_yr",
                              covariates=unname(unlist(covariates)),
                              hypothesis = "H5",
                              person = person)
  names(ind_results) <- ind_outcomes
  print(ind_results)
  results_list_h5_sens_ns[[person]] <- bind_rows(ind_results)
}

# Make all person, covariate, and outcome names clearer
results_h5_sens_ns <- bind_rows(results_list_h5_sens_ns) |> 
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

save(results_h5_sens_ns, file = "./output/results/results_h5_sens_ns.RData")
