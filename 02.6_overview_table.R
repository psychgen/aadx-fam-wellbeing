# Creates an overview stratified by timing of diagnosis

library(tidyverse)

load("./data/full_df_proc.RData")

full_df_asd <- full_df |> 
  filter(received_dx_2x_asd_npr == 1)

rm(full_df)


full_df_asd <- full_df_asd |> 
  mutate(aadx_cat = as.factor(case_when(aadx < 9 ~ "Early",
                                        aadx >= 9 & aadx < 15 ~ "Mid",
                                        aadx >= 15 ~ "Late",
                                        is.na(aadx) ~ "Early")))


plotting <- function(var) {
  var_enquo <- enquo(var)
  
  mean_data <- full_df_asd |> 
    group_by(aadx_cat) |> 
    summarise(mean = mean(!!var_enquo, na.rm = TRUE))
  
  ggplot(full_df_asd, aes(x = {{ var }}))+
    geom_histogram(binwidth = 1)+
    facet_grid(. ~ aadx_cat)+
    geom_vline(data = mean_data, aes(xintercept = mean))+
    theme_minimal()
}

plotting(scq_sci_c_3yr)



is_binary <- function(var) {
  unique_len <- length(unique(na.omit(var)))
  return(unique_len == 2)
}


covariates <- list("Birth year" = c("birth_yr"),
                   "Behaviour and functioning" = c("age_onset_walking",
                                                   "language_skills_1_3yr",
                                                   "language_skills_2_3yr",
                                                   "language_skills_3_3yr",
                                                   "scq_sci_c_3yr",
                                                   "scq_rrb_c_3yr",
                                                   "cbcl_ext_c_3yr",
                                                   "motor_dev_5yr",
                                                   "language_dev_5yr",
                                                   "ccc2_coherence_c_5yr_manual",
                                                   "cbcl_ext_c_5yr"
                                                   #"iq"
                   ),
                   "NPR" = c("received_dx_2x_id_npr",
                             "received_dx_2x_ep_npr",
                             "received_dx_2x_cp_npr"
                   ),
                   "S" = c("sex"),
                   "Socio-familial factors" = c("m_age_at_birth", # Mother's age at birth
                                                "p_age_at_birth", # Father's age at birth
                                                "parity", # Mother's parity
                                                "m_edu",
                                                "f_edu",
                                                "c_household_income",
                                                "parent_cohab_18m", 
                                                "financ_probs_18m"
                                                #"world_region_bg",
                                                #"fylke"
                   ), # financial difficulties 18mo
                   "Social support and concerns" = c("concerns_3yr",
                                                     "concerns_5yr",
                                                     "social_support_3yr", # Social support: How many people other than your partner Q3yr
                                                     "social_support_freq_3yr", # Social support: How often do you see/phone others Q3yr
                                                     "social_support_8yr", # Social support: How many people other than your partner Q8yr
                                                     "social_support_freq_8yr", # Social support: How often do you see/phone others Q8yr
                                                     "social_support_14yr", # Social support: How many people other than your partner Q14yr
                                                     "social_support_freq_14yr" # Social support: How often do you see/phone others Q14yr
                   ),
                   "Child PGS" = c("child_ADHD2022_pgs_res",
                                   "child_an_pgc3_pgs_res",
                                   "child_anx_pgs_res",
                                   "child_asd_pgs_res",
                                   "child_bip2021_pgs_res",
                                   "child_chmaltreat_pgs_res",
                                   "child_ea3_pgs_res",
                                   "child_insomnia_pgs_res",
                                   "child_intell_pgs_res",
                                   "child_mdd_pgc2024_pgs_res",
                                   "child_scz2022_pgs_res"
                   ),
                   "Mother PGS" = c("mother_ADHD2022_pgs_res",
                                    "mother_an_pgc3_pgs_res",
                                    "mother_anx_pgs_res",
                                    "mother_asd_pgs_res",
                                    "mother_bip2021_pgs_res",
                                    "mother_chmaltreat_pgs_res",
                                    "mother_ea3_pgs_res",
                                    "mother_insomnia_pgs_res",
                                    "mother_intell_pgs_res",
                                    "mother_mdd_pgc2024_pgs_res",
                                    "mother_scz2022_pgs_res"
                   ),
                   "Father PGS" = c("father_ADHD2022_pgs_res",
                                    "father_an_pgc3_pgs_res",
                                    "father_anx_pgs_res",
                                    "father_asd_pgs_res",
                                    "father_bip2021_pgs_res",
                                    "father_chmaltreat_pgs_res",
                                    "father_ea3_pgs_res",
                                    "father_insomnia_pgs_res",
                                    "father_intell_pgs_res",
                                    "father_mdd_pgc2024_pgs_res",
                                    "father_scz2022_pgs_res"
                   )
)


# Function to determine if a variable is binary
is_binary <- function(x) {
  is.factor(x) || is.logical(x) || (is.numeric(x) && length(unique(x)) == 2)
}

# Function to create a summary table
create_summary <- function(df, vars) {
  continuous_vars <- vars[!sapply(df[vars], is_binary)]
  binary_vars <- vars[sapply(df[vars], is_binary)]
  
  summary_list <- list()
  
  # Continuous variables summary
  if (length(continuous_vars) > 0) {
    continuous_summary <- df %>%
      pivot_longer(cols = all_of(continuous_vars), names_to = "variable", values_to = "value") %>%
      group_by(variable, aadx_cat) %>%
      summarize(
        mean = mean(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(summary = paste0(round(mean, 2), " (", round(sd, 2), ")")) |> 
      dplyr::select(variable, aadx_cat, summary)
    
    summary_list[[length(summary_list) + 1]] <- continuous_summary
  }
  
  
  # Binary variables summary
  if (length(binary_vars) > 0) {
    binary_summary <- df %>%
      pivot_longer(cols = all_of(binary_vars), names_to = "variable", values_to = "value") %>%
      group_by(variable, aadx_cat) %>%
      summarize(
        proportion = mean(as.numeric(as.character(value)), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(summary = paste0(round(proportion * 100, 1), "%")) |> 
      dplyr::select(variable, aadx_cat, summary)
    summary_list[[length(summary_list) + 1]] <- binary_summary
  }
  
  
  if (length(summary_list) > 0) {
    bind_rows(summary_list) %>%
      pivot_wider(names_from = aadx_cat, values_from = summary) |> 
      dplyr::select(variable, Early, Mid, Late)
  } else {
    message("No variables to summarize.")
    return(NULL)
  }
}

# Create a summary table for each group in the list
summary_tables <- lapply(covariates, function(vars) create_summary(full_df_asd, vars))

# Print each summary table
for (group_name in names(summary_tables)) {
  cat("Summary for", group_name, ":\n")
  print(summary_tables[[group_name]])
  cat("\n")
}




###################
# outcome summary #
###################

outcome_summary <- full_df_asd %>%
  pivot_longer(cols = all_of(var_list$outcome_vars), names_to = "variable", values_to = "value") %>%
  group_by(variable, aadx_cat) %>%
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(summary = paste0(round(mean, 2), " (", round(sd, 2), ")")) %>%
  select(variable, aadx_cat, summary) |> 
  pivot_wider(names_from = aadx_cat, values_from = summary)


outcome_summary <- outcome_summary |> 
  mutate(dev_per = case_when(grepl("_8yr$", variable) | grepl("_far2$", variable) ~ "Childhood",
                             grepl("_14m$", variable) | grepl("_14c$", variable) ~ "Adolescence",
                             TRUE ~ NA_character_),
         person = case_when(
           grepl("^scared_", variable) |  grepl("^smfq", variable) |  grepl("^life_enjoyment", variable) |  grepl("swls_full_c_14c", variable) ~ "Child",
           grepl("_f_", variable) ~ "Father",
           grepl("_m_", variable) ~ "Mother",
           TRUE ~ variable
         )
         
  ) |> 
  select(person, dev_per, variable, Early, Mid, Late)



map <- c("smfq_dep_c_8yr" = "Depression",
         "scared_anx_c_8yr" = "Anxiety",
         "life_enjoyment_8yr" = "Life satisfaction",
         "swls_full_c_14c" = "Life satisfaction",
         "smfq_dep_m_14m" = "Depression",
         "scared_anx_m_14m" = "Anxiety",
         "swls_full_m_8yr" = "Life satisfaction",
         "scl_anx_m_8yr" = "Anxiety",
         "scl_dep_m_8yr" = "Depression",
         "swls_full_m_14m" = "Life satisfaction",
         "scl_anx_m_14m" = "Anxiety",
         "scl_dep_m_14m" = "Depression",
         "swls_full_f_far2" = "Life satisfaction",
         "scl_anx_f_far2" = "Anxiety",
         "scl_dep_f_far2" = "Depression")

outcome_summary$variable <- map[outcome_summary$variable]

outcome_summary <- outcome_summary |> 
  group_by(person, dev_per) |> 
  arrange(variable, .by_group = TRUE)


print(outcome_summary)



















# Function to determine if a variable is binary
is_binary <- function(x) {
  is.factor(x) || is.logical(x) || (is.numeric(x) && length(unique(x)) == 2)
}

# Function to create a summary table
create_summary <- function(df, vars) {
  continuous_vars <- vars[!sapply(df[vars], is_binary)]
  binary_vars <- vars[sapply(df[vars], is_binary)]
  
  summary_list <- list()
  
  # Continuous variables summary
  if (length(continuous_vars) > 0) {
    continuous_summary <- df %>%
      pivot_longer(cols = all_of(continuous_vars), names_to = "variable", values_to = "value") %>%
      group_by(variable, aadx_cat) %>%
      summarize(
        mean = mean(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(summary = paste0(round(mean, 2), " (", round(sd, 2), ")")) |> 
      dplyr::select(variable, aadx_cat, summary)
    
    summary_list[[length(summary_list) + 1]] <- continuous_summary
  }

  
  # Binary variables summary
  if (length(binary_vars) > 0) {
    binary_summary <- df %>%
      pivot_longer(cols = all_of(binary_vars), names_to = "variable", values_to = "value") %>%
      group_by(variable, aadx_cat) %>%
      summarize(
        proportion = mean(as.numeric(as.character(value)), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(summary = paste0(round(proportion * 100, 1), "%")) |> 
      dplyr::select(variable, aadx_cat, summary)
    summary_list[[length(summary_list) + 1]] <- binary_summary
  }
  
  
  if (length(summary_list) > 0) {
    bind_rows(summary_list) %>%
      pivot_wider(names_from = aadx_cat, values_from = summary) |> 
      dplyr::select(variable, Early, Mid, Late)
  } else {
    message("No variables to summarize.")
    return(NULL)
  }
}

# Create a summary table for each group in the list
summary_tables <- lapply(covariates, function(vars) create_summary(full_df_asd, vars))

# Print each summary table
for (group_name in names(summary_tables)) {
  cat("Summary for", group_name, ":\n")
  print(summary_tables[[group_name]])
  cat("\n")
}

