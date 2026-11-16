library(tidyverse)
library(lme4)
library(mice)

load("./data/full_df_asd.RData")
load("./data/full_df_siblings.RData")

source("./scripts/00_replace_terms.R")
source("./scripts/02.1_analysis_covs.R")

date <- format(Sys.Date(), "%Y-%m-%d")

######################
# Missing data table #
######################

missing_data <- colMeans(is.na(full_df_asd)) * 100

missing_data_df <- data.frame(
  Variable = names(missing_data),
  Missingness_Percentage = missing_data
)

# Removing rownames
rownames(missing_data_df) <- NULL

missing_data_df <- missing_data_df |>
  filter(Variable %in% c(unname(unlist(covariates)), unname(unlist(outcomes_childhood)), unname(unlist(outcomes_adolescence))))  |> 
  mutate(Variable = sapply(Variable, function(t) replace_terms(t, map)),
         across(where(is.numeric), function(x) round(x, 2)),
         Missingness_Percentage = paste0(Missingness_Percentage, "%"))


# Doing the same for siblings

missing_data_sib <- colMeans(is.na(full_df_siblings)) * 100

missing_data_sib_df <- data.frame(
  Variable = names(missing_data_sib),
  Missingness_Percentage = missing_data_sib
)

# Removing rownames
rownames(missing_data_sib_df) <- NULL

missing_data_sib_df <- missing_data_sib_df |>
  filter(Variable %in% c(unname(unlist(covariates_sibling)), sibling_outcomes_childhood, sibling_outcomes_adolescence))  |> 
  mutate(Variable = sapply(Variable, function(t) replace_terms(t, map)),
         across(where(is.numeric), function(x) round(x, 2)),
         Missingness_Percentage = paste0(Missingness_Percentage, "%")) |> 
  filter(!Variable %in% missing_data_df$Variable)


missing_data_table <- missing_data_df |> 
  bind_rows(missing_data_sib_df)


write_csv(missing_data_table, file = paste0("./output/missingness/", date, "_missingness_table.csv"))



#####################################
# Running generalised linear models # 
#####################################


# Creating predictor list
predictors <- c("aadx",
                "birth_yr",
                "scq_sci_c_3yr",
                "scq_rrb_c_3yr",
                "concerns_3yr",
                "concerns_5yr",
                "received_dx_2x_id_npr",
                "received_dx_2x_ep_npr",
                "received_dx_2x_cp_npr",
                "sex",
                "parity",
                "m_edu",
                "f_edu",
                "c_household_income",
                "fylke")

# Creating outcome list
outcomes <- c(unname(unlist(outcomes_childhood)),
              unname(unlist(outcomes_adolescence)))


df_asd <- full_df_asd |> 
  dplyr::select(all_of(c(predictors, outcomes)))


mcar_test(df_asd)

# Initialising results list
missingness_tests <- list()

# Iterating over outcomes
for (outcome in outcomes) {
  
  # Creating a dummy variable for each outcome
  dummy_var_name <- paste0(outcome, "_dummy")
  full_df_asd[[dummy_var_name]] <- ifelse(is.na(full_df_asd[[outcome]]), 1, 0)
  
  # Running glm
  model <- paste0(dummy_var_name, " ~ aadx + ", paste(predictors, collapse = " + "))
  glm_result <- glm(as.formula(model), data = full_df_asd, family = binomial(link = "logit"))
  
  # Saving results to list
  missingness_tests[[outcome]] <- summary(glm_result)
}

sibling_outcomes <- c(sibling_outcomes_childhood, sibling_outcomes_adolescence)

#Doing the same for siblings
for (outcome in sibling_outcomes) {
  
  # Creating a dummy variable for each outcome
  dummy_var_name <- paste0(outcome, "_dummy")
  full_df_siblings[[dummy_var_name]] <- ifelse(is.na(full_df_siblings[[outcome]]), 1, 0)
  
  # Running glm
  model <- paste0(dummy_var_name, " ~ aadx + ", paste(predictors, collapse = " + "))
  glm_result <- glm(as.formula(model), data = full_df_siblings, family = binomial(link = "logit"))
  
  # Saving results to list
  missingness_tests[[outcome]] <- summary(glm_result)
}


save(missingness_tests, file = "./output/missingness/missingness_tests.RData")
