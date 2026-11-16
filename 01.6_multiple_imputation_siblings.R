# To do:
#   - check for which variables missingness cannot be imputed (e.g., place of residence?)
#   - Remove the individuals for which these observations are missing


load(file = "./data/full_df_siblings.RData")
library(corrplot)
library(tidyverse)
library(mice)
library(haven)


# Create a dataset with just the variables to be imputed
imp_df_siblings <- full_df_siblings

imp_df_siblings <- imp_df_siblings %>% 
  select(-ind_id, -m_id, -f_id, -index_id, -childageyrs_first_dx_asd_npr, -year_first_dx_asd_npr,
         -received_dx_asd_npr, -received_dx_2x_asd_npr, -received_dx_cp_npr, -received_dx_ep_npr, -received_dx_id_npr,
         -language_skills_2_3yr, -language_skills_3_3yr
         #-fylke,-country_bg, -world_region_bg, -country_birth, -world_region_birth
         #-m_edu, -f_edu
  )

# Initialise settings for multiple imputation
init <- mice(imp_df_siblings, maxit=0) 
predictorMatrix = init$predictorMatrix
meth = init$method
post = init$post

# Create a predictor matrix based on correlations of at least r = 0.1.
pred_siblings <- quickpred(imp_df_siblings, include = c("sibling_sex", 
                                                        "sibling_birth_yr",
                                                        "birth_yr", 
                                                        "age_diff", 
                                                        "sex"))

# Include esat and mchat only as predictors for aadx
pred_siblings["esat_short_c_3yr", ] <- 0
pred_siblings["mchat_short_c_q3yr", ] <- 0
#Also including scq_rrb and swls_full_m_8yr as predictors, like in the main dataset.
pred_siblings["aadx", c("esat_short_c_3yr", "mchat_short_c_q3yr", "scq_rrb_c_3yr", "swls_full_m_8yr")] <- 1

# c_household_income giving problems as an aadx predictor
pred_siblings["aadx", "c_household_income"] <- 0

# Assign an imputation method to each variable.
method_list <- list(
  polr = c("social_support_3yr",
           "social_support_freq_3yr",
           "social_support_8yr",
           "social_support_freq_8yr",
           "social_support_14yr",
           "social_support_freq_14yr",
           "financ_probs_18m"
  ), # ordinal
  pmm = c("scq_sci_c_3yr", 
          "scq_rrb_c_3yr", 
          "age_onset_walking", 
          "cbcl_ext_c_3yr", 
          "ccc2_coherence_c_5yr_manual",
          "cbcl_ext_c_5yr",
          "m_age_at_birth",
          "p_age_at_birth",
          "smfq_dep_c_8yr",
          "scared_anx_c_8yr",
          "life_enjoyment_8yr",
          "swls_full_c_14c",
          "smfq_dep_m_14m",
          "scared_anx_m_14m",
          "swls_full_m_8yr",
          "concerns_3yr",
          "concerns_5yr",
          "scl_anx_m_8yr",
          "scl_dep_m_8yr",
          "swls_full_m_14m",
          "scl_anx_m_14m",
          "scl_dep_m_14m",
          "swls_full_f_far2",
          "scl_anx_f_far2",
          "scl_dep_f_far2",
          "language_skills_1_3yr",
          "c_household_income",
          "sibling_smfq_dep_c_8yr",
          "sibling_scared_anx_c_8yr",
          "sibling_life_enjoyment_8yr",
          "sibling_swls_full_c_14c",
          "sibling_smfq_dep_m_14m",
          "sibling_scared_anx_m_14m",
          "m_edu",
          "f_edu",
          "aadx"
  ),# Count data
  logreg = c("parent_cohab_18m",
             "received_dx_2x_id_npr",
             "received_dx_2x_ep_npr",
             "received_dx_2x_cp_npr",
             "motor_dev_5yr", 
             "language_dev_5yr"
  ),
  norm = c("child_ADHD2022_pgs_res",
           "child_an_pgc3_pgs_res",
           "child_anx_pgs_res",
           "child_asd_pgs_res",
           "child_bip2021_pgs_res",
           "child_chmaltreat_pgs_res",
           "child_ea3_pgs_res",
           "child_insomnia_pgs_res",
           "child_intell_pgs_res",
           "child_mdd_pgc2024_pgs_res",
           "child_ptsd_pgs_res",
           "child_scz2022_pgs_res",
           
           "mother_ADHD2022_pgs_res",
           "mother_an_pgc3_pgs_res",
           "mother_anx_pgs_res",
           "mother_asd_pgs_res",
           "mother_bip2021_pgs_res",
           "mother_chmaltreat_pgs_res",
           "mother_ea3_pgs_res",
           "mother_insomnia_pgs_res",
           "mother_intell_pgs_res",
           "mother_mdd_pgc2024_pgs_res",
           "mother_ptsd_pgs_res",
           "mother_scz2022_pgs_res",
           
           "father_ADHD2022_pgs_res",
           "father_an_pgc3_pgs_res",
           "father_anx_pgs_res",
           "father_asd_pgs_res",
           "father_bip2021_pgs_res",
           "father_chmaltreat_pgs_res",
           "father_ea3_pgs_res",
           "father_insomnia_pgs_res",
           "father_intell_pgs_res",
           "father_mdd_pgc2024_pgs_res",
           "father_ptsd_pgs_res",
           "father_scz2022_pgs_res",
           
           "sibling_child_ADHD2022_pgs_res",
           "sibling_child_an_pgc3_pgs_res",
           "sibling_child_anx_pgs_res",
           "sibling_child_asd_pgs_res",
           "sibling_child_bip2021_pgs_res",
           "sibling_child_chmaltreat_pgs_res",
           "sibling_child_ea3_pgs_res",
           "sibling_child_insomnia_pgs_res",
           "sibling_child_intell_pgs_res",
           "sibling_child_mdd_pgc2024_pgs_res",
           "sibling_child_ptsd_pgs_res",
           "sibling_child_scz2022_pgs_res"
  ),
  custom = c("aadx"))

# Add method to each list
for (method in names(method_list)) {
  meth[method_list[[method]]] <- method
}

# Turn all variables assigned polr into ordinal variables.
for (var_name in method_list$polr) {
  imp_df_siblings[[var_name]] <- ordered(imp_df_siblings[[var_name]])
}

# Create a custom imputation method for age at diagnosis (i.e., diagnosed before 2010)
custom_impute_aadx <- function(y, ry, x, wy, ...) {
  upper_limit <- 2009 - x[, "birth_yr"]  # Calculate upper limit for aadx
  imp_values <- mice.impute.pmm(y, ry, x, wy=wy)  # Impute values using normal regression
  
  # Apply constraint: limit values to be <= upper_limit
  imp_values <- pmin(imp_values, upper_limit[wy])
  return(imp_values)
}

mice.impute.custom <- custom_impute_aadx

# Run imputation
imp_siblings <-
  mice(
    imp_df_siblings,
    method = meth, 
    maxit = 30,
    m = 30,
    predictorMatrix = pred_siblings,
    seed = 37,
    printFlag = TRUE,
    ridge = 0.1
  )



#check logged events
imp$loggedEvents

imputed_datasets_siblings <- complete(imp_siblings, action = 'long', include = TRUE)

# Save imputed datasets
save(imp_siblings, file = "./data/imputations_siblings.RData")
save(imputed_datasets_siblings, file = "./data/imputed_datasets_siblings.RData")


# Create a list of imputed datasets
split_datasets_siblings <- split(imputed_datasets_siblings, imputed_datasets_siblings$.imp)

mids_datasets_siblings <- as.mids(do.call(rbind, split_datasets_siblings))

datlist_siblings <- miceadds::mids2datlist(mids_datasets_siblings)

save(datlist_siblings, file = "./data/MI_datlist_siblings.RData")


corr_df <- datlist_siblings[[1]] %>%
  
  select_if(is.numeric)

cor_matrix <- cor(corr_df)
corrplot(cor_matrix)


##################
# QC
##################
library(lattice)

load(file = "./data/imputations_new.RData")

plot(imp)

densityplot(imp)


