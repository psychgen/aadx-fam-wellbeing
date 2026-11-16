# To do:
#   - check for which variables missingness cannot be imputed (e.g., place of residence?)
#   - Remove the individuals for which these observations are missing


load(file = "./data/full_df_asd.RData")
library(corrplot)
library(dplyr)
library(mice)
library(haven)


# Create a dataset with just the variables to be imputed
imp_df <- full_df_asd

imp_df <- imp_df %>% 
  select(-ind_id, -m_id, -f_id, -childageyrs_first_dx_asd_npr, -year_first_dx_asd_npr,
         -received_dx_asd_npr, -received_dx_2x_asd_npr, -received_dx_cp_npr, -received_dx_ep_npr, -received_dx_id_npr, 
         -language_skills_2_3yr, -language_skills_3_3yr
         #-fylke,-country_bg, -world_region_bg, -country_birth, -world_region_birth
         #-m_edu, -f_edu
         )

# Initialise settings for multiple imputation
init <- mice(imp_df, maxit=0) 
predictorMatrix = init$predictorMatrix
meth = init$method
post = init$post

# Create a predictor matrix based on correlations of at least r = 0.1.
pred <- quickpred(imp_df, include = c("birth_yr", "sex"))

# Include esat and mchat only as predictors for aadx
pred["esat_short_c_3yr", ] <- 0
pred["mchat_short_c_q3yr", ] <- 0
pred["aadx", c("esat_short_c_3yr", "mchat_short_c_q3yr")] <- 1

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
          "m_edu",
          "f_edu"
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
           "father_scz2022_pgs_res"
           ), # Normal distribution
  custom = c("aadx")) 

# Add method to each list
for (method in names(method_list)) {
  meth[method_list[[method]]] <- method
}

# Turn all variables assigned polr into ordinal variables.
for (var_name in method_list$polr) {
  imp_df[[var_name]] <- ordered(imp_df[[var_name]])
}

# Create a custom imputation method for age at diagnosis (i.e., diagnosed before 2010)
custom_impute_aadx <- function(y, ry, x, wy, ...) {
  upper_limit <- 2010 - x[, "birth_yr"]  # Calculate upper limit for aadx
  imp_values <- mice.impute.pmm(y, ry, x, wy=wy)  # Impute values using normal regression
  
  # Apply constraint: limit values to be <= upper_limit
  imp_values <- pmin(imp_values, upper_limit[wy])
  return(imp_values)
}

mice.impute.custom <- custom_impute_aadx


# Run imputation
imp <-
  mice(
    imp_df,
    method = meth, 
    maxit = 30,
    m = 30,
    predictorMatrix = pred,
    seed = 37,
    printFlag = TRUE,
    ridge = 0.1
  )



#check logged events
imp$loggedEvents

imputed_datasets <- complete(imp, action = 'long', include = TRUE)

# Save imputed datasets
save(imp, file = "./data/imputations_new.RData")
save(imputed_datasets, file = "./data/imputed_datasets_new.RData")


# Create a list of imputed datasets
split_datasets <- split(imputed_datasets, imputed_datasets$.imp)

mids_datasets <- as.mids(do.call(rbind, split_datasets))

datlist <- miceadds::mids2datlist(mids_datasets)

save(datlist, file = "./data/MI_datlist_new.RData")


corr_df <- datlist[[1]] %>%
  select(-m_id, -f_id, -ind_id) %>%
  select_if(is.numeric)

cor_matrix <- cor(corr_df)
corrplot(cor_matrix)


###############################
# Checking imputation of aadx #
###############################

library(gridExtra)

create_imputed_plot <- function(data, data_original, imputation_number) {
  # Create logical indicator for imputed values in `var3`
  is_imputed <- is.na(imp_df$aadx)
  # Add the indicator to the completed dataset
  data$imputed <- ifelse(is_imputed, 'Imputed', 'Observed')
  
  # Plot the data
  plot <- ggplot(data, aes(x = birth_yr, y = aadx, color = imputed)) +
    geom_jitter() +
    labs(title = paste("Imputation", imputation_number),
         x = "Birth year", y = "aadx", color = "Value Status") +
    theme_minimal()
  return(plot)
}

plots <- list()
for (i in 1:5) {
  completed_data <- complete(imp, i)
  plots[[i]] <- create_imputed_plot(completed_data, imp_df, i)
}

tiff("./output/2024-12-27_aadx-imputation.tiff", width = 12, height = 8, units = "in", res = 300)
# Combine the plots into a grid
grid.arrange(grobs = plots, ncol = 3)
dev.off()

##################
# QC
##################
library(lattice)

load(file = "./data/imputations_new.RData")

plot(imp)



tiff("./output/2025-05-26_imputation_density_plot.tiff", width = 190, height = 90, units = "mm", res = 300)
densityplot(imp) |> print()
dev.off()

