map <- c("birth_yr" = "Birth year",
  "age_onset_walking" = "Age at onset walking",
  "language_skills_1_3yr" = "Language skills age 3",
  "scq_sci_c_3yr" = "SCQ Social communication age 3",
  "scq_rrb_c_3yr" = "SCQ Restricted repetitive behaviour age 3",
  "cbcl_ext_c_3yr" = "CBCL Externalising behaviour age 3",
  "motor_dev_5yr" = "Motor development problems age 5",
  "language_dev_5yr" = "Language development problems age 5",
  "ccc2_coherence_c_5yr_manual" = "CCC2 Coherence age 5",
  "cbcl_ext_c_5yr" = "CBCL Externalising behaviour age 5",
  "received_dx_2x_id_npr" = "Intellectual disability diagnosis",
  "received_dx_2x_ep_npr" = "Epilepsy diagnosis",
  "received_dx_2x_cp_npr" = "Cerebral palsy diagnosis",
  "sex" = "Sex",
  "m_age_at_birth" = "Maternal age at birth",
  "p_age_at_birth" = "Paternal age at birth",
  "parity" = "Parity",
  "m_edu" = "Maternal education",
  "f_edu" = "Paternal education",
  "c_household_income" = "Household income",
  "parent_cohab_18m" = "Parental cohabitation",
  "financ_probs_18m" = "Financial problems",
  "world_region_bgAmerika" = "Region of origin: Americas",
  "world_region_bgAsia" = "Region of origin: Asia",
  "world_region_bgEuropa utenom Norge" = "Region of origin: Europe (outside Norway)",
  "world_region_bgAfrika" = "Region of origin: Africa",
  "concerns_3yr" = "Maternal concerns age 3",
  "concerns_5yr" = "Maternal concerns age 5",
  "social_support_3yr" = "Social support age 3",
  "social_support_freq_3yr" = "Social support frequency age 3",
  "social_support_8yr" = "Social support age 8",
  "social_support_freq_8yr" = "Social support frequency age 8",
  "social_support_14yr" = "Social support age 14",
  "social_support_freq_14yr" = "Social support frequency age 14",
  "child_ADHD2022_pgs_res" = "Child ADHD PGS",
  "child_an_pgc3_pgs_res" = "Child Anorexia Nervosa PGS",
  "child_anx_pgs_res" = "Child anxiety PGS",
  "child_asd_pgs_res" = "Child autism PGS",
  "child_bip2021_pgs_res" = "Child bipolar disorder PGS",
  "child_chmaltreat_pgs_res" = "Child childhood maltreatment PGS",
  "child_ea3_pgs_res" = "Child educational attainment PGS",
  "child_insomnia_pgs_res" = "Child insomnia PGS",
  "child_intell_pgs_res" = "Child intelligence PGS",
  "child_mdd_pgc2024_pgs_res" = "Child depression PGS",
  "child_ptsd_pgs_res" = "Child PTSD PGS",
  "child_scz2022_pgs_res" = "Child schizophrenia PGS",
  
  "sibling_child_ADHD2022_pgs_res" = "Sibling ADHD PGS",
  "sibling_child_an_pgc3_pgs_res" = "Sibling Anorexia Nervosa PGS",
  "sibling_child_anx_pgs_res" = "Sibling anxiety PGS",
  "sibling_child_asd_pgs_res" = "Sibling autism PGS",
  "sibling_child_bip2021_pgs_res" = "Sibling bipolar disorder PGS",
  "sibling_child_chmaltreat_pgs_res" = "Sibling childhood maltreatment PGS",
  "sibling_child_ea3_pgs_res" = "Sibling educational attainment PGS",
  "sibling_child_insomnia_pgs_res" = "Sibling insomnia PGS",
  "sibling_child_intell_pgs_res" = "Sibling intelligence PGS",
  "sibling_child_mdd_pgc2024_pgs_res" = "Sibling depression PGS",
  "sibling_child_ptsd_pgs_res" = "Sibling PTSD PGS",
  "sibling_child_scz2022_pgs_res" = "Sibling schizophrenia PGS",
  
  "mother_ADHD2022_pgs_res" = "Mother ADHD PGS",
  "mother_an_pgc3_pgs_res" = "Mother Anorexia Nervosa PGS",
  "mother_anx_pgs_res" = "Mother anxiety PGS",
  "mother_asd_pgs_res" = "Mother autism PGS",
  "mother_bip2021_pgs_res" = "Mother bipolar disorder PGS",
  "mother_chmaltreat_pgs_res" = "Mother childhood maltreatment PGS",
  "mother_ea3_pgs_res" = "Mother educational attainment PGS",
  "mother_insomnia_pgs_res" = "Mother insomnia PGS",
  "mother_intell_pgs_res" = "Mother intelligence PGS",
  "mother_mdd_pgc2024_pgs_res" = "Mother depression PGS",
  "mother_ptsd_pgs_res" = "Mother PTSD PGS",
  "mother_scz2022_pgs_res" = "Mother schizophrenia PGS",
  
  "father_ADHD2022_pgs_res" = "Father ADHD PGS",
  "father_an_pgc3_pgs_res" = "Father Anorexia Nervosa PGS",
  "father_anx_pgs_res" = "Father anxiety PGS",
  "father_asd_pgs_res" = "Father autism PGS",
  "father_bip2021_pgs_res" = "Father bipolar disorder PGS",
  "father_chmaltreat_pgs_res" = "Father childhood maltreatment PGS",
  "father_ea3_pgs_res" = "Father educational attainment PGS",
  "father_insomnia_pgs_res" = "Father insomnia PGS",
  "father_intell_pgs_res" = "Father intelligence PGS",
  "father_mdd_pgc2024_pgs_res" = "Father depression PGS",
  "father_ptsd_pgs_res" = "Father PTSD PGS",
  "father_scz2022_pgs_res" = "Father schizophrenia PGS",
  
  "aadx" = "Age at autism diagnosis",
  "age_diff" = "Age difference with sibling",
  "fylke" = "County",
  "world_region_bg" = "Parental world region of origin"
  
)

outcome_map_childhood <- c("smfq_dep_c_8yr" = "Depression",
                           "scared_anx_c_8yr" = "Anxiety",
                           "life_enjoyment_8yr" = "Life satisfaction",
                           "swls_full_m_8yr" = "Life satisfaction",
                           "scl_anx_m_8yr" = "Anxiety",
                           "scl_dep_m_8yr" = "Depression",
                           "swls_full_f_far2" = "Life satisfaction",
                           "scl_anx_f_far2" = "Anxiety",
                           "scl_dep_f_far2" = "Depression",
                           "sibling_smfq_dep_c_8yr" = "Depression",
                           "sibling_scared_anx_c_8yr" = "Anxiety",
                           "sibling_life_enjoyment_8yr" = "Life satisfaction")

outcome_map_adolescence <- c("swls_full_c_14c" = "Life satisfaction",
                             "smfq_dep_m_14m" = "Depression",
                             "scared_anx_m_14m" = "Anxiety",
                             "swls_full_m_14m" = "Life satisfaction",
                             "scl_anx_m_14m" = "Anxiety",
                             "scl_dep_m_14m" = "Depression",
                             "sibling_swls_full_c_14c" = "Life satisfaction",
                             "sibling_smfq_dep_m_14m" = "Depression",
                             "sibling_scared_anx_m_14m" = "Anxiety")




# Replace the keys in `term` with the mapped values
replace_terms <- function(term, map) {
  for (i in seq_along(term)) {
    if (grepl("^sex1:", term[i])) {
      
      # Extract the variable part and the suffix
      variable <- sub("^sex1:", "", term[i]) 

      for (key in names(map)) {
        pattern <- paste0("(^", key, ")(.*)")
        
        if(grepl(pattern,variable)){
          variable <- gsub(pattern, paste0("Sex1:", map[[key]], "\\2"), variable)
        }
        
      }

      term[i] <- variable
      
    } else {
      # For non-prefixed terms, apply mapping
      for (key in names(map)) {
        pattern <- paste0("(^", key, ")(.*)")
        term[i] <- gsub(pattern, paste0(map[[key]], "\\2"), term[i])
      }
    }
  }
  return(term)
}

###################
# Rename outcomes #
###################

rename_outcome <- function(outcome, outcome_map) {
  for (name in names(outcome_map)) {
    
    # Check if the outcome is listed in the outcome_map list
    if (any(sapply(name, function(var) grepl(var, outcome)))) {
      return(outcome_map[[name]])
    }
  }
  return(NA)  # Return NA if no match is found
}

# Apply the replacement to the dataframe
# results_h1$term <- replace_terms(results_h1$term, map)



# summary_tables_adjusted <- map(summary_tables, replace_terms, map=map)





