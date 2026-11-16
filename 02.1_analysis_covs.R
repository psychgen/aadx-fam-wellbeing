covariates <- list("Birth year" = c("birth_yr"),
                   "Development and behaviour" = c("age_onset_walking",
                                                   "language_skills_1_3yr",
                                                   "scq_sci_c_3yr",
                                                   "scq_rrb_c_3yr",
                                                   "cbcl_ext_c_3yr",
                                                   "motor_dev_5yr",
                                                   "language_dev_5yr",
                                                   "ccc2_coherence_c_5yr_manual",
                                                   "cbcl_ext_c_5yr",
                                                   "concerns_3yr",
                                                   "concerns_5yr"),
                   "NPR" = c("received_dx_2x_id_npr",
                             "received_dx_2x_ep_npr",
                             "received_dx_2x_cp_npr"),
                   "Sociodemographic factors" = c("sex",                     
                                                  "m_age_at_birth", # Mother's age at birth
                                                  "p_age_at_birth", # Father's age at birth
                                                  "parity", # Mother's parity
                                                  "m_edu",
                                                  "f_edu",
                                                  "c_household_income",
                                                  "parent_cohab_18m", #Use over cohab measures from Q1
                                                  "financ_probs_18m",
                                                  "world_region_bg",
                                                  "fylke"),
                   "Social support" = c("social_support_3yr", 
                                        "social_support_freq_3yr", 
                                        "social_support_8yr", 
                                        "social_support_freq_8yr",
                                        "social_support_14yr", 
                                        "social_support_freq_14yr"),
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
                                   "child_ptsd_pgs_res",
                                   "child_scz2022_pgs_res"),
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
                                    "mother_ptsd_pgs_res",
                                    "mother_scz2022_pgs_res"),
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
                                    "father_ptsd_pgs_res",
                                    "father_scz2022_pgs_res")
)

covariates_sibling <- covariates

covariates_sibling[["Sibling PGS"]] <- c("sibling_child_ADHD2022_pgs_res",
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
                                         "sibling_child_scz2022_pgs_res")

covariates_sibling[["Birth year"]] <- c(covariates_sibling[["Birth year"]], "age_diff")

covariates_sibling <- covariates_sibling[c("Birth year",
                                           "Development and behaviour",
                                           "NPR",
                                           "Sociodemographic factors",
                                           "Social support",
                                           "Sibling PGS",
                                           "Child PGS",
                                           "Mother PGS",
                                           "Father PGS")]

outcomes_childhood <- list(child_outcomes_8yr = c("smfq_dep_c_8yr",
                                        "scared_anx_c_8yr",
                                        "life_enjoyment_8yr"),
                 mother_outcomes_8yr = c("swls_full_m_8yr",
                                         "scl_anx_m_8yr",
                                         "scl_dep_m_8yr"),
                 father_outcomes_2015 = c("swls_full_f_far2",
                                          "scl_anx_f_far2",
                                          "scl_dep_f_far2")
)

sibling_outcomes_childhood <- c("sibling_smfq_dep_c_8yr",
                                "sibling_scared_anx_c_8yr",
                                "sibling_life_enjoyment_8yr")


outcomes_adolescence <- list(child_outcomes_14yr = c("swls_full_c_14c",
                                                     "smfq_dep_m_14m",
                                                     "scared_anx_m_14m"),
                             mother_outcomes_14yr = c("swls_full_m_14m",
                                                      "scl_anx_m_14m",
                                                      "scl_dep_m_14m")
)

sibling_outcomes_adolescence <- c("sibling_swls_full_c_14c",
                                  "sibling_smfq_dep_m_14m",
                                  "sibling_scared_anx_m_14m")



