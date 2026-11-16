library(tidyverse)
library(haven)

if(!file.exists("data/pheno_df.RData")){
  load(file = "./data/pheno_df_raw.RData")
  
  # Defining functions
  
  # Removing outliers beyond 3SD from the mean
  remove_outliers <- function(x) {
    x[abs(x - mean(x, na.rm = TRUE)) > 3 * sd(x, na.rm = TRUE)] <- NA   # Set 0 to NA
    return(x)
  }
  
  # Basic processing for individual items, setting 0 to NA, and adjusting all 
  # other items so 0 is the minimum.
  basic_processing <- function(x) {
    x[x == 0] <- NA   # Set 0 to NA
    x <- x - 1        # Subtract 1 from each value
    return(x)
  }
  
  # Reversing items
  reverse_values <- function(x) {
    x <- max(x, na.rm = TRUE) - x
    return(x)
  }
  
  # Turning all diagnosis variables into binary variables (i.e., 1=diagnosis in 
  # NPR, 0 = no diagnosis)
  dx_processing <- function(x) {
    x[x != "yes"] <- as.numeric(0)   # Set "no" or missing to 0
    x[x == "yes"] <- as.numeric(1)   # Set "yes" to 1
    return(as.factor(x))
  }
  
  
  # Recoding and average score calculation
  # All pregnancies have their own unique ID number (preg_id). But children born 
  # from the same pregnancy (i.e., twins, triplets, etc.) share a preg_id, but
  # have their own BARN_NR (i.e., 1, 2, etc.). For each dataset that will be 
  # merged, we create a unique ind_id which consists of the preg_id and BARN_NR,
  # to ensure that we do not get any duplicates.
  
  pheno_df <- pheno_df_raw %>%
    
    # Setting BARN_NR to 1 for those that are missing
    mutate(BARN_NR = ifelse(is.na(BARN_NR), 1, BARN_NR),
           preg_id = as.numeric(preg_id)) %>%
    mutate(ind_id = paste0(str_trim(preg_id), "_", str_trim(BARN_NR)))
  
  # Individual items that need basic processing
  ind_scale_items <- c("LL213_raw", # CCC-2 coherence subscale
                       "LL214_raw", 
                       "LL215_raw", 
                       "LL216_raw", 
                       "LL217_raw", 
                       "LL218_raw",
                       "GG226_raw",
                       "GG239_raw",
                       "GG255_raw", # Use short phrases or sentences age 3
                       "LL76_raw", # Delayed language development age 5
                       "LL78_raw", # Delayed motor development age 5
                       "LL79_raw", # Language development confirmed by doctor
                       "LL81_raw", # Motor development confirmed by doctor
                       "GG450_raw", # Social support age 3
                       "GG451_raw", # Social support freq age 3
                       "NN333_raw", # Social support age 8
                       "NN334_raw", # Social support freq age 8
                       "UM347_raw", # Social support age 14
                       "UM348_raw", # Social support freq age 14
                       "GG380_raw", # Concerns age 3
                       "GG381_raw",
                       "GG382_raw",
                       "GG594_raw",
                       "GG595_raw",
                       "LL326_raw", # Concerns age 5
                       "LL327_raw",
                       "LL328_raw",
                       "LL329_raw",
                       "LL330_raw",
                       "LL331_raw",
                       "EE488_raw", # Live together with child's father - ultimately we want 0 = yes, 1 = no.
                       "EE584_raw", # Financial problems 18m
                       "UB36_raw", # individual swls items
                       "UB37_raw", # individual swls items
                       "UB38_raw", # individual swls items
                       "UB39_raw", # individual swls items
                       "UB40_raw", # individual swls items
                       "UM178_raw", # individual sMFQ items
                       "UM179_raw", # individual sMFQ items
                       "UM180_raw", # individual sMFQ items
                       "UM181_raw", # individual sMFQ items
                       "UM182_raw", # individual sMFQ items
                       "UM183_raw", # individual sMFQ items
                       "UM184_raw", # individual SCARED items
                       "UM185_raw", # individual SCARED items
                       "UM186_raw", # individual SCARED items
                       "UM187_raw", # individual SCARED items
                       "UM188_raw", # individual SCARED items
                       "EE871_raw", # Referred to habilitation service 18mo
                       "EE873_raw", # Referrred to child psychiatric outpationt clinic/dpeartment 18mo
                       "GG173_raw", # Referred to habilitation service 3yr
                       "GG175_raw", # Referred to Child psychiatric clinic/department 3yr
                       "LL88_raw", 
                       "LL89_raw",
                       "LL91_raw",
                       "LL92_raw",
                       "NN55_raw", # Referred to specialist for autistic traits/autism/Asperger's Syndrome 8yr
                       "UM119_raw",
                       "UM122_raw"
  )
  
  # Reverse necessary items
  rev_items <- c("LL217_raw",
                 "LL218_raw")
  
  # Variables for which outliers should be removed
  outlier_vars <- c("EE987_raw", # Age at onset walking 18mo
                    "GG27_raw", # Age at onset walking 3yr
                    "MORS_ALDER_raw", # Maternal age at birth
                    "FARS_ALDER_raw" # Paternal age at birth
  )
  
  pheno_df <- pheno_df %>% 
    mutate(across(all_of(ind_scale_items), basic_processing),
           across(all_of(rev_items), reverse_values),
           across(all_of(outlier_vars), remove_outliers))
  
  
  # Calculating scores for questionnaires
  pheno_df <- pheno_df %>%
    mutate(swls_full_c_14c = rowSums(select(., UB36_raw, UB37_raw, UB38_raw, UB39_raw, UB40_raw)),
           smfq_dep_m_14m = rowSums(select(., UM178_raw, UM179_raw, UM180_raw, UM181_raw, UM182_raw, UM183_raw)),
           scared_anx_m_14m = rowSums(select(., UM184_raw, UM185_raw, UM186_raw, UM187_raw, UM188_raw)),
           ccc2_coherence_c_5yr_manual = rowSums(select(., LL213_raw, LL214_raw, LL215_raw, LL216_raw, LL217_raw, LL218_raw)),
           life_enjoyment_8yr = as.numeric(case_when(NN99_raw==1~0,
                                          NN99_raw==2~1, 
                                          NN99_raw==3~2,
                                          NN99_raw==4~3,
                                          NN99_raw==5~4,
                                          NN99_raw==0~NA_real_))) 
  
  
  # Item-specific processing
  
  # Recode sex to be a binary variable (boy = 0, girl = 1).
  pheno_df <- pheno_df %>%
    mutate(sex = as.factor(case_when(KJONN_raw == 1~0,
                                     KJONN_raw == 2~1)))
  
  
  # Combining age at onset walking items
  pheno_df <- pheno_df %>%
    mutate(age_onset_walking = ifelse(!is.na(EE987_raw), EE987_raw, ifelse(!is.na(GG27_raw), GG27_raw, NA_real_)))
  

  
  # Recode maternal concerns
  
  pheno_df <- pheno_df %>% 
    mutate(concerns_3yr = as.factor(if_else(rowSums(select(., GG380_raw, GG381_raw, GG382_raw, GG594_raw, GG595_raw)) > 0, 1, 0)),
           concerns_5yr = as.factor(if_else(rowSums(select(., LL326_raw, LL327_raw, LL328_raw, LL329_raw, LL330_raw, LL331_raw)) > 0, 1, 0)))
  
  # Recoding received_dx and received_dx_2x
  dx_vars <- names(pheno_df)[grepl("^received_dx_", names(pheno_df))]
  
  pheno_df <- pheno_df %>%
    mutate(across(all_of(dx_vars), dx_processing))
  
  # Convert the date of first dx to date format
  pheno_df <- pheno_df %>%
    mutate(
      date_first_dx_asd_npr = as.Date(date_first_dx_asd_npr, origin = "1970-01-01")
    )
  
  
  # Recode language skills Q3yr - convert composite values to highest value
  response_map <- list(
    '1' = c(1), '2' = c(2), '3' = c(3), '4' = c(4), '5' = c(5), '6' = c(6),
    '7' = c(1, 2), '8' = c(1, 3), '9' = c(1, 4), '10' = c(1, 5), '11' = c(1, 6),
    '12' = c(2, 3), '13' = c(2, 4), '14' = c(2, 5), '15' = c(2, 6),
    '16' = c(3, 4), '17' = c(3, 5), '18' = c(3, 6),
    '19' = c(4, 5), '20' = c(4, 6),
    '21' = c(5, 6),
    '22' = c(1, 2, 3), '23' = c(1, 2, 4), '24' = c(1, 2, 5), '25' = c(1, 2, 6),
    '26' = c(1, 3, 4), '27' = c(1, 3, 5), '28' = c(1, 3, 6),
    '29' = c(1, 4, 5), '30' = c(1, 4, 6),
    '31' = c(1, 5, 6),
    '32' = c(2, 3, 4), '33' = c(2, 3, 5), '34' = c(2, 3, 6),
    '35' = c(2, 4, 5), '36' = c(2, 4, 6),
    '37' = c(2, 5, 6),
    '38' = c(3, 4, 5), '39' = c(3, 4, 6),
    '40' = c(3, 5, 6),
    '41' = c(4, 5, 6),
    '42' = c(1, 2, 3, 4), '43' = c(1, 2, 3, 5), '44' = c(1, 2, 3, 6),
    '45' = c(1, 2, 4,5), '46' = c(1, 2, 4, 6),
    '47' = c(1, 2, 5, 6),
    '48' = c(1, 3, 4, 5), '49' = c(1, 3, 4, 6),
    '50' = c(1, 3, 5, 6),
    '51' = c(1, 4, 5, 6),
    '52' = c(2, 3, 4, 5), '53' = c(2, 3, 4, 6),
    '54' = c(2, 3, 5, 6),
    '55' = c(2, 4, 5, 6),
    '56' = c(3, 4, 5, 6),
    '57' = c(1, 2, 3, 4, 5), '58' = c(1, 2, 3, 4, 6),
    '59' = c(1, 2, 3, 5, 6),
    '60' = c(1, 2, 4, 5, 6),
    '61' = c(1, 3, 4, 5, 6),
    '62' = c(1, 2, 3, 4, 5, 6)
  )
  
  # Define a function to decode the highest response option
  decode_max_value <- function(value) {
    if (value %in% 1:62) {
      return(max(response_map[[as.character(value)]]))
    } else {
      return(NA)
    }
  }
  
  pheno_df$language_skills_1_3yr <- sapply(pheno_df$GG226_raw, decode_max_value)
  
  
  # Recode language skills 2 - sentences of 3-4 words long
  
  pheno_df <- pheno_df %>% 
    mutate(language_skills_2_3yr = case_when(
      GG239_raw == 0 ~ 0,
      GG239_raw == 1 ~ 1,
      GG239_raw == 2 ~ 1
    ))
  
  ###############################
  # Checking language variables #
  ###############################
  
  library(psych)
  library(knitr)
  asd_df <- pheno_df %>% 
    filter(received_dx_2x_asd_npr == 1)
  
  subset <- asd_df[, c("language_skills_1_3yr",
                       "language_skills_2_3yr",
                         "GG255_raw")]
  subset$language_skills_1_3yr <- as.numeric(asd_df$language_skills_1_3yr)
  subset$GG255_raw <- as.numeric(asd_Df$GG255_raw)
  subset$language_skills_2_3yr <- as.numeric(asd_df$language_skills_2_3yr)
  
  cor_matrix <- cor(subset, method = "pearson", use = "complete.obs")
  
  kable(cor_matrix)
  
  subset2 <- subset[, c("GG255_raw",
                        "GG239_raw")]


  
  subset2$GG239_raw <- as.numeric(pheno_df$language_skills_2_3yr)
  subset2$GG255_raw <- as.numeric(pheno_df$GG255_raw)
  
  tetra_matrix <- tetrachoric(subset, na.rm = TRUE)
  
  kable(tetra_matrix$rho)
  
  
  
  # Autism MoBa
  
  pheno_df <- pheno_df %>% 
    mutate(
      habilitation_18mo = EE871_raw,
      psych_clinic_18mo = EE873_raw,
      habilitation_3yr = GG173_raw,
      psych_clinic_3yr = GG175_raw,
      autism_moba_5yr = ifelse(LL88_raw == 1 | LL91_raw == 1, 1, 0),
      autism_confirmed_5yr = ifelse(LL89_raw == 1 | LL92_raw == 1, 1, 0),
      autism_moba_8yr = case_when(
        NN52_raw == 1 ~ 0,
        NN53_raw == 1 ~ 1),
      autism_confirmed_8yr = NN55_raw,
      autism_moba_14yr = UM119_raw,
      autism_referral_14yr = UM122_raw
    )
  
  
  # Other variables
  
  pheno_df$birth_yr <- as.numeric(pheno_df$birth_yr)
  pheno_df$parity <- as.numeric(pheno_df$PARITET_5_raw)
  pheno_df$m_age_at_birth <- as.numeric(pheno_df$MORS_ALDER_raw)
  pheno_df$p_age_at_birth <- as.numeric(pheno_df$FARS_ALDER_raw)
  
  
  
  # Creating a separate outcomes dataframe
  outcomes <- list(child_outcomes_8yr = c("smfq_dep_c_8yr",
                                          "scared_anx_c_8yr",
                                          "NN99"
  ),
  child_outcomes_14yr = c("UB36", # individual swls items
                          "UB37", # individual swls items
                          "UB38", # individual swls items
                          "UB39", # individual swls items
                          "UB40", # individual swls items
                          "UM178", # individual sMFQ items
                          "UM179", # individual sMFQ items
                          "UM180", # individual sMFQ items
                          "UM181", # individual sMFQ items
                          "UM182", # individual sMFQ items
                          "UM183", # individual sMFQ items
                          "UM184", # individual SCARED items
                          "UM185", # individual SCARED items
                          "UM186", # individual SCARED items
                          "UM187", # individual SCARED items
                          "UM188", # individual SCARED items
                          "smfq_dep_c_14c", # self-report (not item of interest)
                          "scared_anx_c_14c" #self-report (not item of interest)
  ),
  mother_outcomes_8yr = c("swls_full_m_8yr",
                          "scl_anx_m_8yr",
                          "scl_dep_m_8yr"
  ),
  mother_outcomes_14yr = c("swls_full_m_14m",
                           "scl_anx_m_14m",
                           "scl_dep_m_14m"
  ),
  father_outcomes_2015 = c("swls_full_f_far2",
                           "scl_anx_f_far2",
                           "scl_dep_f_far2"
  )
  )
  

  
  
  
  
  
  # Final selection of pheno_df variables
  
  pheno_df <- pheno_df %>%
    select(ind_id,
           m_id,
           f_id,
           sex,
           birth_yr,
           parity,
           m_age_at_birth,
           p_age_at_birth,
           parent_cohab_18m=EE488_raw, #Use over cohab measures from Q1
           financ_probs_18m=EE584_raw,
           concerns_3yr,
           concerns_5yr,
           social_support_3yr=GG450_raw,
           social_support_freq_3yr=GG451_raw,
           social_support_8yr=NN333_raw,
           social_support_freq_8yr=NN334_raw,
           social_support_14yr=UM347_raw,
           social_support_freq_14yr=UM348_raw,
           age_onset_walking,
           language_skills_1_3yr,
           language_skills_2_3yr,
           language_skills_3_3yr=GG255_raw,
           scq_sci_c_3yr,
           scq_rrb_c_3yr,
           cbcl_ext_c_3yr,
           esat_short_c_3yr,
           mchat_short_c_q3yr,
           motor_dev_5yr=LL76_raw,
           language_dev_5yr=LL79_raw,
           # ccc2_coherence_c_5yr,
           ccc2_coherence_c_5yr_manual,
           cbcl_ext_c_5yr,
           received_dx_asd_npr,
           received_dx_2x_asd_npr,
           childageyrs_first_dx_asd_npr,
           year_first_dx_asd_npr,
           received_dx_cp_npr,
           received_dx_2x_cp_npr,
           received_dx_ep_npr,
           received_dx_2x_ep_npr,
           received_dx_id_npr, 
           received_dx_2x_id_npr, 
           smfq_dep_c_8yr,
           scared_anx_c_8yr,
           life_enjoyment_8yr,
           swls_full_c_14c,
           smfq_dep_c_14c,
           smfq_dep_m_14m,
           scared_anx_c_14c,
           scared_anx_m_14m,
           swls_full_m_8yr,
           scl_anx_m_8yr,
           scl_dep_m_8yr,
           swls_full_m_14m,
           scl_anx_m_14m,
           scl_dep_m_14m,
           swls_full_f_far2,
           scl_anx_f_far2,
           scl_dep_f_far2
           )
  
  save(pheno_df, file="data/pheno_df.RData")
} else {
  load(file="data/pheno_df.RData")
}


