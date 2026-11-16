#01_data_prep.R

# To do:
#   
#   - For all missing Barn_nr: check if duplicated (probably not), then set to 1


# This script for pulling in and preparing data... (etc)

# Load libraries

library(phenotools)
library(tidyverse)
library(haven)
library(lubridate)

###########################
# Selecting variables
###########################

# covariate Variable selection

covariates <- list(child_related_cov = c("KJONN", # child's gender
                                         "EE987", # If yes, how old (18mo)
                                         "GG27", # Age at onset of walking
                                         "GG226", # Language skills
                                         "GG239", # Sentences of 3-4 words long
                                         "GG255", # Able to talk short sentences
                                         "scq_sci_c_3yr", # SCQ social comm impairment 3yr
                                         "scq_rrb_c_3yr", # SCQ restricted and repetitive behaviours 3yr
                                         "cbcl_ext_c_3yr", # CBCL ext. 3yr
                                         "esat_short_c_3yr", # Early screening of autistic traits 3yr - aadx imputation
                                         "mchat_short_c_q3yr", # MCHAT short 3yr - aadx imputation
                                         "LL76", # Delayed motor development/clumsy Q5yr
                                         "LL77", # Confirmed by doctor?
                                         "LL78", # Does your child still have the problem?
                                         "LL79", # Delayed/deviant language development Q5yr
                                         "LL80", # Confirmed by doctor?
                                         "LL81", # Does your child still have the problem?
                                         "ccc2_coherence_c_5yr", # CCC2 coherence subscale Q5yr
                                         "LL213", # Individual CCC2 coherence items.
                                         "LL214", 
                                         "LL215", 
                                         "LL216", 
                                         "LL217", 
                                         "LL218",
                                         "cbcl_ext_c_5yr" # CBCL externalising Q5yr
),
socio_fam_cov = c("MORS_ALDER", # Mother's age at birth
                  "FARS_ALDER", # Father's age at birth
                  "PARITET_5", # Mother's parity
                  #"AA1305", # Other languages spoken by either parent 
                  #"AA1309", # Other languages spoken by grandparent
                  "EE488", # "Do you and your child live with your child's father?"
                  "EE584", # financial difficulties
                  "GG380", # maternal concerns Q3yr
                  "GG381",
                  "GG382",
                  "GG594",
                  "GG595",
                  "LL326", # maternal concerns Q5yr
                  "LL327", 
                  "LL328",
                  "LL329",
                  "LL330", 
                  "LL331",
                  "GG450", # Social support: How many people other than your partner Q3yr
                  "GG451", # Social support: How often do you see/phone others Q3yr
                  "NN333", # Social support: How many people other than your partner Q8yr
                  "NN334", # Social support: How often do you see/phone others Q8yr
                  "UM347", # Social support: How many people other than your partner Q14yr
                  "UM348" # Social support: How often do you see/phone others Q14yr
),
autism_moba = c("EE871", # Referred to habilitation service? 18mo
                "EE873", # Referred to child psychiatric clinic/department? 18mo
                "GG173", # Referred to habilitation service? 3yr
                "GG175", # Referred to child psychiatric clinic/department? 3yr
                "LL88", # Does the child have/had autism? 5yr
                "LL89", # Confirmed by doctor/psychologist?
                "LL91", # Asperger's 5yr
                "LL92", # Confirmed by doctor/psychologist?
                "JJ436", # Does the child have autistic characteristics/autism? 7yr
                "JJ437", # Does the child have Asperger's? 7yr
                "NN52", # Does the child have autistic characteristics/autism/Asperger's? 8yr - No
                "NN53", # Yes, currently
                "NN55", # Referred to specialist?
                "UM119", # Has child (had) autistic traits/autism/Asperger's 14yr
                "UM122" # Referred to specialist?
)
)


# outcome variable selection

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
# NPR codes

npr_codes <- c("id=F70,F71,F72,F73,F78,F79", # Intellectual disability
               "asd=F840,F841,F845,F848,F849", # Autism
               "ep=G40", # Epilepsy
               "cp=G80" # Cerebral palsy
)

#   NOT AVAILABLE IN NPR YET
#   Check MoBa questionnaire data
#                "Q90", # Down syndrome
#                "Q91", # Trisomy 18 and Trisomy 13
#                "Q92", # Other trisomies and partial trisomies of the autosomes, not elsewhere classified
#                "Q93", # Monosomies and deletions from the autosomes, not elsewhere classified
#                "Q95", # Balanced rearrangements and structural markers, not elsewhere classified
#                "Q96", # Turner's Syndrome
#                "Q97", # Other sex chromosome abnormalities, female phenotype, not elsewhere classified
#                "Q98", # Other sex chromosome abnormalities, male phenotype, not elsewhere classified
#                "Q99"  # Other chromosome abnormalities, not elsewhere classified

###########################
# Creating dataset
###########################
if(!file.exists("data/pheno_df_raw.RData")){
  
  pheno_df_raw <- curate_dataset(variables_required = list(moba = c(covariates$child_related_cov, 
                                                                    covariates$socio_fam_cov,
                                                                    covariates$autism_moba,
                                                                    outcomes$child_outcomes_8yr,
                                                                    outcomes$child_outcomes_14yr,
                                                                    outcomes$mother_outcomes_8yr,
                                                                    outcomes$mother_outcomes_14yr,
                                                                    outcomes$father_outcomes_2015),
                                                           npr = npr_codes), 
                                 out_format = "merged_df",
                                 recursive = T,
                                 dx_owners = "child",
                                 return_items = T)
  
  
  save(pheno_df_raw, file="data/pheno_df_raw.RData")
} else {
  load(file="data/pheno_df_raw.RData")
}

# Creating separate outcome_df for calculating internal consistency

if(!file.exists("data/outcome_df.RData")){
  
  outcome_df <- pheno_df_raw |> 
    
    # Setting BARN_NR to 1 for those that are missing
    mutate(BARN_NR = ifelse(is.na(BARN_NR), 1, BARN_NR),
           preg_id = as.numeric(preg_id)) %>%
    mutate(ind_id = paste0(str_trim(preg_id), "_", str_trim(BARN_NR))) |> 
    select(c(which(sapply(names(pheno_df_raw), function(x) any(grepl(paste(unname(unlist(outcomes)), collapse="|"), x))))),
           received_dx_2x_asd_npr,
           ind_id,
           m_id)
  
  save(outcome_df, file = "./data/outcome_df.RData")
  
} else {
  load(file="data/outcome_df.RData")
}


# Preprocessing pheno_df

source("./scripts/01.1_pheno_data_proc.R")


###############################
# Loading and joining ABC data
###############################

source("./scripts/01.2_abc_data.R")

# Joining datasets
full_df <- left_join(pheno_df, abc_df, by = "ind_id")

# Creating a new age at diagnosis variable (asd_aadx)

full_df <- full_df %>% 
  mutate(
    clin_ass_age = clin_ass_year - birth_yr,
    aadx = case_when(
      birth_yr >= 2006 ~ childageyrs_first_dx_asd_npr,
      received_dx == 1 & clin_ass_age < childageyrs_first_dx_asd_npr ~ clin_ass_age,
      year_first_dx_asd_npr >= 2010 ~ childageyrs_first_dx_asd_npr,
      TRUE ~ NA_real_
    ),
    aadx_source = case_when(
      birth_yr >= 2006 ~ 1,
      received_dx == 1 & clin_ass_age < childageyrs_first_dx_asd_npr ~ 2,
      year_first_dx_asd_npr >= 2010 ~ 3,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(-clin_ass_year, - clin_ass_age, -received_dx)


###########################
# Adding SSB data
###########################

source("./scripts/01.3_ssb.R")

full_df <- full_df %>%
  left_join(ssb_df %>% 
              filter(SUP_Type == "child") %>% 
              select(ind_id, year, hush_ies, fylke, world_region_bg), 
            by = c("ind_id" = "ind_id", "birth_yr" = "year")) %>%
  rename(c_household_income = hush_ies)

full_df <- full_df %>%
  left_join(ssb_df %>% 
              filter(SUP_Type == "mother") %>% 
              select(m_id, year, education), 
            by = c("m_id" = "m_id", "birth_yr" = "year")) %>%
  rename(m_edu = education)

full_df <- full_df %>%
  left_join(ssb_df %>% 
              filter(SUP_Type == "father") %>% 
              select(f_id, year, education), 
            by = c("f_id" = "f_id", "birth_yr" = "year")) %>%
  rename(f_edu = education)



###########################
# Adding PGSs
###########################

source("./scripts/01.4_pgs.R")

full_df <- full_df %>%
  left_join(pgs_df_procd %>% 
              filter(Role=="Child") %>% 
              select(ind_id,`ADHD2022_pgs_res`:`scz2022_pgs_res`) %>% 
              rename_with(~ paste0("child_",.), `ADHD2022_pgs_res`:`scz2022_pgs_res`)) %>% 
  left_join(pgs_df_procd %>% 
              filter(Role=="Mother") %>% 
              select(m_id,`ADHD2022_pgs_res`:`scz2022_pgs_res`) %>% 
              rename_with(~ paste0("mother_",.), `ADHD2022_pgs_res`:`scz2022_pgs_res`)) %>% 
  left_join(pgs_df_procd %>% 
              filter(Role=="Father") %>% 
              select(f_id,`ADHD2022_pgs_res`:`scz2022_pgs_res`) %>% 
              rename_with(~ paste0("father_",.), `ADHD2022_pgs_res`:`scz2022_pgs_res`))


save(full_df, file="./data/full_df_proc.RData")

# Multiple imputation
source("./scripts/01.5_multiple_imputation.R")



###################
# Sibling dataset #
###################

load("./data/full_df_proc.RData")

# Creating a df with only children who received an autism diagnosis.

full_df_asd <- full_df %>%
  filter(received_dx_2x_asd_npr == 1)

save(full_df_asd, file="./data/full_df_asd.RData")

load("./data/full_df_asd.RData")
# Creating a sibling dataset

# Extracting all unique mother IDs in our asd df
m_ids_asd <- full_df_asd %>% 
  dplyr::select(m_id) %>% 
  distinct()


# Selecting an index child for each m_id by selecting all children with a
# diagnosis. If they are the only child with a diagnosis for their m_id, they 
# automatically become the index child. If there are multiple children with a 
# diagnosis, one child randomly gets assigned as the index child.
set.seed(123)

index_children_df <- full_df_asd %>% 
  
  # Selecting by m_id to get all autistic children within one family
  group_by(m_id) %>% 
  
  # select one of these children to be the index child
  slice_sample(n=1) %>% 
  ungroup() %>% 
  
  # Rename ind_id to index_id
  mutate(index_id = ind_id)


# Creating a df with all children who are part of a family with an autistic
# child (autistic children and their siblings).
siblings_df <- full_df %>% 
  inner_join(m_ids_asd, by = "m_id") %>% 

  # Adding the index_ids of all index children to the siblings_df, then 
  # removing the actual index children to keep only the siblings
  
  inner_join(index_children_df %>% dplyr::select(index_id, m_id), by = "m_id") %>% 
  group_by(m_id) %>% 
  filter(ind_id != index_id) %>% 
  ungroup() %>% 
  
  # Selecting only the relevant sibling variables
  
  dplyr::select(ind_id,
         m_id,
         index_id,
         sex,
         birth_yr,
         life_enjoyment_8yr,
         smfq_dep_c_8yr,
         scared_anx_c_8yr,
         swls_full_c_14c,
         smfq_dep_m_14m,
         scared_anx_m_14m,
         child_ADHD2022_pgs_res:child_scz2022_pgs_res
  ) %>% 
  
  # Adding "sibling_" prefix to all variable names, except ind_id, index_id, and
  # m_id.
  rename_with(~paste0("sibling_", .), -c(ind_id, index_id, m_id)) 

full_df_siblings <- siblings_df %>% 
  left_join(index_children_df %>% 
              dplyr::select(-ind_id, -m_id), 
            by = "index_id") %>%
  
  # Add age difference variable so that if the sibling is older, the age
  # difference is positive.
  mutate(age_diff = sibling_birth_yr - birth_yr)

save(full_df_siblings, file = "./data/full_df_siblings.RData")

# Multiple imputation of sibling dataset
source("./scripts/01.6_multiple_imputation_siblings.R")
