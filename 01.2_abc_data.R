library(dplyr)
library(haven)

if(!file.exists("./data/abc_df.RData")){
  # Creating variable list
  abc_vars <- c("ind_id", # pregnancy ID to link datasets
                "clin_ass_year", # year of clinical assessment
                "received_dx" # received dx at clinical assessment
  )
  
  # Loading dataset
  abc_df <- haven::read_spss("N:/durable/data/ABC/PDB2306_ABC_ANALYTIC.sav")
  
  # Renaming preg_id variable to link to other dataset, selecting only vars of interest
  abc_df <- abc_df %>%
    mutate(preg_id = as.numeric(PREG_ID_2306),
           ind_id = paste0(str_trim(PREG_ID_2306), "_", str_trim(BARN_NR)), 
           clin_ass_year = as.numeric(ASSYEAR),
           received_dx = ifelse(!is.na(E__1_1_1) & E__1_1_1 == 1, 1, 0)) %>% 
    select(all_of(abc_vars))
  
  save(abc_df, file="data/abc_df.RData")
} else {
  load(file="./data/abc_df.RData")
}
