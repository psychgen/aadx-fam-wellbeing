library(tidyverse)
library(haven)


if(!file.exists("./data/ssb_df.RData")){
  # Load SSB education data
  ssb_edu <- read.csv("N:\\durable\\data\\SSB\\Utdanning\\W21_5323_BU_UTD.csv")
  head(ssb_edu)
  
  # Load SSB household income data
  ssb_household_income <- read.csv(file = "N:/durable/data/SSB/Inntekt/W21_5323_HUSHINNT.csv")
  head(ssb_household_income)
  
  # Load SSB demographic data
  ssb_region <- read.csv("N:\\durable\\data\\SSB\\FDtrygd\\W21_5323_F_DEMO.csv", colClasses = c(bosted = "character"))
  head(ssb_region)
  
  # Load ssb country of origin
  ssb_origin_country <- read.csv("N:\\durable\\data\\SSB\\FDtrygd\\W21_5323_TAB_FLAN.csv", colClasses = c(LANDBAKG = "character", FODELAND = "character"))
  
  # Load linkage file to MoBa
  linkagefile <- read_spss("N:/durable/data/Linkage_files/SSB_link/PDB2306_kobling_SSB&KUHR_Combined_20220112.sav") 
  head(linkagefile)
  
  # Load country code classification file
  load("./data/ssb_country_codes.RData")

  # Load fylke code classification file
  load("./data/ssb_fylke_codes.RData")
  
  # Cleaning linkage IDs
  linkagefile <- linkagefile %>%
    mutate(PID = str_trim(PID_NPR_2306))
  ssb_edu <- ssb_edu %>%
    mutate(PID = str_trim(PID))
  ssb_household_income <- ssb_household_income %>%
    mutate(PID = str_trim(PID))
  ssb_region <- ssb_region %>%
    mutate(PID = str_trim(PID))
  ssb_origin_country <- ssb_origin_country %>% 
    mutate(PID = str_trim(PID))
  
  
  #############
  # Education #
  #############
  moba_ssb_edu <- ssb_edu %>%
    
    # Removing all redundant columns
    select(PID, matches("^BU_.*(1999|200[0-9])$")) %>%
    
    # Extract "highest level of education" from SSB data
    mutate(across(matches("^BU_.*(1999|200[0-9])$"), ~ {
      first_digit <- as.numeric(substr(as.character(.), 1, 1))
      ifelse(first_digit == 9, 0, first_digit)
    }))
  
  # Turning into long data
  moba_ssb_edu_long <- moba_ssb_edu %>%
    pivot_longer(
      cols = starts_with("BU_"),
      names_to = "year",
      values_to = "education"
    ) %>%
    
    # Re-categorising the NUS level into SSB standard.:
    # https://www.ssb.no/utdanning/utdanningsniva/statistikk/befolkningens-utdanningsniva
    mutate(year = as.numeric(sub("BU_", "", year)),
           education = case_when(
             education == 1 ~ 7,
             education == 2 ~ 10,
             education == 3 ~ 11,
             education == 4 ~ 13,
             education == 5 ~ 13,
             education == 6 ~ 16,
             education == 7 ~ 18,
             education == 8 ~ 21,
             education == 0 ~ 0
           ))
  
  
  
  ##########
  # Income #
  ##########
  
  moba_ssb_household_income_long <- ssb_household_income %>%
    
    # Edit year variable
    mutate(year = as.numeric(aargang)) %>% 
    
    # Select only relevant columns
    select(PID, year, hush_ies) %>% 
    
    # Filter out only relevant years (birth years in MoBa between 1999 and 2009)
    filter(year < 2010)
  
  
  #######################
  # Region of residence #
  #######################
  
  ssb_fylke_codes <- ssb_fylke_codes %>% 
    group_by(code) %>% 
    slice_tail() %>% 
    ungroup()
  
  
  moba_ssb_ror <- ssb_region %>%
    
    # Linking to linkagefile to identify the children
    full_join(linkagefile) %>% 
    mutate(m_id = str_trim(M_ID_2306),
           f_id = str_trim(F_ID_2306),
           ind_id = ifelse(PREG_ID_2306, paste0(str_trim(PREG_ID_2306), "_", str_trim(BARN_NR)), NA_real_),
           SUP_Type = str_trim(SUP_Type)) %>% 
    select(ind_id, m_id, f_id, PID, SUP_Type, kode, regdato, bosted) %>% 
    
    # Extract only the rows where kode equals 1 (new entry into the registry,
    # which happens at birth or at immigration into Norway)
    filter(SUP_Type == "child",
           kode == 1) %>%
    
    # Select the earliest occurrence of an event with kode = 1.
    group_by(ind_id) %>% 
    mutate(regdato = min(as.numeric(substr(regdato, 1, 4)))) %>% 
    ungroup() %>% 
    
    # Remove double registrations
    distinct(ind_id, .keep_all = TRUE) %>%
    
    # Get fylke code
    mutate(fylke_code = substr(bosted, 1, 2)) %>% 
    
    # Get name of fylke
    merge(ssb_fylke_codes, by.x = "fylke_code", by.y = "code") %>% 
    
    # Select relevant columns
    select(PID, fylke=name)
  
  moba_ssb_ror$fylke <- factor(moba_ssb_ror$fylke, levels = c("Oslo", setdiff(unique(x), "Oslo")))
  
  ##########################
  # World region of origin #
  ##########################

  
  moba_ssb_origin_country <- ssb_origin_country %>%
    
    # Joining linkagefile to identify children
    left_join(linkagefile) %>% 
    mutate(SUP_Type = str_trim(SUP_Type)) %>% 
    filter(SUP_Type == "child") %>% 
    
    # Adding countries and world regions to the country code for child's 
    # background
    merge(ssb_country_codes, by.x = "LANDBAKG", by.y = "country_code") %>% 
    select(PID, FODELAND, country_bg=country_name, world_region_bg=region_level1_name) %>% 
    
    # Adding countries and world regions to the country code for mother's 
    # country of residence during child's birth.
    merge(ssb_country_codes, by.x = "FODELAND", by.y = "country_code") %>% 
    select(PID, country_bg, world_region_bg, country_birth=country_name, world_region_birth=region_level1_name)
  
  country_vars <- c("country_bg", "world_region_bg", "country_birth", "world_region_birth")
  
  # Loop through each variable and set "Norway" as the reference category
  moba_ssb_origin_country[country_vars] <- lapply(moba_ssb_origin_country[country_vars], function(x) {
    factor(x, levels = c("Norge", setdiff(unique(x), "Norge")))
  })
  

  
  ####################
  # Linking datasets #
  ####################
  
  
  ssb_df <- moba_ssb_household_income_long %>% 
    full_join(moba_ssb_edu_long, by = c("PID", "year")) %>% 
    full_join(moba_ssb_ror, by = "PID") %>% 
    full_join(moba_ssb_origin_country, by = "PID") %>% 
    full_join(linkagefile) %>% 
    mutate(m_id = str_trim(M_ID_2306),
           f_id = str_trim(F_ID_2306),
           ind_id = ifelse(PREG_ID_2306, paste0(str_trim(PREG_ID_2306), "_", str_trim(BARN_NR)), NA_real_)) %>% 
    select(ind_id, m_id, f_id, SUP_Type, year, hush_ies, education, fylke, country_bg, world_region_bg, country_birth, world_region_birth) %>%
    distinct()
  
  save(ssb_df, file="./data/ssb_df.RData")
} else {
  load(file="data/ssb_df.RData")
}
