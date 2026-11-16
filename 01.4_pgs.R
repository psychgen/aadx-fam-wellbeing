library(genotools)
library(tidyverse)

if(!file.exists("data/pgs_df_procd.RData")){
  pgs_phenotypes <- c("ADHD2022", # ADHD
                      "an_pgc3", # Anorexia Nervosa
                      "anx",
                      "asd", # Autism
                      "bip2021", # Bipolar
                      "chmaltreat", # Childhood maltreatment
                      #"cross_dis", # Cross-disorder
                      "ea3",
                      "insomnia", # Insomnia
                      "intell",
                      "mdd_pgc2024", # Depression (major depressive disorder)
                      "ptsd",
                      "scz2022" # Schizophrenia
                      
                      # Not yet available
                      # "ptsd",
                      # "cross_dis"

  )
  
  pgs_df <- fetch_pgs(pgs_list = pgs_phenotypes,
                      geno_data = "MoBaPsychGen_v1-ec-eur-batch-basic-qc",
                      pgs_directory = "//ess01/P471/data/durable/projects/asd_aodx_famwb/pgs/p471-berntgla",
                      pgs_software = "ldpred2",
                      pgs_meta = genotools::pgs_metadata)
  
  pgs_df_procd <- process_pgs(pgs_df, 
                             regress_pgs = TRUE)

  pgs_df_procd <- pgs_df_procd %>%
    mutate(preg_id = as.numeric(preg_id),
           ind_id = paste0(str_trim(preg_id), "_", str_trim(BARN_NR)))                          
  
  library(ggplot2)
  library(ggcorrplot)
  
  # Replace 'mydata' with the name of your dataset
  # Exclude ID variables (ID1, ID2, ID3) and handle NAs
  
  # Step 1: Select all columns except the ID variables
  pgs_df_procd_corr <- pgs_df_procd %>% 
    select(ADHD2022,
           an_pgc3_pgs,
           anx,
           asd_pgs, 
           bip2021_pgs, 
           chmaltreat_pgs, 
           #cross_dis_pgs,
           ea3,
           insomnia_pgs,
           intell_pgs,
           mdd_pgc2024_pgs,
           ptsd_pgs,
           scz2022_pgs
           )
  
  pgs_df_procd_corr <- pgs_df_procd_corr[sapply(pgs_df_procd_corr, is.numeric)]
  
  # Step 2: Create the correlation matrix, handling NAs with use = "pairwise.complete.obs"
  correlation_matrix <- cor(pgs_df_procd_corr, use = "pairwise.complete.obs")
  
  # Step 3: Plot the correlation matrix with a color gradient
  ggcorrplot(correlation_matrix, 
             method = "square",        # Square-shaped cells
             hc.order = TRUE,
             type = "lower",           # Show only lower triangle
             insig = "blank",
             lab = TRUE,               # Add correlation coefficients
             lab_size = 3,             # Size of the labels
             colors = c("darkred", "white", "darkblue"), # Color gradient (blue to red)
             title = "Correlation Matrix", 
             tl.cex = 10)   
  
                                            
  pgs_df_procd <- pgs_df_procd %>% 
    mutate(preg_id = as.numeric(preg_id),
           ind_id = paste0(str_trim(preg_id), "_", str_trim(BARN_NR)))
  
  
  save(pgs_df_procd, file="data/pgs_df_procd.RData")
} else {
  load(file="./data/pgs_df_procd.RData")
}
