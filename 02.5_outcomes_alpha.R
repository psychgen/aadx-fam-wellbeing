library(psych)
library(tidyverse)
library(haven)

ordinal_alpha <- function(x){
  psych::alpha(psych::polychoric(x)$rho)
}

load("./data/outcome_df.RData")


outcomes <- list(child_outcomes_8yr = c("smfq_dep_c_8yr",
                                        "scared_anx_c_8yr",
                                        "NN99"),
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

child_outcomes_14yr <- list(swls_full_c_14c = c("UB36_raw", # individual swls items
                                                "UB37_raw", # individual swls items
                                                "UB38_raw", # individual swls items
                                                "UB39_raw", # individual swls items
                                                "UB40_raw" # individual swls items
                                                ),
                            smfq_dep_m_14m = c("UM178_raw", # individual sMFQ items
                                               "UM179_raw", # individual sMFQ items
                                               "UM180_raw", # individual sMFQ items
                                               "UM181_raw", # individual sMFQ items
                                               "UM182_raw", # individual sMFQ items
                                               "UM183_raw" # individual sMFQ items
                            ),
                            scared_anx_m_14m = c("UM184_raw", # individual SCARED items
                                                 "UM185_raw", # individual SCARED items
                                                 "UM186_raw", # individual SCARED items
                                                 "UM187_raw", # individual SCARED items
                                                 "UM188_raw" # individual SCARED items
                            )
)



#############################
# Create sibling outcome df #
#############################

outcome_df_asd <- outcome_df %>%
  filter(received_dx_2x_asd_npr == "yes")


# Creating a sibling dataset

# Extracting all unique mother IDs in our asd df
m_ids_asd <- outcome_df_asd %>% 
  dplyr::select(m_id) %>% 
  distinct()


# Selecting an index child for each m_id by selecting all children with a
# diagnosis. If they are the only child with a diagnosis for their m_id, they 
# automatically become the index child. If there are multiple children with a 
# diagnosis, one child randomly gets assigned as the index child.
set.seed(123)

index_children_outcome_df <- outcome_df_asd %>% 
  
  # Selecting by m_id to get all autistic children within one family
  group_by(m_id) %>% 
  
  # select one of these children to be the index child
  slice_sample(n=1) %>% 
  ungroup() %>% 
  
  # Rename ind_id to index_id
  mutate(index_id = ind_id)


# Creating a df with all children who are part of a family with an autistic
# child (autistic children and their siblings).
siblings_outcome_df <- outcome_df %>% 
  inner_join(m_ids_asd, by = "m_id") %>% 
  
  # Adding the index_ids of all index children to the siblings_df, then 
  # removing the actual index children to keep only the siblings
  
  inner_join(index_children_outcome_df %>% dplyr::select(index_id, m_id), by = "m_id") %>% 
  group_by(m_id) %>% 
  filter(ind_id != index_id) %>% 
  ungroup()







# Calculate Ordinal Cronbach's alpha for all items


# Initialize a list to store results
alpha_results <- list()


# Loop through each scale
for (scale in unname(unlist(outcomes))) {
  print(scale)
  # Create a pattern to match item names
  item_pattern <- paste0("^", scale, "_.*_coded$")
  
  # Extract the items that match the pattern
  scale_items <- outcome_df[, grep(item_pattern, names(outcome_df), value = TRUE)]
  
  print(scale_items)
  
  # Check if we have any items for this scale
  if (ncol(scale_items) > 0) {
    # Calculate Cronbach's alpha
    alpha_result <- ordinal_alpha(scale_items)
    print(alpha_result)
    # Store the result along with the scale name
    alpha_results[[scale]] <- list(cronbach_alpha = alpha_result$total$std.alpha,
                                   n_items = length(names(scale_items)),
                                   items = paste(names(scale_items), collapse = ", "))
  } else {
    alpha_results[[scale]] <- list(cronbach_alpha = NA,
                                   n_items = NA,
                                   items = NA)  # No items found
  }
}

# Manually append the results list with scales not available in phenotools



for (scale in names(child_outcomes_14yr)) {
  
  items <- child_outcomes_14yr[[scale]]
  
  if (length(items) > 0) {
    # Extract the relevant columns from the data frame
    scale_items <- outcome_df[, items]
    
    # Calculate Cronbach's alpha for the extracted items
    alpha_result <- ordinal_alpha(scale_items)
    print(alpha_result)
    
    # Append the alpha value to the corresponding scale in outcomes
    alpha_results[[scale]] <- list(cronbach_alpha = alpha_result$total$std.alpha,
                                   n_items = length(items),
                                   items = paste(items, collapse = ", "))
    #list(ca = alpha_result$total$raw_alpha,
                             
  } else {
    alpha_results[[scale]] <- list(cronbach_alpha = NA,
                                   n_items = NA,
                                   items = NA)  # No items found
  }
}





#######
# Siblings


# Loop through each scale
for (scale in unname(outcomes$child_outcomes_8yr)) {
  print(scale)
  # Create a pattern to match item names
  item_pattern <- paste0("^", scale, "_.*_coded$")
  
  # Extract the items that match the pattern
  scale_items <- siblings_outcome_df[, grep(item_pattern, names(siblings_outcome_df), value = TRUE)]
  
  print(scale_items)
  
  # Check if we have any items for this scale
  if (ncol(scale_items) > 0) {
    # Calculate Cronbach's alpha
    alpha_result <- ordinal_alpha(scale_items)
    print(alpha_result)
    # Store the result along with the scale name
    alpha_results[[paste0("sibling_", scale)]] <- list(cronbach_alpha = alpha_result$total$std.alpha,
                                                       n_items = length(names(scale_items)),
                                                       items = paste(names(scale_items), collapse = ", "))
  } else {
    alpha_results[[paste0("sibling_", scale)]] <- list(cronbach_alpha = NA,
                                                       n_items = NA,
                                                       items = NA)  # No items found
  }
}

# Manually append the results list with scales not available in phenotools



for (scale in names(child_outcomes_14yr)) {
  
  items <- child_outcomes_14yr[[scale]]
  
  if (length(items) > 0) {
    # Extract the relevant columns from the data frame
    scale_items <- siblings_outcome_df[, items]
    
    # Calculate Cronbach's alpha for the extracted items
    alpha_result <- ordinal_alpha(scale_items)
    print(alpha_result)
    
    # Append the alpha value to the corresponding scale in outcomes
    alpha_results[[paste0("sibling_", scale)]] <- list(cronbach_alpha = alpha_result$total$std.alpha,
                                                       n_items = length(items),
                                                       items = paste(items, collapse = ", "))
    #list(ca = alpha_result$total$raw_alpha,
    
  } else {
    alpha_results[[paste0("sibling_", scale)]] <- list(cronbach_alpha = NA,
                                                       n_items = NA,
                                                       items = NA)  # No items found
  }
}

# Convert results to a data frame for easier viewing
alpha_results_df <- do.call(rbind, lapply(alpha_results, as.data.frame))
print(alpha_results_df)

save(alpha_results, file = "./output/summary/apha_results.RData")
