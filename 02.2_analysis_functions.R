library(tidyverse)
library(MASS)  # For ordinal logistic regression
library(miceadds)
library(mice)
library(broom)
library(mitools)
library(paletteer)
library(car)


####################
# Standardise data #
####################

standardize_data <- function(data) {
  # Identify numeric variables in the dataset
  numeric_vars <- sapply(data, is.numeric)
  
  # Standardize numeric variables (center and scale)
  data[numeric_vars] <- scale(data[numeric_vars])
  
  return(data)
}




is_ordinal <- function(data, outcome_variable) {
  is.factor(data[[outcome_variable]]) && length(levels(data[[outcome_variable]])) > 2
}


####################################
# Assign each covariate to a group #
####################################

assign_cov_group <- function(term, covariates) {
  for (group_name in names(covariates)) {
    
    # Check if the term contains any variable in the current group, regardless of position
    if (any(sapply(covariates[[group_name]], function(var) grepl(var, term)))) {
      return(group_name)
    }
  }
  return(NA)  # Return NA if no match is found
}



##############
# Regression #
##############

run_regression <- function(data, formula, cutoff_age = NULL, sens = NULL) {
  if(!is.null(cutoff_age)) {
    data <- data |> 
      filter(data[["aadx"]] >= cutoff_age)
  }
  
  if(!is.null(sens)) {
    if (sens == "bin") {
      data <- data |> 
        mutate(aadx_bin = as.factor(ifelse(aadx < 9, 0, 1)))
    } else if (sens == "cat") {
      data <- data |> 
        mutate(aadx_cat = as.factor(case_when(aadx < 9 ~ "Early",
                                          aadx >= 9 & aadx < 15 ~ "Mid",
                                          aadx >= 15 ~ "Late",
                                          is.na(aadx) ~ "Early")))
    } else {
      warning("No valid sens input")
    }
    warning("No valid sens input")
  }
  
  # Standardise data
  std_data <- standardize_data(data)
  
  # Run the regression
  model <- lm(formula, std_data)
  return(model)
}


########################
# Regression clustered #
########################

run_regression_cluster <- function(data, formula, cutoff_age = NULL, cluster_var) {
  
  if(!missing(cutoff_age)) {
    data <- data |> 
      filter(aadx >= cutoff_age)
  }
  
  # Standardise data
  std_data <- standardize_data(data)
  
  # Run the regression
  model <- lm.cluster(data = std_data, formula = formula, cluster = cluster_var)
  return(model)
}

###################
# Pooling results #
###################

# Check if the model has interaction terms
has_interactions <- function(model) {
  tl <- attr(terms(model), "term.labels")
  any(grepl(":", tl, fixed = TRUE))
}

get_vif_vector <- function(model) {
  # If the model has interaction terms, use type="predictor" 
  type <- if (has_interactions(model)) "predictor" else "terms"
  v <- vif(model, type = type)
  
  # If the output is a vector of named numbers, return just that
  if (is.numeric(v) && is.null(dim(v))) {
    # simple case: numeric vector
    return(v)
  }
  
  # Otherwise, convert to dataframe
  v <- as.data.frame(v)
  
  # If Df is 1, return GVIF, otherwise return GVIF^1/(2*Df)
  vif_num <- ifelse(
    v[, 2] == 1,
    v[, 1],
    v[, 3]
  )
  
  # Add name to value
  names(vif_num) <- rownames(v)
  vif_num
}


# Function for matching vif terms to terms in lm model output.
get_term_vif <- function(coef_name, vif_terms, vif_vals) {
  if (grepl(":", coef_name)) return(NA_real_)  # no VIF for interactions
  
  idx <- match(coef_name, vif_terms)
  if (!is.na(idx)) return(vif_vals[idx])
  
  matches <- sapply(vif_terms, function(p) grepl(p, coef_name))
  if (!any(matches)) return(NA_real_)
  if (sum(matches) == 1L) return(vif_vals[matches])
  
  cand_terms <- vif_terms[matches]
  j <- which.max(nchar(cand_terms))
  vif_vals[which(matches)[j]]
}

pool_results <- function(results_list, hypothesis, covariates, outcome, return = "PR") { 
  
  # Calculate and pool coefficients and variances
  betas <- lapply(results_list, function(model) coef(model))
  variances <- lapply(results_list, function(model) vcov(model))
  pooled_result <- summary(pool_mi(qhat = betas, u = variances))
  
  # Calculate VIF and extract correct VIF value depending on model/predictor
  vif_list <- lapply(results_list, get_vif_vector)
  terms <- names(vif_list[[1]])
  vif_df <- sapply(vif_list, function(v) v[terms])
  mean_vif <- rowMeans(vif_df, na.rm = TRUE)
  vif_summary <- data.frame(
    term = terms,
    vif = as.numeric(mean_vif),
    row.names = NULL
  )

  pooled_results <- data.frame()
  
  # Create a pooled results dataframe that is consistent regardless of which method is used.
  
  for (rowname in rownames(pooled_result)) {
    if (any(sapply(c("(Intercept)", covariates), function(cov) grepl(cov, rowname)))) {
      
      row_lm <- pooled_result[rowname, ]
      
      vif_val <- get_term_vif(
        coef_name = rowname,
        vif_terms = vif_summary$term,
        vif_vals = vif_summary$vif
      )

      df_row <- data.frame(
        term = rowname,
        results = row_lm["results"],
        se = row_lm["se"],
        CIL = row_lm["results"] - 1.96 * row_lm["se"],
        CIU = row_lm["results"] + 1.96 * row_lm["se"],
        p = row_lm["p"],
        vif = vif_val,
        outcome = outcome,
        hypothesis = hypothesis
      )
      rownames(df_row) <- NULL
      colnames(df_row) <- c("term", 
                            "results", 
                            "se",
                            "CIL",
                            "CIU",
                            "p", 
                            "vif",
                            "Outcome", 
                            "Hypothesis")
      pooled_results <- rbind(pooled_results, df_row)
    }
  }
  
  if (return == "PR") { #Pooled results
    return(pooled_results)
  } else if (return == "VE") { # Variance explained
    return(variance_explained)
  }
}

######################
# Rounding entire df #
######################

round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
