library(tidyverse)
library(mice)

# Loading results df
load("./data/MI_datlist_new.RData")
load("./data/imputations_new.RData")
load("./output/results/results_h4.RData")
load("./output/results/results_h5.RData")
load("./output/results/results_h6.RData")
load("./output/results/results_h7.RData")

source("./scripts/03.1_plotting_functions.R")

# Plotting aadx regression coefficients

source("./scripts/03.2_plotting_aadx_regression.R")


# Plotting outcomes regression coefficients
source("./scripts/03.3_plotting_outcomes_regression.R")

# Get today's date
date <- format(Sys.Date(), "%Y-%m-%d")

# Find minimum and maximum aadx values

# Function to standardize aadx and find min/max
standardize_and_find_min_max <- function(data) {
  # Standardize aadx
  data$aadx <- scale(data$aadx)
  
  # Find min and max for the standardized aadx
  min_value <- min(data$aadx, na.rm = TRUE)
  max_value <- max(data$aadx, na.rm = TRUE)
  
  return(list(min = min_value, max = max_value))
}

# Apply the function to all datasets
results <- lapply(datlist, standardize_and_find_min_max)

# Print results for each dataset
print(results)

min_values <- sapply(results, function(res) res$min)
max_values <- sapply(results, function(res) res$max)

# Calculate global min and max
aadx_min <- min(min_values, na.rm = TRUE)
aadx_max <- max(max_values, na.rm = TRUE)

# Get pooled mean and sd of aadx
imp.data <- complete(imp, "long")
aadx_mean <- with(imp.data, tapply(aadx, .imp, mean))
aadx_pooled_mean <- mean(aadx_mean)

aadx_sd <- with(imp.data, tapply(aadx, .imp, sd))
aadx_pooled_sd <- mean(aadx_sd)



#############
# Childhood #
#############

for (fam_member in unique(results_h4$person)) {
  file_name <- paste0("./output/Figures/", date, "_H4_regression_slopes_", fam_member, ".tiff")
  tiff(file_name, width = 190, height = 90, units = "mm", res = 300)
  results_h4 |> 
    filter(person == fam_member) |> 
    plot_slope_with_ci_facet(variable_of_interest = "Age at autism diagnosis", 
                             x_min = aadx_min, 
                             x_max = aadx_max,
                             x_mean = aadx_pooled_mean,
                             x_sd = aadx_pooled_sd,
                             outcome_period = "childhood",
                             palette = "oslo",
                             begin = 0.1,
                             end = 0.6
                             ) |> 
    print()
  dev.off()
}

for (fam_member in unique(results_h5$person)) {
  file_name <- paste0("./output/Figures/", date, "_H5_regression_slopes_", fam_member, ".tiff")
  tiff(file_name, width = 190, height = 90, units = "mm", res = 300)
  results_h5 |> 
    filter(person == fam_member) |> 
    plot_slope_with_ci_facet(variable_of_interest = "Age at autism diagnosis", 
                             x_min = aadx_min, 
                             x_max = aadx_max,
                             x_mean = aadx_pooled_mean,
                             x_sd = aadx_pooled_sd,
                             outcome_period = "adolescent",
                             palette = "oslo",
                             begin = 0.1,
                             end = 0.6)|> 
    print()
  dev.off()
}


for (fam_member in unique(results_h6$person)) {
  file_name <- paste0("./output/Figures/", date, "_H6_regression_slopes_", fam_member, ".tiff")
  tiff(file_name, width = 190, height = 90, units = "mm", res = 300)
  results_h6 |> 
    filter(person == fam_member) |> 
    plot_slope_with_ci_facet_int(variable_of_interest = "Age at autism diagnosis", 
                                 x_min = aadx_min, 
                                 x_max = aadx_max,
                                 x_mean = aadx_pooled_mean,
                                 x_sd = aadx_pooled_sd,
                                 outcome_period = "childhood",
                                 palette = "berlin",
                                 model = "Unadjusted",
                                 begin = 0.9,
                                 end = 0.25)|> 
    print()
  dev.off()
}

for (fam_member in unique(results_h7$person)) {
  file_name <- paste0("./output/Figures/", date, "_H7_regression_slopes_", fam_member, ".tiff")
  tiff(file_name, width = 190, height = 90, units = "mm", res = 300)
  results_h7 |> 
    filter(person == fam_member) |> 
    plot_slope_with_ci_facet_int(variable_of_interest = "Age at autism diagnosis", 
                                 x_min = aadx_min, 
                                 x_max = aadx_max,
                                 x_mean = aadx_pooled_mean,
                                 x_sd = aadx_pooled_sd,
                                 outcome_period = "adolescent",
                                 palette = "berlin",
                                 model = "Unadjusted",
                                 begin = 0.9,
                                 end = 0.25)|> 
    print()
  dev.off()
}
