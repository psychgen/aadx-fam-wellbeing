library(tidyverse)

date <- format(Sys.Date(), "%Y-%m-%d")

load("./output/results/results_h1.RData")

source("./scripts/03.1_plotting_functions.R")

# Create new plotting df
plot_df_h1_main <- clean_plotting_df(results_h1, included_vars)
plot_df_h1_sup <- clean_plotting_df(results_h1, included_vars, plot = "sup")

tiff(paste0("./output/Figures/", date, "_H1_regression_main_aadx.tiff"), width = 190, height = 270, units = "mm", res = 300)
plot_regression_aadx(plot_df_h1_main, 
                     palette = "oslo",
                     begin = 0.1,
                     end = 0.6) |> 
  print()
dev.off()

tiff(paste0("./output/Figures/", date, "_H1_regression_extra_aadx.tiff"), width = 190, height = 270, units = "mm", res = 300)
plot_regression_aadx(plot_df_h1_sup,
                     palette = "oslo",
                     begin = 0.1,
                     end = 0.6) |> 
  print()
dev.off()
