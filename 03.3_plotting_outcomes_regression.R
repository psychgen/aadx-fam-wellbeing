library(tidyverse)

date <- format(Sys.Date(), "%Y-%m-%d")

load("./output/results/results_h2.RData")
load("./output/results/results_h3.RData")

source("./scripts/03.1_plotting_functions.R")

######
# H2 #
######

plot_df_h2_main <- clean_plotting_df(results_h2, included_vars)
plot_df_h2_sup <- clean_plotting_df(results_h2, included_vars, plot = "sup")

for (fam_member in unique(plot_df_h2_main$person)) {
  file_name <- paste0("./output/Figures/", date, "_H2_regression_main_", fam_member, ".tiff")
  tiff(file_name, width = 190, height = 270, units = "mm", res = 300)
  plot_regression_outcomes(plot_df_h2_main, fam_member = fam_member,
                           palette = "bamako",
                           begin = 0,
                           end = 0.9) |> print()
  dev.off()
}


for (fam_member in unique(plot_df_h2_sup$person)) {
  file_name <- paste0("./output/Figures/", date, "_H2_regression_extra_", fam_member, ".tiff")
  tiff(file_name, width = 190, height = 270, units = "mm", res = 300)
  plot_regression_outcomes(plot_df_h2_sup, fam_member = fam_member,
                           palette = "bamako",
                           begin = 0,
                           end = 0.9) |> print()
  dev.off()
}


######
# H3 #
######

plot_df_h3_main <- clean_plotting_df(results_h3, included_vars)
plot_df_h3_sup <- clean_plotting_df(results_h3, included_vars, plot = "sup")

for (fam_member in unique(plot_df_h3_main$person)) {
  file_name <- paste0("./output/Figures/", date, "_H3_regression_main_", fam_member, ".tiff")
  tiff(file_name, width = 190, height = 250, units = "mm", res = 300)
  plot_regression_outcomes(plot_df_h3_main, fam_member = fam_member,
                           palette = "bamako",
                           begin = 0,
                           end = 0.9) |> print()
  dev.off()
}


for (fam_member in unique(plot_df_h3_main$person)) {
  file_name <- paste0("./output/Figures/", date, "_H3_regression_extra_", fam_member, ".tiff")
  tiff(file_name, width = 190, height = 250, units = "mm", res = 300)
  plot_regression_outcomes(plot_df_h3_sup, fam_member = fam_member,
                           palette = "bamako",
                           begin = 0,
                           end = 0.9) |> print()
  dev.off()
}

