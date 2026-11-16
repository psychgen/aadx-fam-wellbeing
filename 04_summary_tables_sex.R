library(tidyverse)

load("./data/full_df_asd.RData")
load("./data/full_df_siblings.RData")

source("./scripts/00_replace_terms.R")
source("./scripts/02.1_analysis_covs.R")

date <- format(Sys.Date(), "%Y-%m-%d")

# Function to determine if a variable is binary
is_binary <- function(x) {
  is.factor(x) || is.logical(x) || (is.numeric(x) && length(unique(na.omit(x))) == 2)
}

# Create groups of early, middle, and late diagnosed
full_df_asd <- full_df_asd |> 
  mutate(aadx_cat = as.factor(case_when(aadx < 9 ~ "Early",
                                        aadx >= 9 & aadx < 15 ~ "Mid",
                                        aadx >= 15 ~ "Late",
                                        is.na(aadx) ~ "Early")))

# Create groups of early, middle, and late diagnosed in sibling dia
full_df_siblings <- full_df_siblings |> 
  mutate(aadx_cat = as.factor(case_when(aadx < 9 ~ "Early",
                                        aadx >= 9 & aadx < 15 ~ "Mid",
                                        aadx >= 15 ~ "Late",
                                        is.na(aadx) ~ "Early")))


# Function to create a summary table grouped by aadx category
create_summary_grouped <- function(df, vars) {
  continuous_vars <- vars[!sapply(df[vars], is_binary)]
  binary_vars <- vars[sapply(df[vars], is_binary)]
  
  summary_list <- list()

  df <- df %>%
    mutate(across(all_of(binary_vars), ~ as.numeric(as.character(.))))
  

  
  # Continuous variables summary
  if (length(continuous_vars) > 0) {
    continuous_summary <- df %>%
      pivot_longer(cols = all_of(continuous_vars), names_to = "variable", values_to = "value") %>%
      group_by(variable, aadx_cat) %>%
      summarize(
        mean = mean(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE),
        #n = sum(!is.na(value)),
        #n_total = n(),
        #perc_missing = paste0(round((n_total - n) / n_total * 100, 1), "%"),
        .groups = 'drop'
      ) |> 
      mutate(summary = paste0(round(mean, 2), " (", round(sd, 2), ")")) |> 
      bind_rows(
        df |>
          pivot_longer(cols = all_of(continuous_vars), names_to = "variable", values_to = "value") %>%
          group_by(variable) |> 
          summarize(
            aadx_cat = "All",
            mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            .groups = 'drop',
            summary = paste0(round(mean, 2), " (", round(sd, 2), ")")
          )
      ) |> 
      
      dplyr::select(variable, aadx_cat, summary)
    
    summary_list[[length(summary_list) + 1]] <- continuous_summary
  }
  
  
  # Binary variables summary
  if (length(binary_vars) > 0) {
    binary_summary <- df %>%
      pivot_longer(cols = all_of(binary_vars), names_to = "variable", values_to = "value") %>%
      group_by(variable, aadx_cat) %>%
      summarize(
        proportion = mean(as.numeric(as.character(value)), na.rm = TRUE),
        #n = sum(!is.na(value)),
        #n_total = n(),
        #perc_missing = paste0(round((n_total - n) / n_total * 100, 1), "%"),
        .groups = 'drop'
      ) |> 
      mutate(summary = paste0(round(proportion * 100, 1), "%")) |> 
      bind_rows(
        df |> 
          pivot_longer(cols = all_of(binary_vars), names_to = "variable", values_to = "value") %>%
          group_by(variable) |> 
          summarize(
            aadx_cat = "All",
            proportion = mean(as.numeric(as.character(value)), na.rm = TRUE),
            .groups = 'drop',
            summary = paste0(round(proportion * 100, 1), "%")
          )
      ) |> 
      
      dplyr::select(variable, aadx_cat, summary)
    summary_list[[length(summary_list) + 1]] <- binary_summary
  }
  
  
  print(summary_list)
  if (length(summary_list) > 0) {
    bind_rows(summary_list) %>%
      pivot_wider(names_from = aadx_cat, values_from = summary) |> 
      dplyr::select(variable, Early, Mid, Late, All)
  } else {
    message("No variables to summarize.")
    return(NULL)
  }
}


##############
# Covariates #
##############

full_df_asd <- full_df_asd |> 
  mutate(aadx_cat = as.factor(case_when(aadx < 9 ~ "Early",
                                        aadx >= 9 & aadx < 15 ~ "Mid",
                                        aadx >= 15 ~ "Late",
                                        is.na(aadx) ~ "Early")))

summary_df <- full_df_asd |> 
  dplyr::select(c("ind_id", unname(unlist(covariates)), aadx_cat, aadx)) |> 
  mutate(world_region_bg = as.factor(world_region_bg),
         fylke = as.factor(fylke)) |> 
  mutate(world_region_bg = forcats::fct_na_value_to_level(world_region_bg),
         fylke = forcats::fct_na_value_to_level(fylke)) |> 
  
  pivot_wider(names_from = world_region_bg, 
              values_from = world_region_bg, 
              values_fn = length, 
              values_fill = list(world_region_bg = 0),
              names_prefix = "world_region_bg") |> 
  pivot_wider(names_from = fylke, 
              values_from = fylke, 
              values_fn = length, 
              values_fill = list(fylke = 0),
              names_prefix = "fylke") |> 
  mutate(across(starts_with("world_region_bg"), factor),
         across(starts_with("fylke"), factor))


names(summary_df)
covariates_tables <- covariates

covariates_tables[["Geographical"]] <- c(names(summary_df)[grepl("^fylke_", names(summary_df))],
                                         names(summary_df)[grepl("^world_region_", names(summary_df))])
covariates_tables[["Sociodemographic factors"]] <- setdiff(covariates_tables[["Sociodemographic factors"]], c("world_region_bg", "fylke"))

covariates_tables[["Age at autism diagnosis"]] <- c("aadx")


# Creating covariates summary table for males

summary_df_m <- summary_df |> filter(sex == 0)

summary_tables_grouped_m <- lapply(covariates_tables, function(vars) create_summary_grouped(summary_df_m, vars))

for (i in seq_along(summary_tables_grouped_m)) {
  summary_tables_grouped_m[[i]]$variable <- replace_terms(summary_tables_grouped_m[[i]]$variable, map)
}

n_summary_m <- bind_rows(
  summary_df |> 
    filter(sex == 0) |> 
    group_by(aadx_cat) |> 
    summarize(variable = "n",
              summary = as.character(n()),
              .group = 'drop') |> 
    bind_rows(
      summary_df |> 
        filter(sex == 0) |> 
        summarize(
          aadx_cat = "All",
          variable = "n",
          summary = as.character(n()),
          .group = 'drop')
    ) 
) |> 
  pivot_wider(names_from = aadx_cat, values_from = summary) |> 
  dplyr::select(variable, Early, Mid, Late, All)

summary_tables_grouped_m[["n"]] <- n_summary_m

summary_tables_grouped_m <- bind_rows(summary_tables_grouped_m, .id = "cov_group")

write_csv(summary_tables_grouped_m, file = paste0("./output/summary/csv/", date, "_summary_tables_grouped_m.csv"))


# Creating covariates summary table for females

summary_df_f <- summary_df |> filter(sex == 1)

summary_tables_grouped_f <- lapply(covariates_tables, function(vars) create_summary_grouped(summary_df_f, vars))

for (i in seq_along(summary_tables_grouped_f)) {
  summary_tables_grouped_f[[i]]$variable <- replace_terms(summary_tables_grouped_f[[i]]$variable, map)
}

n_summary_f <- bind_rows(
  summary_df |> 
    filter(sex == 1) |> 
    group_by(aadx_cat) |> 
    summarize(variable = "n",
              summary = as.character(n()),
              .group = 'drop') |> 
    bind_rows(
      summary_df |> 
        filter(sex == 1) |> 
        summarize(
          aadx_cat = "All",
          variable = "n",
          summary = as.character(n()),
          .group = 'drop')
    ) 
) |> 
  pivot_wider(names_from = aadx_cat, values_from = summary) |> 
  dplyr::select(variable, Early, Mid, Late, All)

summary_tables_grouped_f[["n"]] <- n_summary_f

summary_tables_grouped_f <- bind_rows(summary_tables_grouped_f, .id = "cov_group")

write_csv(summary_tables_grouped_f, file = paste0("./output/summary/csv/", date, "_summary_tables_grouped_f.csv"))



############
# Outcomes #
############

outcome_tables_c <- c(outcomes_childhood, 
                      outcomes_adolescence)

outcome_tables_s <- list(sibling_outcomes_8yr = sibling_outcomes_childhood, 
                         sibling_outcomes_14yr = sibling_outcomes_adolescence)

outcomes_summary_df_c <- full_df_asd |> 
  dplyr::select(c("ind_id", "sex", unname(unlist(outcome_tables_c)), aadx_cat))

outcomes_summary_df_s <- full_df_siblings |> 
  dplyr::select(c("ind_id", "sex", unname(unlist(outcome_tables_s)), aadx_cat))


# Outcomes summary tables for males
outcomes_summary_df_c_m <- outcomes_summary_df_c |> filter(sex == 0)

outcomes_summary_df_s_m <- outcomes_summary_df_s |> filter(sex == 0)

outcome_summary_c_grouped_m <- lapply(outcome_tables_c, function(vars) create_summary_grouped(outcomes_summary_df_c_m, vars))

outcome_summary_s_grouped_m <- lapply(outcome_tables_s, function(vars) create_summary_grouped(outcomes_summary_df_s_m, vars))

outcome_summary_grouped_m <- c(outcome_summary_c_grouped_m, outcome_summary_s_grouped_m)

for (i in seq_along(outcome_summary_grouped_m)) {
  outcome_summary_grouped_m[[i]]$variable <- replace_terms(outcome_summary_grouped_m[[i]]$variable, outcome_map_childhood)
}

for (i in seq_along(outcome_summary_grouped_m)) {
  outcome_summary_grouped_m[[i]]$variable <- replace_terms(outcome_summary_grouped_m[[i]]$variable, outcome_map_adolescence)
}

outcome_summary_grouped_m <- bind_rows(outcome_summary_grouped_m, .id = "cov_group")

write_csv(outcome_summary_grouped_m, file = paste0("./output/summary/csv/", date, "_outcome_summary_grouped_m.csv"))


# Outcomes summary tables for females
outcomes_summary_df_c_f <- outcomes_summary_df_c |> filter(sex == 1)

outcomes_summary_df_s_f <- outcomes_summary_df_s |> filter(sex == 1)

outcome_summary_c_grouped_f <- lapply(outcome_tables_c, function(vars) create_summary_grouped(outcomes_summary_df_c_f, vars))

outcome_summary_s_grouped_f <- lapply(outcome_tables_s, function(vars) create_summary_grouped(outcomes_summary_df_s_f, vars))

outcome_summary_grouped_f <- c(outcome_summary_c_grouped_m, outcome_summary_s_grouped_f)

for (i in seq_along(outcome_summary_grouped_f)) {
  outcome_summary_grouped_f[[i]]$variable <- replace_terms(outcome_summary_grouped_f[[i]]$variable, outcome_map_childhood)
}

for (i in seq_along(outcome_summary_grouped_f)) {
  outcome_summary_grouped_f[[i]]$variable <- replace_terms(outcome_summary_grouped_f[[i]]$variable, outcome_map_adolescence)
}

outcome_summary_grouped_f <- bind_rows(outcome_summary_grouped_f, .id = "cov_group")

write_csv(outcome_summary_grouped_f, file = paste0("./output/summary/csv/", date, "_outcome_summary_grouped_f.csv"))

