load("./output/results/results_h4.RData")
load("./output/results/results_h4_sens.RData")
load("./output/results/results_h4_sens_ns.RData")
load("./output/results/results_h5.RData")
load("./output/results/results_h5_sens.RData")
load("./output/results/results_h5_sens_ns.RData")


results_h4_cleaned <- results_h4 |> 
  filter(grepl("Age at autism diagnosis", term)) |> 
  mutate(
    est_ci = sprintf("%.2f [%.2f, %.2f]", results, CIL, CIU),
    p_symbol = case_when(
      adjusted_p < 0.001 ~ "***",
      adjusted_p < 0.01  ~ "**",
      adjusted_p < 0.05  ~ "*",
      adjusted_p < 0.10  ~ "†",
      TRUE               ~ ""
    )) |> 
  mutate(est_ci_sig = str_trim(paste0(est_ci, p_symbol)))

aadx_table_h4 <- results_h4_cleaned |> 
  arrange(Outcome, person, desc(Model)) |> 
  unite(col = "person_model", person, Model, sep = "_") |> 
  dplyr::select(Outcome, person_model, est_ci_sig) |> 
  pivot_wider(
    names_from = person_model,
    values_from = est_ci_sig
  )

results_h5_cleaned <- results_h5 |> 
  filter(grepl("Age at autism diagnosis", term)) |> 
  mutate(
    est_ci = sprintf("%.2f [%.2f, %.2f]", results, CIL, CIU),
    p_symbol = case_when(
      adjusted_p < 0.001 ~ "***",
      adjusted_p < 0.01  ~ "**",
      adjusted_p < 0.05  ~ "*",
      adjusted_p < 0.10  ~ "†",
      TRUE               ~ ""
    )) |> 
  mutate(est_ci_sig = str_trim(paste0(est_ci, p_symbol)))

aadx_table_h5 <- results_h5_cleaned |> 
  arrange(Outcome, person, desc(Model)) |> 
  unite(col = "person_model", person, Model, sep = "_") |> 
  dplyr::select(Outcome, person_model, est_ci_sig) |> 
  pivot_wider(
    names_from = person_model,
    values_from = est_ci_sig
  )


aadx_table <- bind_rows(aadx_table_h4, aadx_table_h5, .id = "timepoint") |> 
  mutate(timepoint = case_when(
    timepoint == 1 ~ "Childhood",
    timepoint == 2 ~ "Adolescence"
  ))

write.csv(aadx_table, file = "./output/tables/aadx_tables.csv", row.names = FALSE)
save(aadx_table, file = "./output/tables/aadx_tables.RData")

# Sensitivity



results_h4_sens_aadx <- results_h4_sens |> 
  filter(grepl("Age at autism diagnosis", term)) |> 
  mutate(
    term_clean = case_when(
      term == "Age at autism diagnosis_catMid"  ~ "Mid vs Early",
      term == "Age at autism diagnosis_catLate" ~ "Late vs Early",
      TRUE ~ term
    ),
    est_ci = sprintf("%.2f (%.2f, %.2f)", results, CIL, CIU),
    p_symbol = case_when(
      adjusted_p < 0.001 ~ "***",
      adjusted_p < 0.01  ~ "**",
      adjusted_p < 0.05  ~ "*",
      adjusted_p < 0.10  ~ "†",
      TRUE               ~ ""
    ),
    est_ci_sig = str_trim(paste0(est_ci, p_symbol))
  ) |>
  dplyr::select(Outcome, term_clean, person, Model, est_ci_sig) |>
  unite("person_model", person, Model, sep = "_") |>
  pivot_wider(
    names_from  = person_model,
    values_from = est_ci_sig
  ) |>
  arrange(Outcome, desc(term_clean))

results_h5_sens_aadx <- results_h5_sens |> 
  filter(grepl("Age at autism diagnosis", term)) |> 
  mutate(
    term_clean = case_when(
      term == "Age at autism diagnosis_catMid"  ~ "Mid vs Early",
      term == "Age at autism diagnosis_catLate" ~ "Late vs Early",
      TRUE ~ term
    ),
    est_ci = sprintf("%.2f (%.2f, %.2f)", results, CIL, CIU),
    p_symbol = case_when(
      adjusted_p < 0.001 ~ "***",
      adjusted_p < 0.01  ~ "**",
      adjusted_p < 0.05  ~ "*",
      adjusted_p < 0.10  ~ "†",
      TRUE               ~ ""
    ),
    est_ci_sig = str_trim(paste0(est_ci, p_symbol))
  ) |>
  dplyr::select(Outcome, term_clean, person, Model, est_ci_sig) |>
  unite("person_model", person, Model, sep = "_") |>
  pivot_wider(
    names_from  = person_model,
    values_from = est_ci_sig
  ) |>
  arrange(Outcome, desc(term_clean))

aadx_table_sens <- bind_rows(results_h4_sens_aadx, results_h5_sens_aadx, .id = "timepoint") |> 
  mutate(timepoint = case_when(
    timepoint == 1 ~ "Childhood",
    timepoint == 2 ~ "Adolescence"
  ))


write.csv(aadx_table_sens, file = "./output/tables/aadx_tables_sens.csv", row.names = FALSE)
save(aadx_table_sens, file = "./output/tables/aadx_tables_sens.RData")


# Sensitivity analysis no siblings
results_h4_cleaned_ns <- results_h4_sens_ns |> 
  filter(grepl("Age at autism diagnosis", term)) |> 
  mutate(
    est_ci = sprintf("%.2f [%.2f, %.2f]", results, CIL, CIU),
    p_symbol = case_when(
      adjusted_p < 0.001 ~ "***",
      adjusted_p < 0.01  ~ "**",
      adjusted_p < 0.05  ~ "*",
      adjusted_p < 0.10  ~ "†",
      TRUE               ~ ""
    )) |> 
  mutate(est_ci_sig = str_trim(paste0(est_ci, p_symbol)))

aadx_table_h4_sens_ns <- results_h4_cleaned_ns |> 
  arrange(Outcome, person, desc(Model)) |> 
  unite(col = "person_model", person, Model, sep = "_") |> 
  dplyr::select(Outcome, person_model, est_ci_sig) |> 
  pivot_wider(
    names_from = person_model,
    values_from = est_ci_sig
  )

results_h5_cleaned_ns <- results_h5_sens_ns |> 
  filter(grepl("Age at autism diagnosis", term)) |> 
  mutate(
    est_ci = sprintf("%.2f [%.2f, %.2f]", results, CIL, CIU),
    p_symbol = case_when(
      adjusted_p < 0.001 ~ "***",
      adjusted_p < 0.01  ~ "**",
      adjusted_p < 0.05  ~ "*",
      adjusted_p < 0.10  ~ "†",
      TRUE               ~ ""
    )) |> 
  mutate(est_ci_sig = str_trim(paste0(est_ci, p_symbol)))

aadx_table_h5_sens_ns <- results_h5_cleaned_ns |> 
  arrange(Outcome, person, desc(Model)) |> 
  unite(col = "person_model", person, Model, sep = "_") |> 
  dplyr::select(Outcome, person_model, est_ci_sig) |> 
  pivot_wider(
    names_from = person_model,
    values_from = est_ci_sig
  )


aadx_table_sens_ns <- bind_rows(aadx_table_h4_sens_ns, aadx_table_h5_sens_ns, .id = "timepoint") |> 
  mutate(timepoint = case_when(
    timepoint == 1 ~ "Childhood",
    timepoint == 2 ~ "Adolescence"
  ))


write.csv(aadx_table_sens_ns, file = "./output/tables/aadx_tables_sens_ns.csv", row.names = FALSE)
save(aadx_table_sens_ns, file = "./output/tables/aadx_tables_sens_ns.RData")
