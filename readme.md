Age at autism diagnosis in the Norwegian Mother, Father and Child Cohort
Study (MoBa): exploring the association with family wellbeing.
================

This is the readme file for asd_aadx_famwb, written in an R Markdown
format used for publishing markdown documents to GitHub.

### Paper working title: Age at autism diagnosis in the Norwegian Mother, Father and Child Cohort Study (MoBa): exploring the association with family wellbeing.

### Lead author(s): Bernt Glaser

### Last author(s): Laurie Hannigan, Alexandra Havdahl

### Corresponding author(s): Bernt Glaser

### Other authors: Meseret Mamo Bazezew, Daniela Bragantini, Laura Hegemann, Stian Valand, Stian Orm, Morgan James Morgan, Somer Bishop

### Target journals: Autism

### Target date for submission to journal & BioRxiv:

# Project description and notes

The scripts are divided into four parts, with the main script of each
part running all sub-scripts.

## 1. Data preparation

The script `01_data_prep.R` uses the `phenotools` package to curate a
dataset, then runs `01.1_pheno_data_proc.R` to process some of the
variables further. It subsequently runs `01.2_abc_data.R`, `01.3_ssb.R`,
and `01.4_pgs.R` to extract data from the ABC study, from SSB, and
polygenic scores, respectively. Finally, it creates a second dataset
which includes siblings and runs two multiple imputation scripts
(`01.5_multiple_imputation.R` and
`01.6_multiple_imputation_siblings.R`).

## 2. Analysis

The `02_analysis.R` script runs the main analyses using
`02.1_analysis_covs.R` and `02.2_analysis_functions.R`, which contain a
list with covariates and the main functions for performing the analyses,
respectively. It then runs performs Wald tests for comparing regression
coefficients (`02.3_wald_tests.R`). It also runs scripts for calculating
descriptive statistics for age at autism diagnosis
(`02.4_descriptive_statistics.R`), Cronbach’s alpha for the outcome
variables (`02.5_outcomes_alpha.R`), and for creating summary tables for
all variables (`02.6_overview_table.R`)

## 3. Plotting

The plotting functions create plots for each of the hypothesis.
`03_plotting.R` first loads the functions from
`03.1_plotting_functions.R`. Then first it plots the regression
coefficients for age at diagnosis (`03.2_plotting_aadx_regression.R`)
and the outcome variables (`03.3_plotting_outcomes_regression.R`).
Finally, it plots the regression slopes for age at diagnosis for
hypotheses 4-7.

## Summary tables

The final set of scripts create summary tables for the outcomes from the
standard regression (`04_summary_tables.R`) and the regression
stratified by sex (`04_summary_tables_sex.R`).
