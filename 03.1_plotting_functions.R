library(tidyverse)
library(scico)

#######################
# Regression overview #
#######################

# Included vars

included_vars <- c("Age at onset walking",
                   "Language skills age 3",
                   "SCQ Social communication age 3",
                   "SCQ Restricted repetitive behaviour age 3",
                   "CBCL Externalising behaviour age 3",
                   "Motor development problems age 5",
                   "Language development problems age 5",
                   "CCC2 Coherence age 5",
                   "CBCL Externalising behaviour age 5",
                   "Intellectual disability diagnosis",
                   "Epilepsy diagnosis",
                   "Cerebral palsy diagnosis",
                   "Sex",
                   "Maternal age at birth",
                   "Paternal age at birth",
                   "Parity",
                   "Maternal education",
                   "Paternal education",
                   "Household income",
                   "Parental cohabitation",
                   "Financial problems",
                   "Region of origin: Americas",
                   "Region of origin: Asia",
                   "Region of origin: Europe (outside Norway)",
                   "Region of origin: Africa",
                   "Maternal concerns age 3",
                   "Maternal concerns age 5",
                   "Social support age 3",
                   "Social support frequency age 3",
                   "Social support age 8",
                   "Social support frequency age 8",
                   "Social support age 14",
                   "Social support frequency age 14",
                   "Child ADHD PGS",
                   "Child Anorexia Nervosa PGS",
                   "Child anxiety PGS",
                   "Child autism PGS",
                   "Child bipolar disorder PGS",
                   "Child childhood maltreatment PGS",
                   "Child educational attainment PGS",
                   "Child insomnia PGS",
                   "Child intelligence PGS",
                   "Child depression PGS",
                   "Child PTSD PGS",
                   "Child schizophrenia PGS")


# Cleaning df for plotting

clean_plotting_df <- function(df, included_vars, plot = "main") {

  df <- df |> 
    # Delete all variables with .Q or .C suffix
    filter(!grepl('\\.Q$', term),
           !grepl('\\.C$', term),
           term != "(Intercept)") |> 
    
    # Remove 1 and .L suffixes
    mutate(term = str_remove(term, "\\.L$")) |> 
    mutate(term = str_remove(term, "1$"))
  
  if ("adjusted_p" %in% colnames(df)) {
    df <- df |>
      group_by(term, person) |> 
      mutate(
        new_term = case_when(
          any(adjusted_p < 0.05) ~ paste0(term, "*"),
          adjusted_p >= 0.05 ~ term,
          TRUE ~ term  # To handle cases, if adjusted_p is NA
        )
      ) |> 
      ungroup()
  } else {
    df <- df |>
      group_by(term) |> 
      mutate(
        new_term = ifelse(any(Model == "Multiple regression" & p < 0.05),
                          paste(term, "*", sep = ""),
                          term),
        .groups = 'drop') |>
      ungroup()
  }


  if (plot == "main") {

    df <- df |>
      filter(term %in% included_vars) |>
      mutate(cov_group = case_when(cov_group == "Development and behaviour" ~ "Dev. and beh.",
                                   TRUE ~ cov_group)) |>
      mutate(new_term = case_when(cov_group == "Dev. and beh." ~ factor(new_term, levels = rev(c(unique(new_term[grepl("Age at onset walking",new_term)]),
                                                                                                 unique(new_term[grepl("Language skills age 3",new_term)]),
                                                                                                 unique(new_term[grepl("SCQ Social communication age 3",new_term)]),
                                                                                                 unique(new_term[grepl("SCQ Restricted repetitive behaviour age 3",new_term)]),
                                                                                                 unique(new_term[grepl("CBCL Externalising behaviour age 3",new_term)]),
                                                                                                 unique(new_term[grepl("CBCL Externalising behaviour age 5",new_term)]),
                                                                                                 unique(new_term[grepl("CCC2 Coherence age 5",new_term)]),
                                                                                                 unique(new_term[grepl("Motor development problems age 5",new_term)]),
                                                                                                 unique(new_term[grepl("Language development problems age 5",new_term)]),
                                                                                                 unique(new_term[grepl("Maternal concerns age 3",new_term)]),
                                                                                                 unique(new_term[grepl("Maternal concerns age 5",new_term)])))),
                                  cov_group == "Sociodemographic factors" ~ factor(new_term, levels = rev(c(unique(new_term[grepl("Sex",new_term)]),
                                                                                                            unique(new_term[grepl("Maternal age at birth",new_term)]),
                                                                                                            unique(new_term[grepl("Paternal age at birth",new_term)]),
                                                                                                            unique(new_term[grepl("Maternal education",new_term)]),
                                                                                                            unique(new_term[grepl("Paternal education",new_term)]),
                                                                                                            unique(new_term[grepl("Parental cohabitation",new_term)]),
                                                                                                            unique(new_term[grepl("Household income",new_term)]),
                                                                                                            unique(new_term[grepl("Financial problems",new_term)]),
                                                                                                            unique(new_term[grepl("Region of origin\\: Americas",new_term)]),
                                                                                                            unique(new_term[grepl("Region of origin\\: Asia",new_term)]),
                                                                                                            unique(new_term[grepl("Region of origin\\: Europe \\(outside Norway\\)",new_term)]),
                                                                                                            unique(new_term[grepl("Region of origin\\: Africa",new_term)]),
                                                                                                            unique(new_term[grepl("Parity",new_term)])))),
                                  cov_group == "Social support" ~ factor(new_term, levels = rev(c(unique(new_term[grepl("Social support age 3",new_term)]),
                                                                                                  unique(new_term[grepl("Social support frequency age 3",new_term)]),
                                                                                                  unique(new_term[grepl("Social support age 8",new_term)]),
                                                                                                  unique(new_term[grepl("Social support frequency age 8",new_term)]),
                                                                                                  unique(new_term[grepl("Social support age 14",new_term)]),
                                                                                                  unique(new_term[grepl("Social support frequency age 14",new_term)])))),
                                  TRUE ~ factor(new_term) # Default case
      ),
      cov_group = factor(cov_group, levels = c("Dev. and beh.",
                                               "NPR",
                                               "Child PGS",
                                               "Sociodemographic factors",
                                               "Social support")
                         )
      )

  } else {
    df <- df |>
      filter(!(term %in% included_vars)) |>
      mutate(cov_group = case_when(cov_group == "Birth year" ~ "B.",
                                   TRUE ~ cov_group))
  }

  df <- df[!is.na(df$new_term),]

  return(df)

}



# Plotting aadx regressions

plot_regression_aadx <- function(df, palette, begin = 0,end = 1) {
  plot <- df |> 
    ggplot(aes(x = new_term, 
               y = results, 
               #alpha = alpha_value, 
               colour = Model, 
               shape = Model)) +
    geom_point(position = position_dodge(width = 1), size = 2) +
    geom_linerange(aes(ymin = results - 1.96*se, ymax = results + 1.96*se), position = position_dodge(width = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed")+
    #scale_alpha_identity()+
    theme_bw(base_size = 12)+
    theme(legend.position="bottom")+
    #axis.text.y = element_text(size = 12))+
    labs(title = NULL,
         x = NULL,
         y = "Standardised effects on age at first autism diagnosis")+
    facet_grid(cov_group ~ ., scales = "free", space = "free_y")+
    scale_colour_scico_d(palette = palette, begin = begin, end = end)+
    scale_fill_scico_d(palette = palette, begin = begin, end = end)+
    #facet_wrap(~cov_group, scales = "free")+
    #scale_color_manual(values = custom_scale_plot1) +
    coord_flip()
  
  return(plot)
}

# Plotting outcomes regressions 

plot_regression_outcomes <- function(df, fam_member, palette, begin = 0, end = 1) {
  plot <- df |> filter(person == fam_member) |> 
    ggplot(aes(x = new_term, 
               y = results, 
               #alpha = alpha_value, 
               colour = Outcome, 
               shape = Outcome)) +
    geom_point(position = position_dodge(width = 1), size = 2) +
    geom_linerange(aes(ymin = results - 1.96*se, ymax = results + 1.96*se), position = position_dodge(width = 1)) +
    geom_hline(yintercept = 0, linetype = "dashed")+
    #scale_alpha_identity()+
    theme_bw(base_size = 12)+
    theme(legend.position="bottom")+
    #axis.text.y = element_text(size = 12))+
    labs(title = NULL,
         x = NULL,
         y = "Standardised effects on age at first autism diagnosis")+
    facet_grid(cov_group ~ ., scales = "free", space = "free_y")+
    #facet_wrap(~cov_group, scales = "free")+
    #scale_color_manual(values = custom_scale_plot1) +
    scale_colour_scico_d(palette = palette, begin = begin, end = end)+
    scale_fill_scico_d(palette = palette, begin = begin, end = end)+
    coord_flip()
  return(plot)
}


###############################
# Regression slope comparison #
###############################

# Creating plotting function
plot_slope_with_ci_facet <- function(regression_summary, 
                                     variable_of_interest, 
                                     x_min, 
                                     x_max, 
                                     x_mean, 
                                     x_sd, 
                                     outcome_period, 
                                     palette,
                                     begin = 0,
                                     end = 1) {
  # Define a range of x values for prediction
  x_values <- c(x_min, x_max)
  
  # Loop over unique combinations of Model and Outcome
  plot_data <- regression_summary %>%
    filter(term %in% c("(Intercept)", variable_of_interest)) %>%
    group_by(Model, Outcome) %>%
    do({
      # Extract parameters
      intercept_row <- .[.$term == "(Intercept)", ]
      intercept <- intercept_row$results
      intercept_se <- intercept_row$se
      
      slope_row <- .[.$term == variable_of_interest, ]
      slope <- slope_row$results
      slope_se <- slope_row$se
      
      
      # Calculate predicted y and CIs
      y_values <- intercept + slope * x_values
      prediction_se <- sqrt(intercept_se^2 + (x_values * slope_se)^2)
      y_ci_lower <- y_values - 1.96 * prediction_se
      y_ci_upper <- y_values + 1.96 * prediction_se
      
      # Return data frame
      data.frame(x = x_values, y = y_values, ci_lower = y_ci_lower, ci_upper = y_ci_upper)
    })
  
  # Create the plot
  ggplot(plot_data, aes(x = (x_mean + x * x_sd), y = y, colour = Model)) +
    geom_hline(yintercept =  0, color = "gray", linetype = "dashed", linewidth = 0.5)+
    geom_line(linewidth = 1) +                        # Regression line
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Model), alpha = 0.2, colour = NA) + # Confidence intervals
    facet_grid(. ~ Outcome) +                 # Create facets for each Outcome and Model type
    labs(x = "Age at first autism diagnosis", y = "Predicted standardised value") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")+
    scale_colour_scico_d(palette = palette, begin = begin, end = end)+
    scale_fill_scico_d(palette = palette, begin = begin, end = end)+
    #ggtitle(paste0("Regression slopes for age at autism diagnosis on ", outcome_period, " wellbeing")) +
    ylim(-1, 1)
}


###############
# Interaction #
###############

plot_slope_with_ci_facet_int <- function(regression_summary, 
                                         variable_of_interest, 
                                         x_min, 
                                         x_max, 
                                         x_mean, 
                                         x_sd, 
                                         outcome_period, 
                                         model = "Adjusted",
                                         palette,
                                         begin = 0,
                                         end = 1) {
  # Define a range of x values for prediction
  x_values <- c(x_min, x_max)
  
  # Loop over unique combinations of Model and Outcome
  plot_data <- regression_summary %>%
    filter(term == "(Intercept)" | grepl(variable_of_interest, term),
           Model == model) %>%
    
    group_by(Outcome) %>%
    do({
      # Extract parameters
      intercept_row <- .[.$term == "(Intercept)", ]
      intercept <- intercept_row$results
      intercept_se <- intercept_row$se
      
      slope_row <- .[.$term == variable_of_interest, ]
      slope <- slope_row$results
      slope_se <- slope_row$se
      
      interaction_effect_row <- .[.$term == paste0("Sex1:", variable_of_interest), ]
      interaction_effect <- interaction_effect_row$results
      interaction_effect_se <- interaction_effect_row$se
      
      boys_y_values <- intercept + slope * x_values
      girls_y_values <- intercept + (slope + interaction_effect) * x_values
      
      # Prediction SE for confidence intervals
      boys_prediction_se <- sqrt(intercept_se^2 + (x_values * slope_se)^2)
      girls_prediction_se <- sqrt(intercept_se^2 + (x_values * slope_se)^2 + (x_values * interaction_effect_se)^2)
      
      # Confidence intervals
      boys_ci_lower <- boys_y_values - 1.96 * boys_prediction_se
      boys_ci_upper <- boys_y_values + 1.96 * boys_prediction_se
      
      girls_ci_lower <- girls_y_values - 1.96 * girls_prediction_se
      girls_ci_upper <- girls_y_values + 1.96 * girls_prediction_se
      
      data.frame(
        x = rep(x_values, 2),
        y = c(boys_y_values, girls_y_values),
        ci_lower = c(boys_ci_lower, girls_ci_lower),
        ci_upper = c(boys_ci_upper, girls_ci_upper),
        sex = rep(c("Boys", "Girls"), each = length(x_values))
        #Outcome = interaction_effect_row$Outcome[1]  # Adds outcome information for faceting
      )
    })
  plot_data <- plot_data |> 
    mutate(sex = factor(sex, levels = c("Girls", "Boys")))
  # print(plot_data)
  # Create the plot
  ggplot(plot_data, aes(x = (x_mean + x * x_sd), y = y, colour = sex)) +
    geom_hline(yintercept =  0, color = "gray", linetype = "dashed", linewidth = 0.5)+
    geom_line(linewidth = 1) +                        # Regression line
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = sex), alpha = 0.2, colour = NA) + # Confidence intervals
    facet_grid(. ~ Outcome) +                 # Create facets for each Outcome and Model type
    labs(x = "Age at first autism diagnosis", y = "Predicted standardised value") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")+
    scale_colour_scico_d(palette = palette, begin = begin, end = end)+
    scale_fill_scico_d(palette = palette, begin = begin, end = end)+
    #ggtitle(paste0("Regression slopes for age at autism diagnosis on ", outcome_period, " wellbeing")) +
    #scale_color_manual(values = c("Boys" = "#619CFF", "Girls" = "#F8766D")) +
    #scale_fill_manual(values = c("Boys" = "#619CFF", "Girls" = "#F8766D"))+
    ylim(-1.5, 1.5)
}
