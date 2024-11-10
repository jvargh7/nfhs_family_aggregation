# Load libraries
library(tidyverse)
library(ggpubr)

# Load the data
source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")

# Load and process the coefficients data
coefs <- read_csv("analysis/nfaan05_poisson regression_of familial aggregation.csv") %>% 
  dplyr::filter(str_detect(term, "I\\(o_")) %>%
  dplyr::filter(model %in% c("P1", "Q1")) %>%
  mutate(
    type = "Coefficient",
    level = "Overall",
    contrast = "Overall",
    modifier = "None",
    model_type = substr(model, 1, 1),
    exposure = case_when(
      str_detect(term, "I\\(o_diagnosedhtn >= 1\\)") & model == "P1" ~ "Aggregation of Diagnosed",
      str_detect(term, "I\\(o_undiagnosedhtn >= 1\\)") & model == "Q1" ~ "Aggregation of Undiagnosed",
      TRUE ~ NA_character_
    ),
    group = case_when(
      model == "P1" ~ "Clustering of Diagnosed Hypertension",
      model == "Q1" ~ "Clustering of Undiagnosed Hypertension",
      TRUE ~ "Other"
    )
  ) %>%
  rename(
    Estimate = PR,
    LCI = lci,
    UCI = uci
  ) %>%
  select(level, term, Estimate, LCI, UCI, modifier, contrast, model, model_type, exposure, group, type)

# Load and process the contrasts data
contrasts <- read_csv("analysis/nfaan05_contrasts of poisson regression of familial aggregation.csv") %>%
  rename(modifier = contrast) %>%
  mutate(
    contrast = rep(c("Contrast 1", "Contrast 2", "Contrast 3"), length.out = n()),
    model_type = substr(modifier, 1, 1),
    model = substr(modifier, 1, 2),
    modifier = substr(modifier, 4, nchar(modifier)),
    exposure = case_when(
      str_detect(term, "I\\(o_diagnosedhtn >= 1\\)") & model_type == "P" ~ "Aggregation of Diagnosed",
      str_detect(term, "I\\(o_undiagnosedhtn >= 1\\)") & model_type == "Q" ~ "Aggregation of Undiagnosed",
      TRUE ~ NA_character_
    ),
    group = case_when(
      model_type == "P" ~ "Clustering of Diagnosed Hypertension",
      model_type == "Q" ~ "Clustering of Undiagnosed Hypertension",
      TRUE ~ "Other"
    ),
    type = "Effect Modifier"
  ) %>%
  mutate(level = case_when(
    contrast == "Contrast 1" & str_detect(modifier, "sex") ~ "Female",
    contrast == "Contrast 2" & str_detect(modifier, "sex") ~ "Male",
    contrast == "Contrast 1" & str_detect(modifier, "age_category") ~ "18-39",
    contrast == "Contrast 2" & str_detect(modifier, "age_category") ~ str_replace(modifier, "age_category", ""),
    contrast == "Contrast 1" & str_detect(modifier, "residence") ~ "Rural",
    contrast == "Contrast 2" & str_detect(modifier, "residence") ~ "Urban",
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(level)) %>%
  dplyr::filter(model_type %in% c("P", "Q")) %>%
  distinct(contrast, model, exposure, level, .keep_all = TRUE) %>%
  select(level, term, Estimate, LCI, UCI, modifier, contrast, model, model_type, exposure, group, type) %>%
  mutate(
    Estimate = exp(Estimate),
    LCI = exp(LCI),
    UCI = exp(UCI)
  )

# Modify exposure labels and combine data
combined_data <- bind_rows(coefs, contrasts) %>% 
  mutate(
    level = factor(level, levels = c("Overall", "Female", "Male", "18-39", "40-64", "65 plus", "Rural", "Urban")),
    exposure = factor(exposure, levels = c(
      "Aggregation of Diagnosed",
      "Aggregation of Undiagnosed"
    ))
  )

combined_data %>% 
  write_csv("paper/table_contrasts and coefficients of poisson regression.csv")

# Define custom colors
custom_colors <- c(
  "Aggregation of Diagnosed" = "#A084CA",
  "Aggregation of Undiagnosed" = "#F8C291"
)

# Create figure B
figB <- combined_data %>%
  dplyr::filter(model_type %in% c("P", "Q")) %>%
  ggplot(aes(x = Estimate, y = level, color = exposure)) +
  geom_point(size = 2, position = position_dodge(width = 0.95)) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2, position = position_dodge(width = 0.95)) +
  labs(x = "Prevalence Ratio (95% CI) of Familial Aggregation", y = "") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12)
  ) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.8, 2.4), breaks = seq(0.8, 2.4, by = 0.2)) +
  geom_vline(xintercept = 1.0, col = "red", linetype = 2) +
  scale_color_manual(values = custom_colors) +
  guides(color = "none")  # Hide the legend but keep colors

# Create survey designs for different family hypertension exposures
diaghtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_diagnosedhtn >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

undiaghtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_undiagnosedhtn >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

# Function to calculate mean and confidence intervals for a variable
calculate_mean_ci_distribution <- function(svy_design, variable, subset_expr = NULL) {
  if (!is.null(subset_expr)) {
    svy_design <- subset(svy_design, eval(subset_expr))
  }
  mean_result <- svymean(as.formula(paste("~", variable)), svy_design)
  ci_result <- confint(mean_result)
  estimate <- coef(mean_result)[1] * 100  # Convert to percentage
  lower_ci <- ci_result[1, 1] * 100  # Convert to percentage
  upper_ci <- ci_result[1, 2] * 100  # Convert to percentage
  
  result <- list(estimate = 100 - estimate, 
                 lower_ci = 100 - lower_ci, 
                 upper_ci = 100 - upper_ci)
  return(result)
}

# Calculate estimates for different family statuses and hypertension types
diaghtn_htn <- calculate_mean_ci_distribution(diaghtn_analytic_svy, "htn_disease == '1'")
diaghtn_diag <- calculate_mean_ci_distribution(diaghtn_analytic_svy, "htn_diagnosed == '1'")
diaghtn_undiag <- calculate_mean_ci_distribution(diaghtn_analytic_svy, "htn_undiagnosed == '1'")

undiaghtn_htn <- calculate_mean_ci_distribution(undiaghtn_analytic_svy, "htn_disease == '1'")
undiaghtn_diag <- calculate_mean_ci_distribution(undiaghtn_analytic_svy, "htn_diagnosed == '1'")
undiaghtn_undiag <- calculate_mean_ci_distribution(undiaghtn_analytic_svy, "htn_undiagnosed == '1'")

# Create a data frame from the calculated estimates
data <- data.frame(
  Family_Status = rep(c("Any member\nhas diagnosed\nhypertension", "Any member\nhas undiagnosed\nhypertension"), each = 3),
  Type = rep(c("Hypertension", "Diagnosed", "Undiagnosed"), times = 2),
  Estimate = c(
               diaghtn_htn$estimate, diaghtn_diag$estimate, diaghtn_undiag$estimate,
               undiaghtn_htn$estimate, undiaghtn_diag$estimate, undiaghtn_undiag$estimate),
  Lower_CI = c(
               diaghtn_htn$lower_ci, diaghtn_diag$lower_ci, diaghtn_undiag$lower_ci,
               undiaghtn_htn$lower_ci, undiaghtn_diag$lower_ci, undiaghtn_undiag$lower_ci),
  Upper_CI = c(
               diaghtn_htn$upper_ci, diaghtn_diag$upper_ci, diaghtn_undiag$upper_ci,
               undiaghtn_htn$upper_ci, undiaghtn_diag$upper_ci, undiaghtn_undiag$upper_ci)
)

# Convert Family_Status and Type to a factor with the desired order
data$Family_Status <- factor(data$Family_Status, levels = c("Any member\nhas diagnosed\nhypertension", "Any member\nhas undiagnosed\nhypertension"))

data$Type <- factor(data$Type, levels = c("Hypertension", "Diagnosed", "Undiagnosed"))

# Define colors for the Family_Status
colors <- c("Any member\nhas diagnosed\nhypertension" = "#A084CA",  # Soft Purple
            "Any member\nhas undiagnosed\nhypertension" = "#F8C291") 

# Create the faceted plot with labels positioned higher
q <- ggplot(data, aes(x = Family_Status, y = Estimate, fill = Family_Status)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                position = position_dodge(width = 0.8), width = 0.2) +
  geom_text(aes(label = round(Estimate, 1)), 
            position = position_dodge(width = 0.8), 
            vjust = -1.2, size = 3) +  # Adjust the vjust to move labels higher
  scale_fill_manual(values = colors) +
  labs(x = "Family Status", y = "Prevalence (%)", fill = "Family Status") +
  facet_wrap(~Type, scales = "free_x") +  # Create panels for each type of hypertension
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove the x-axis text
        axis.ticks.x = element_blank(),  # Optionally remove x-axis ticks
        legend.position = "bottom")  # Move the legend to the bottom

# Combine the two figures using ggpubr::ggarrange with a single common legend from Panel B
grouped_plot <- ggarrange(q, figB, nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom", labels = c("A", "B"))

# Save the plot
ggsave(filename = paste0(path_family_aggregation_folder, "/figures/familial aggregation of hypertension by diagnosis status.png"), 
       plot = grouped_plot, width = 15, height = 6)
