library(tidyverse)

# Load and process the coefficients data
coefs <- read_csv("analysis/nfaan05_poisson regression of familial aggregation.csv") %>% 
  dplyr::filter(str_detect(term, "I\\(o_")) %>%
  dplyr::filter(model %in% c("M1", "N1", "P1", "Q1", "S1", "T1", "U1")) %>%
  mutate(
    type = "Coefficient",
    level = "Base",
    contrast = "Base",
    modifier = "None",
    exposure = case_when(
      str_detect(term, "I\\(o_htn >= 1\\)") ~ "I(o_htn >= 1)",
      str_detect(term, "I\\(o_htn_blood_related >= 1\\)") ~ "I(o_htn_blood_related >= 1)",
      str_detect(term, "I\\(o_htn_not_blood_related >= 1\\)") ~ "I(o_htn_not_blood_related >= 1)",
      str_detect(term, "I\\(o_diagnosedhtn >= 1\\)") ~ "I(o_diagnosedhtn >= 1)",
      str_detect(term, "I\\(o_undiagnosedhtn >= 1\\)") ~ "I(o_undiagnosedhtn >= 1)",
      TRUE ~ NA_character_
    ),
    group = case_when(
      model == "M1" ~ "Disease Predicting Disease",
      model == "S1" ~ "Disease Predicted by Disease in Blood Relation",
      model == "T1" ~ "Disease Predicted by Disease in Non-Blood Relation",
      model == "U1" ~ "Disease Predicted by Disease in Blood Relation and Non-Blood Relation",
      model == "P1" ~ "Clustering of Diagnosed Hypertension",
      model == "Q1" ~ "Clustering of Undiagnosed Hypertension",
      model == "N1" ~ "Diagnosed Hypertension Predicting Undiagnosed Hypertension",
      TRUE ~ "Other"
    )
  )

# Ensure Estimate, LCI, and UCI columns exist and are correctly named in coefs
coefs <- coefs %>%
  rename(
    Estimate = PR,
    LCI = lci,
    UCI = uci
  ) %>%
  select(level, term, Estimate, LCI, UCI, modifier, contrast, model, exposure, group, type)

# Load and process the contrasts data
contrasts <- read_csv("analysis/nfaan05_contrasts of poisson regression of familial aggregation.csv") %>%
  rename(modifier = contrast) %>%
  mutate(
    contrast = rep(c("Contrast 1", "Contrast 2", "Contrast 3"), length.out = n()),
    model_type = substr(modifier, 1, 1),  # Extract first character for model type letter
    model = substr(modifier, 1, 2),  # Extract first two characters for term
    modifier = substr(modifier, 4, nchar(modifier)),  # Remove first three characters from modifier
    exposure = case_when(
      str_detect(term, "I\\(o_htn >= 1\\)") ~ "I(o_htn >= 1)",
      str_detect(term, "I\\(o_htn_blood_related >= 1\\)") ~ "I(o_htn_blood_related >= 1)",
      str_detect(term, "I\\(o_htn_not_blood_related >= 1\\)") ~ "I(o_htn_not_blood_related >= 1)",
      str_detect(term, "I\\(o_diagnosedhtn >= 1\\)") ~ "I(o_diagnosedhtn >= 1)",
      str_detect(term, "I\\(o_undiagnosedhtn >= 1\\)") ~ "I(o_undiagnosedhtn >= 1)",
      TRUE ~ NA_character_
    ),
    group = case_when(
      model_type == "M" ~ "Disease Predicting Disease",
      model_type == "S" ~ "Disease Predicted by Disease in Blood Relation",
      model_type == "T" ~ "Disease Predicted by Disease in Non-Blood Relation",
      model_type == "U" ~ "Disease Predicted by Disease in Blood Relation and Non-Blood Relation",
      model_type == "P" ~ "Clustering of Diagnosed Hypertension",
      model_type == "Q" ~ "Clustering of Undiagnosed Hypertension",
      model_type == "N" ~ "Diagnosed Hypertension Predicting Undiagnosed Hypertension",
      TRUE ~ "Other"
    ),
    type = "Effect Modifier"
  )

contrasts <- contrasts %>%
  mutate(level = case_when(
    contrast == "Contrast 1" & str_detect(modifier, "sex") ~ "Female",
    contrast == "Contrast 2" & str_detect(modifier, "sex") ~ "Male",
    contrast == "Contrast 1" & str_detect(modifier, "age_category") ~ "18-39",
    contrast == "Contrast 2" & str_detect(modifier, "age_category") ~ str_replace(modifier, "age_category", ""),
    contrast == "Contrast 1" & str_detect(modifier, "residence") ~ "Rural",
    contrast == "Contrast 2" & str_detect(modifier, "residence") ~ "Urban",
    TRUE ~ NA_character_
  )) %>%
  distinct(contrast, model, level, .keep_all = TRUE) %>%
  select(level, term, Estimate, LCI, UCI, modifier, contrast, model, exposure, group, type)

# Exponentiate coefficients and LCI, UCI in contrasts
contrasts <- contrasts %>%
  mutate(
    Estimate = exp(Estimate),
    LCI = exp(LCI),
    UCI = exp(UCI)
  )

# Combine coefficients and contrasts data
combined_data <- bind_rows(coefs, contrasts)
cleaned_data <- combined_data %>%
  dplyr::filter(!is.na(level))

# Plot using the group variable on the y-axis
grouped_plot <- ggplot(cleaned_data, aes(x = Estimate, y = group, color = level, shape = exposure)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2) +
  labs(
    x = "Prevalence Ratio",
    y = "Model Type",
    color = "Effect Modifier",
    shape = "Exposure"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),  # Adjust y-axis text size
    axis.text.x = element_text(size = 8)   # Adjust x-axis text size
  )


# Save the plot
ggsave("paper/combined_models_plot.png", plot = grouped_plot, width = 16, height = 12)
