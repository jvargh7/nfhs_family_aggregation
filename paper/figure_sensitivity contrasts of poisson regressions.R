library(tidyverse)
library(ggpubr)

# Load and process the coefficients data
coefs <- read_csv("sensitivity/nfase01_poisson regression of familial aggregation.csv") %>% 
  dplyr::filter(str_detect(term, "I\\(o_")) %>%
  dplyr::filter(model %in% c("R1", "W1")) %>%
  mutate(
    type = "Coefficient",
    level = "Overall",
    contrast = "Overall",
    modifier = "None",
    model_type = substr(model, 1, 1),
    exposure = case_when(
      str_detect(term, "I\\(o_diagnosedhtn >= 1\\)")~ "Diagnosed",
      str_detect(term, "I\\(o_undiagnosedhtn >= 1\\)")~ "Undiagnosed",
      TRUE ~ NA_character_
    ),
    group = case_when(
      model == "R1" ~ "Disease Predicting Disease",
      model == "W1" ~ "Disease Predicted by Disease in Blood Relation",
      TRUE ~ "Other"
    )
  ) %>%
  # Ensure Estimate, LCI, and UCI columns exist and are correctly named in coefs
  rename(
    Estimate = PR,
    LCI = lci,
    UCI = uci
  ) %>%
  select(design, level, term, Estimate, LCI, UCI, modifier, contrast, model, model_type, exposure, group, type)

# Load and process the contrasts data
contrasts <- read_csv("sensitivity/nfase01_contrasts of poisson regression of familial aggregation.csv") %>%
  rename(modifier = contrast) %>%
  mutate(
    contrast = rep(c("Contrast 1", "Contrast 2", "Contrast 3"), length.out = n()),
    model_type = substr(modifier, 1, 1),  # Extract first character for model type letter
    model = substr(modifier, 1, 2),  # Extract first two characters for term
    modifier = substr(modifier, 4, nchar(modifier)),  # Remove first three characters from modifier
    exposure = case_when(
       str_detect(term, "I\\(o_diagnosedhtn >= 1\\)") ~ "At least one Diagnosed",
      str_detect(term, "I\\(o_undiagnosedhtn >= 1\\)")~ "At least one Undiagnosed",
      TRUE ~ NA_character_
    ),
    group = case_when(
      model_type == "R" ~ "Diagnosed Hypertension",
      model_type == "W" ~ "Undiagnosed Hypertension",
      TRUE ~ NA_character_
    ),
    type = "Effect Modifier"
  )  %>%
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
  dplyr::filter(model_type %in% c("R", "W")) %>%
  # Included 'exposure' because we need 2 sex, 3 ages and 2 regions per exposure
  distinct(contrast, model, exposure, level, .keep_all = TRUE) %>%
  select(level, term, Estimate, LCI, UCI, modifier, contrast, model,model_type, exposure, group, type) %>%
  # Exponentiate coefficients and LCI, UCI in contrasts 
  mutate(
    Estimate = exp(Estimate),
    LCI = exp(LCI),
    UCI = exp(UCI)
  )

# Modify the exposure labels
combined_data <- bind_rows(coefs, contrasts) %>% 
  mutate(level = factor(level, levels = c("Overall", "Female", "Male", "18-39", "40-64", "65 plus", "Rural", "Urban")),
         exposure = case_when(
           exposure == "Screening of Undiagnosed \nby Diagnosed" ~ "Undiagnosed with a Diagnosed Family Member",
           TRUE ~ exposure
         ) %>%
           factor(levels = c("Any Family Member",
                             "Consanguineal (Blood Relation)",
                             "Affinal (Non-Blood Relation)",
                             "Aggregation of Diagnosed",
                             "Aggregation of Undiagnosed",
                             "Undiagnosed with a Diagnosed Family Member"
           )))


combined_data %>% 
  write_csv("paper/table_contrasts and coefficients of poisson regression.csv")

# Define custom colors and labels
custom_colors <- c("Any Family Member" = "#F4A261", 
                   "Consanguineal (Blood Relation)" = "#E76F51",
                   "Affinal (Non-Blood Relation)" = "#81B29A",  
                   "Aggregation of Diagnosed" = "#A084CA",  
                   "Aggregation of Undiagnosed" = "#F8C291",  
                   "Undiagnosed with a Diagnosed Family Member" = "#A8DADC")


# Modify figA with small boxes in the legend
figA <- combined_data %>% 
  dplyr::filter(model_type %in% c("M", "U")) %>% 
  ggplot(aes(x = Estimate, y = level, color = exposure)) +
  geom_point(size = 2, position = position_dodge(width = 0.95)) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2, position = position_dodge(width = 0.95)) +
  labs(x = "Prevalence Ratio (95% CI) of Familial Aggregation", y = "") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    axis.text.x = element_text(size = 12),  # Adjust x-axis text size
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.position = "bottom",  # Move legend to the bottom
    legend.box = "horizontal",   # Arrange legend items horizontally
    legend.box.spacing = unit(0.5, "lines"),  # Add some spacing between legend boxes
    legend.key.size = unit(1, "lines"),  # Set the size of legend keys to small boxes
    legend.title = element_blank()  # Remove the legend title
  ) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.8, 2.4), breaks = seq(0.8, 2.4, by = 0.2)) +
  geom_vline(xintercept = 1.0, col = "red", linetype = 2) +
  scale_color_manual(values = custom_colors) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                              override.aes = list(shape = 15, size = 5)))  # Use square boxes (shape = 15)

# Modify figB with small boxes in the legend
figB <- combined_data %>% 
  dplyr::filter(model_type %in% c("P", "Q", "N")) %>% 
  ggplot(aes(x = Estimate, y = level, color = exposure)) +
  geom_point(size = 2, position = position_dodge(width = 0.95)) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2, position = position_dodge(width = 0.95)) +
  labs(x = "Prevalence Ratio (95% CI) of Familial Aggregation", y = "") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    axis.text.x = element_text(size = 12),  # Adjust x-axis text size
    legend.text = element_text(size = 12),  # Adjust legend text size
    legend.position = "bottom",  # Move legend to the bottom
    legend.box = "horizontal",   # Arrange legend items horizontally
    legend.box.spacing = unit(0.5, "lines"),  # Add some spacing between legend boxes
    legend.key.size = unit(1, "lines"),  # Set the size of legend keys to small boxes
    legend.title = element_blank()  # Remove the legend title
  ) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.8, 2.4), breaks = seq(0.8, 2.4, by = 0.2)) +
  geom_vline(xintercept = 1.0, col = "red", linetype = 2) +
  scale_color_manual(values = custom_colors) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                              override.aes = list(shape = 15, size = 5)))  # Use square boxes (shape = 15)

# Combine the two figures using ggpubr::ggarrange
grouped_plot <- ggarrange(figA, figB, nrow = 1, ncol = 2, legend = "bottom", labels = c("A", "B"))

# Save the plot
ggsave(filename = paste0(path_family_aggregation_folder, "/figures/combined_models_plot.png"), 
       plot = grouped_plot, width = 15, height = 6)