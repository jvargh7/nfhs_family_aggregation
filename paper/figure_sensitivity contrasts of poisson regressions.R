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
      str_detect(term, "I\\(o_diagnosedhtn >= 1\\)")~ "At least one Diagnosed",
      str_detect(term, "I\\(o_undiagnosedhtn >= 1\\)")~ "At least one Undiagnosed",
      TRUE ~ NA_character_
    ),
    group = case_when(
      model == "R1" ~ "Diagnosed Hypertension",
      model == "W1" ~ "Undiagnosed Hypertension",
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
  # temp---------
  group_by(exposure,group, modifier, level) %>% 
  mutate(design = 1:n()) %>%
  ungroup() %>% 
  mutate(design = case_when(design == 1 ~ "all_adults_analytic_svy",
                            design == 2 ~ "hypertension_all_adults_analytic_svy",
                            TRUE ~ NA_character_)) %>% 

  # temp:end -------
  
  
  # Included 'exposure' because we need 2 sex, 3 ages and 2 regions per exposure
  distinct(design,model, exposure, level, .keep_all = TRUE) %>%
  select(design, level, term, Estimate, LCI, UCI, modifier, contrast, model,model_type, exposure, group, type) %>%
  # Exponentiate coefficients and LCI, UCI in contrasts 
  mutate(
    Estimate = exp(Estimate),
    LCI = exp(LCI),
    UCI = exp(UCI)
  )

# Modify the exposure labels
combined_data <- bind_rows(coefs, contrasts) %>% 
  mutate(level = factor(level, levels = c("Overall", "Female", "Male", "18-39", "40-64", "65 plus", "Rural", "Urban")))


combined_data %>% 
  write_csv("paper/table_sensitivity contrasts and coefficients of poisson regression.csv")

# Define custom colors and labels
custom_colors <- c(  
                   "At least one Diagnosed" = "#A084CA",  
                   "At least one Undiagnosed" = "#F8C291")


# Modify figA with small boxes in the legend
figA <- combined_data %>% 
  dplyr::filter(model_type %in% c("R", "W"),design == "all_adults_analytic_svy",model_type=="R") %>% 
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
  scale_x_continuous(limits = c(0.2, 3), breaks = seq(0.2, 3, by = 0.4)) +
  geom_vline(xintercept = 1.0, col = "red", linetype = 2) +
  scale_color_manual(values = custom_colors) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                              override.aes = list(shape = 15, size = 5)))  # Use square boxes (shape = 15)

# Modify figA with small boxes in the legend
figB <- combined_data %>% 
  dplyr::filter(model_type %in% c("R", "W"),design == "all_adults_analytic_svy",model_type=="W") %>% 
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
  scale_x_continuous(limits = c(0.2, 3), breaks = seq(0.2, 3, by = 0.4)) +
  geom_vline(xintercept = 1.0, col = "red", linetype = 2) +
  scale_color_manual(values = custom_colors) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                              override.aes = list(shape = 15, size = 5)))  # Use square boxes (shape = 15)
# Combine the two figures using ggpubr::ggarrange
library(ggpubr)
grouped_plot <- ggarrange(figA, figB, nrow = 1, ncol = 2, legend = "bottom", labels = c("A", "B"))

# Save the plot
ggsave(filename = paste0(path_family_aggregation_folder, "/figures/sensitivity combined models plot for all.jpg"), 
       plot = grouped_plot, width = 15, height = 6)


# Only Hypertension -----------

# Modify figA with small boxes in the legend
figA2 <- combined_data %>% 
  dplyr::filter(model_type %in% c("R", "W"),design == "hypertension_all_adults_analytic_svy",model_type=="R") %>% 
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
  scale_x_continuous(limits = c(0.2, 3), breaks = seq(0.2, 3, by = 0.4)) +
  geom_vline(xintercept = 1.0, col = "red", linetype = 2) +
  scale_color_manual(values = custom_colors) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                              override.aes = list(shape = 15, size = 5)))  # Use square boxes (shape = 15)

# Modify figA with small boxes in the legend
figB2 <- combined_data %>% 
  dplyr::filter(model_type %in% c("R", "W"),design == "hypertension_all_adults_analytic_svy",model_type=="W") %>% 
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
  scale_x_continuous(limits = c(0.2, 3), breaks = seq(0.2, 3, by = 0.4)) +
  geom_vline(xintercept = 1.0, col = "red", linetype = 2) +
  scale_color_manual(values = custom_colors) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, 
                              override.aes = list(shape = 15, size = 5)))  # Use square boxes (shape = 15)
# Combine the two figures using ggpubr::ggarrange
library(ggpubr)
grouped_plot <- ggarrange(figA2, figB2, nrow = 1, ncol = 2, legend = "bottom", labels = c("A", "B"))

# Save the plot
ggsave(filename = paste0(path_family_aggregation_folder, "/figures/sensitivity combined models plot for hypertension.jpg"), 
       plot = grouped_plot, width = 15, height = 6)
