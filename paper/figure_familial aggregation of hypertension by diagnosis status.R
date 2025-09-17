rm(list=ls());gc();source(".Rprofile")

# Load libraries
library(tidyverse)
library(ggpubr)

# A. Create survey designs for different family hypertension exposures ---------
# Load the data
source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")
source("functions/calculate_mean_ci_distribution.R")

anydiaghtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_diagnosedhtn >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

nodiaghtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_diagnosedhtn == 0) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")


anyundiaghtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_undiagnosedhtn >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

noundiaghtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_undiagnosedhtn == 0) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")




# Calculate estimates for different family statuses and hypertension types
anydiaghtn_htn <- calculate_mean_ci_distribution(anydiaghtn_analytic_svy, "htn_disease == '1'")
anydiaghtn_diag <- calculate_mean_ci_distribution(anydiaghtn_analytic_svy, "htn_diagnosed == '1'")
anydiaghtn_undiag <- calculate_mean_ci_distribution(anydiaghtn_analytic_svy, "htn_undiagnosed == '1'")

nodiaghtn_htn <- calculate_mean_ci_distribution(nodiaghtn_analytic_svy, "htn_disease == '1'")
nodiaghtn_diag <- calculate_mean_ci_distribution(nodiaghtn_analytic_svy, "htn_diagnosed == '1'")
nodiaghtn_undiag <- calculate_mean_ci_distribution(nodiaghtn_analytic_svy, "htn_undiagnosed == '1'")


anyundiaghtn_htn <- calculate_mean_ci_distribution(anyundiaghtn_analytic_svy, "htn_disease == '1'")
anyundiaghtn_diag <- calculate_mean_ci_distribution(anyundiaghtn_analytic_svy, "htn_diagnosed == '1'")
anyundiaghtn_undiag <- calculate_mean_ci_distribution(anyundiaghtn_analytic_svy, "htn_undiagnosed == '1'")

noundiaghtn_htn <- calculate_mean_ci_distribution(noundiaghtn_analytic_svy, "htn_disease == '1'")
noundiaghtn_diag <- calculate_mean_ci_distribution(noundiaghtn_analytic_svy, "htn_diagnosed == '1'")
noundiaghtn_undiag <- calculate_mean_ci_distribution(noundiaghtn_analytic_svy, "htn_undiagnosed == '1'")


# Create a data frame from the calculated estimates
data <- data.frame(
  Family_Status = rep(c("Any member\nhas diagnosed\nhypertension", 
                        "No member \nhas diagnosed\nhypertension",
                        "Any member\nhas undiagnosed\nhypertension",
                        "No member \nhas undiagnosed\nhypertension"), each = 3),
  Type = rep(c("Hypertension", "Diagnosed", "Undiagnosed"), times = 4),
  Estimate = c(
    anydiaghtn_htn$estimate, anydiaghtn_diag$estimate, anydiaghtn_undiag$estimate,
    nodiaghtn_htn$estimate, nodiaghtn_diag$estimate, nodiaghtn_undiag$estimate,
    anyundiaghtn_htn$estimate, anyundiaghtn_diag$estimate, anyundiaghtn_undiag$estimate,
    noundiaghtn_htn$estimate, noundiaghtn_diag$estimate, noundiaghtn_undiag$estimate
  ),
  Lower_CI = c(
    anydiaghtn_htn$lower_ci, anydiaghtn_diag$lower_ci, anydiaghtn_undiag$lower_ci,
    nodiaghtn_htn$lower_ci, nodiaghtn_diag$lower_ci, nodiaghtn_undiag$lower_ci,
    anyundiaghtn_htn$lower_ci, anyundiaghtn_diag$lower_ci, anyundiaghtn_undiag$lower_ci,
    noundiaghtn_htn$lower_ci, noundiaghtn_diag$lower_ci, noundiaghtn_undiag$lower_ci
  ),
  Upper_CI = c(
    anydiaghtn_htn$upper_ci, anydiaghtn_diag$upper_ci, anydiaghtn_undiag$upper_ci,
    nodiaghtn_htn$upper_ci, nodiaghtn_diag$upper_ci, nodiaghtn_undiag$upper_ci,
    anyundiaghtn_htn$upper_ci, anyundiaghtn_diag$upper_ci, anyundiaghtn_undiag$upper_ci,
    noundiaghtn_htn$upper_ci, noundiaghtn_diag$upper_ci, noundiaghtn_undiag$upper_ci
  )
)


write_csv(data,"paper/table_familial aggregation of hypertension by diagnosis status.csv")


# B. Load and process the coefficients data --------
coefs <- read_csv("analysis/nfaan05_poisson regression of familial aggregation.csv") %>% 
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
    level = factor(level, levels = c("Overall", "Female", "Male", "18-39", "40-64", "65 plus", "Rural", "Urban"))  ,
    exposure = factor(exposure, levels = c(
      "Aggregation of Diagnosed",
      "Aggregation of Undiagnosed"
    ))
  )

combined_data %>% 
  write_csv("paper/table_contrasts and coefficients of poisson regression by diagnosis status.csv")

# Create figure A --------
data <- read_csv("paper/table_familial aggregation of hypertension by diagnosis status.csv") %>% 
  # Convert Family_Status and Type to a factor with the desired order
  
  mutate(Family_Status = factor(Family_Status, levels = c("Any member\nhas diagnosed\nhypertension", 
                                               "No member \nhas diagnosed\nhypertension",
                                               "Any member\nhas undiagnosed\nhypertension",
                                               "No member \nhas undiagnosed\nhypertension")),
         Type = factor(Type, levels = c("Hypertension", "Diagnosed", "Undiagnosed")))




# Define colors for the Family_Status
colors <- c("Any member\nhas diagnosed\nhypertension" = "#A084CA",  # Soft Purple
            "No member \nhas diagnosed\nhypertension" = "#C2A7E1",
            "Any member\nhas undiagnosed\nhypertension" = "#D89B6A",
            "No member \nhas undiagnosed\nhypertension" = "#F8C291") 

# Create the faceted plot with labels positioned higher
format_figA = ggplot() 

figA <- ggplot(data %>% 
                  dplyr::filter(Family_Status %in% c("Any member\nhas diagnosed\nhypertension", 
                                                     "No member \nhas diagnosed\nhypertension")), 
                aes(x = Family_Status, y = Estimate, fill = Family_Status)) + geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                position = position_dodge(width = 0.8), width = 0.2) +
  geom_text(aes(label = round(Estimate, 1)), 
            position = position_dodge(width = 0.8), 
            vjust = -1.2, size = 4) +  # Adjust the vjust to move labels higher
  scale_fill_manual(name = "", values = colors) +
  labs(x = "Family Status", y = "Prevalence (%)", fill = "Family Status") +
  facet_wrap(~Type, scales = "free_x") +  # Create panels for each type of hypertension
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove the x-axis text
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.ticks.x = element_blank(),  # Optionally remove x-axis ticks
        legend.position = "bottom",
        legend.text = element_text(size = 14))  # Move the legend to the bottom


figB <- ggplot(data %>% 
                  dplyr::filter(Family_Status %in% c("Any member\nhas undiagnosed\nhypertension", 
                                                     "No member \nhas undiagnosed\nhypertension")), 
                aes(x = Family_Status, y = Estimate, fill = Family_Status)) + geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                position = position_dodge(width = 0.8), width = 0.2) +
  geom_text(aes(label = round(Estimate, 1)), 
            position = position_dodge(width = 0.8), 
            vjust = -1.2, size = 4) +  # Adjust the vjust to move labels higher
  scale_fill_manual(name = "", values = colors) +
  labs(x = "Family Status", y = "Prevalence (%)", fill = "Family Status") +
  facet_wrap(~Type, scales = "free_x") +  # Create panels for each type of hypertension
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove the x-axis text
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 12),
        axis.ticks.x = element_blank(),  # Optionally remove x-axis ticks
        legend.position = "bottom",
        legend.text = element_text(size = 14))  # Move the legend to the bottom






# Create figure B --------

# Define custom colors
custom_colors <- c(
  "Aggregation of Diagnosed" = "#A084CA",
  "Aggregation of Undiagnosed" = "#D89B6A"
)


figC <- combined_data %>%
  dplyr::filter(model_type %in% c("P", "Q")) %>%
  ggplot(aes(x = Estimate, y = level, color = exposure)) +
  geom_point(size = 2, position = position_dodge(width = 0.95)) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2, position = position_dodge(width = 0.95)) +
  labs(x = "Prevalence Ratio (95% CI)", y = "") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 14)
  ) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.8, 2.4), breaks = seq(0.8, 2.4, by = 0.2)) +
  geom_vline(xintercept = 1.0, col = "red", linetype = 2) +
  scale_color_manual(name = "", values = custom_colors) +
  guides(color=guide_legend(nrow=1))


# Combine the two figures using ggpubr::ggarrange with a single common legend from Panel B
grouped_plot <- ggarrange(ggarrange(figA,
                                    figB,nrow=2,ncol=1,labels=c("A","B")),
                          
                          figC, nrow = 1, ncol = 2, common.legend = FALSE, legend = "bottom", labels = c("", "C"))

# Save the plot
ggsave(filename = paste0(path_family_aggregation_folder, "/figures/familial aggregation of hypertension by diagnosis status.png"), 
       plot = grouped_plot, width = 12, height = 8)

