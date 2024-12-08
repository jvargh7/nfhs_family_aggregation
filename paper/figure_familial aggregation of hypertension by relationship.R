rm(list=ls());gc();source(".Rprofile")


library(tidyverse)
library(ggpubr)

# Load the data
source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")
source("functions/calculate_mean_ci_distribution.R")


# FIG A. DATASET ------------

# Create survey designs for different family hypertension exposures
anyhtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_htn >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

nohtn_analytic_svy <- all_adults_analytic_sample %>%
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_htn == 0) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

anyconsanghtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_htn_blood_related >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

noconsanghtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_htn_blood_related == 0) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")


anyaffinalhtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_htn_not_blood_related >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")


noaffinalhtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_htn_not_blood_related == 0) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")



# Calculate estimates for different family statuses and hypertension types
nohtn_htn <- calculate_mean_ci_distribution(nohtn_analytic_svy, "htn_disease == '1'")
nohtn_diag <- calculate_mean_ci_distribution(nohtn_analytic_svy, "htn_diagnosed == '1'")
nohtn_undiag <- calculate_mean_ci_distribution(nohtn_analytic_svy, "htn_undiagnosed == '1'")

anyhtn_htn <- calculate_mean_ci_distribution(anyhtn_analytic_svy, "htn_disease == '1'")
anyhtn_diag <- calculate_mean_ci_distribution(anyhtn_analytic_svy, "htn_diagnosed == '1'")
anyhtn_undiag <- calculate_mean_ci_distribution(anyhtn_analytic_svy, "htn_undiagnosed == '1'")

anyconsanghtn_htn <- calculate_mean_ci_distribution(anyconsanghtn_analytic_svy, "htn_disease == '1'")
anyconsanghtn_diag <- calculate_mean_ci_distribution(anyconsanghtn_analytic_svy, "htn_diagnosed == '1'")
anyconsanghtn_undiag <- calculate_mean_ci_distribution(anyconsanghtn_analytic_svy, "htn_undiagnosed == '1'")

noconsanghtn_htn <- calculate_mean_ci_distribution(noconsanghtn_analytic_svy, "htn_disease == '1'")
noconsanghtn_diag <- calculate_mean_ci_distribution(noconsanghtn_analytic_svy, "htn_diagnosed == '1'")
noconsanghtn_undiag <- calculate_mean_ci_distribution(noconsanghtn_analytic_svy, "htn_undiagnosed == '1'")


anyaffinalhtn_htn <- calculate_mean_ci_distribution(anyaffinalhtn_analytic_svy, "htn_disease == '1'")
anyaffinalhtn_diag <- calculate_mean_ci_distribution(anyaffinalhtn_analytic_svy, "htn_diagnosed == '1'")
anyaffinalhtn_undiag <- calculate_mean_ci_distribution(anyaffinalhtn_analytic_svy, "htn_undiagnosed == '1'")

noaffinalhtn_htn <- calculate_mean_ci_distribution(noaffinalhtn_analytic_svy, "htn_disease == '1'")
noaffinalhtn_diag <- calculate_mean_ci_distribution(noaffinalhtn_analytic_svy, "htn_diagnosed == '1'")
noaffinalhtn_undiag <- calculate_mean_ci_distribution(noaffinalhtn_analytic_svy, "htn_undiagnosed == '1'")


# Create a data frame from the calculated estimates
data <- data.frame(
  Family_Status = rep(c("No family\nmember has\nhypertension", 
                        "Any family\nmember has\nhypertension", 
                        "Any consanguineal\nmember has\nhypertension", 
                        "No consanguineal\nmember has\nhypertension", 
                        "Any affinal\nmember has\nhypertension",
                        "No affinal\nmember has\nhypertension"
                        ), each = 3),
  Type = rep(c("Hypertension", "Diagnosed", "Undiagnosed"), times = 6)) %>% 
  bind_cols(
    bind_rows(data.frame(nohtn_htn),
              data.frame(nohtn_diag),
              data.frame(nohtn_undiag),
              data.frame(anyhtn_htn),
              data.frame(anyhtn_diag),
              data.frame(anyhtn_undiag),
              data.frame(anyconsanghtn_htn),
              data.frame(anyconsanghtn_diag),
              data.frame(anyconsanghtn_undiag),
              data.frame(noconsanghtn_htn),
              data.frame(noconsanghtn_diag),
              data.frame(noconsanghtn_undiag),
              data.frame(anyaffinalhtn_htn),
              data.frame(anyaffinalhtn_diag),
              data.frame(anyaffinalhtn_undiag),
              data.frame(noaffinalhtn_htn),
              data.frame(noaffinalhtn_diag),
              data.frame(noaffinalhtn_undiag)
              ))
write_csv(data,"paper/table_familial aggregation of hypertension by relationship.csv")

  
  
# B. Load and process the coefficients data -----------
coefs <- read_csv("analysis/nfaan05_poisson regression of familial aggregation.csv") %>% 
  dplyr::filter(str_detect(term, "I\\(o_")) %>%
  dplyr::filter(model %in% c("M1",
                             # "S1", "T1", 
                             "U1")) %>%
  mutate(
    type = "Coefficient",
    level = "Overall",
    contrast = "Overall",
    modifier = "None",
    model_type = substr(model, 1, 1),
    exposure = case_when(
      str_detect(term, "I\\(o_htn >= 1\\)") ~ "Any Family Member",
      str_detect(term, "I\\(o_htn_blood_related >= 1\\)") ~ "Consanguineal (Blood Relation)",
      str_detect(term, "I\\(o_htn_not_blood_related >= 1\\)") ~ "Affinal (Non-Blood Relation)",
      TRUE ~ NA_character_
    ),
    group = case_when(
      model == "M1" ~ "Disease Predicting Disease",
      model == "S1" ~ "Disease Predicted by Disease in Blood Relation",
      model == "T1" ~ "Disease Predicted by Disease in Non-Blood Relation",
      model == "U1" ~ "Disease Predicted by Disease in Blood Relation and Non-Blood Relation",
      TRUE ~ "Other"
    )
  ) %>%
  # Ensure Estimate, LCI, and UCI columns exist and are correctly named in coefs
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
    model_type = substr(modifier, 1, 1),  # Extract first character for model type letter
    model = substr(modifier, 1, 2),  # Extract first two characters for term
    modifier = substr(modifier, 4, nchar(modifier)),  # Remove first three characters from modifier
    exposure = case_when(
      str_detect(term, "I\\(o_htn >= 1\\)") ~ "Any Family Member",
      str_detect(term, "I\\(o_htn_blood_related >= 1\\)") ~ "Consanguineal (Blood Relation)",
      str_detect(term, "I\\(o_htn_not_blood_related >= 1\\)") ~ "Affinal (Non-Blood Relation)",
      TRUE ~ NA_character_
    ),
    group = case_when(
      model_type == "M" ~ "Disease Predicting Disease",
      model_type == "S" ~ "Disease Predicted by Disease in Blood Relation",
      model_type == "T" ~ "Disease Predicted by Disease in Non-Blood Relation",
      model_type == "U" ~ "Disease Predicted by Disease in Blood Relation and Non-Blood Relation",
      TRUE ~ "Other"
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
  dplyr::filter(model_type %in% c("M",
                                  # Excluded S & T because U is more useful
                                  # "S", "T", 
                                  "U")) %>%
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
(combined_data <- bind_rows(coefs, contrasts)) %>% 
  write_csv("paper/table_contrasts and coefficients of poisson regression by relationship.csv")

  
 
# Create figure A ---------------
data <- read_csv("paper/table_familial aggregation of hypertension by relationship.csv") %>% 
  mutate(Family_Status = factor(Family_Status,
                                levels = c("Any family\nmember has\nhypertension", 
                                           "No family\nmember has\nhypertension", 
                                           
                                           "Any consanguineal\nmember has\nhypertension", 
                                           "No consanguineal\nmember has\nhypertension", 
                                           
                                           "Any affinal\nmember has\nhypertension",
                                           "No affinal\nmember has\nhypertension"
                                           )),
         Type = factor(Type, levels = c("Hypertension", "Diagnosed", "Undiagnosed"))) %>% 
  rename(Estimate = estimate,
         Lower_CI = lower_ci,
         Upper_CI = upper_ci)

# Define colors for the Family_Status
colors <- c("No family\nmember has\nhypertension" = "#A8DADC",   # Soft Sky Blue
            "Any family\nmember has\nhypertension" = "#F4A261",  # Soft Orange
            "Any consanguineal\nmember has\nhypertension" = "#E76F51",  # Soft Red
            "No consanguineal\nmember has\nhypertension" = "#F2A88C",  # Softer Red
            "Any affinal\nmember has\nhypertension" = "#81B29A",  # Soft Green
            "No affinal\nmember has\nhypertension" = "#A5D2C3" # Softer Green
            ) 

# Create the faceted plot with labels positioned higher
figA <- data %>% 
  dplyr::filter(Family_Status %in% c("No family\nmember has\nhypertension", 
                "Any family\nmember has\nhypertension")) %>% 
  ggplot(., aes(x = Family_Status, y = Estimate, fill = Family_Status)) +
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
        legend.position = "bottom") +  # Move the legend to the bottom
  scale_y_continuous(limits = c(0, max(data$Upper_CI) * 1.2))  # Extend to 20% above the max value
  

figB <- data %>% 
  dplyr::filter(Family_Status %in% c("Any consanguineal\nmember has\nhypertension", 
                                     "No consanguineal\nmember has\nhypertension")) %>% 
  ggplot(., aes(x = Family_Status, y = Estimate, fill = Family_Status)) +
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
        legend.position = "bottom") +  # Move the legend to the bottom
  scale_y_continuous(limits = c(0, max(data$Upper_CI) * 1.2))  # Extend to 20% above the max value


figC <- data %>% 
  dplyr::filter(Family_Status %in% c("Any affinal\nmember has\nhypertension",
                                     "No affinal\nmember has\nhypertension")) %>% 
  ggplot(., aes(x = Family_Status, y = Estimate, fill = Family_Status)) +
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
        legend.position = "bottom") +  # Move the legend to the bottom
  scale_y_continuous(limits = c(0, max(data$Upper_CI) * 1.2))  # Extend to 20% above the max value

#Create figure D -----------------

# Define custom colors and labels
custom_colors <- c("Any family\nmember has\nhypertension" = "#F4A261", 
                   "Any consanguineal\nmember has\nhypertension" = "#E76F51",
                   "Any affinal\nmember has\nhypertension" = "#81B29A"  
                   )


# Modify figA with small boxes in the legend

combined_data = read_csv("paper/table_contrasts and coefficients of poisson regression by relationship.csv")  %>% 
  mutate(
    level = factor(level, levels = c("Overall", "Female", "Male", "18-39", "40-64", "65 plus", "Rural", "Urban")),
    exposure = case_when(
      exposure == "Screening of Undiagnosed \nby Diagnosed" ~ "Undiagnosed with a Diagnosed Family Member",
      TRUE ~ exposure
    ) %>%
      factor(levels = c(
        "Any Family Member",
        "Consanguineal (Blood Relation)",
        "Affinal (Non-Blood Relation)"
      ),
      labels = c("Any family\nmember has\nhypertension",
                 "Any consanguineal\nmember has\nhypertension",
                 "Any affinal\nmember has\nhypertension"))
  )

figD <-   combined_data %>% 
  dplyr::filter(model_type %in% c("M", "U")) %>% 
  ggplot(aes(x = Estimate, y = level, color = exposure)) +  # Keep `color = exposure` for colors
  geom_point(size = 2, position = position_dodge(width = 0.95)) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2, position = position_dodge(width = 0.95)) +
  labs(x = "Prevalence Ratio (95% CI) of Familial Aggregation", y = "") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12)
  ) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0.8, 2.0), breaks = seq(0.8, 2.0, by = 0.2)) +
  geom_vline(xintercept = 1.0, col = "red", linetype = 2) +
  scale_color_manual(name = "",values = custom_colors) +
  guides(color=guide_legend(nrow=1))

figD

# Combine the two figures using ggpubr::ggarrange with a single common legend from Panel B
grouped_plot <- ggarrange(ggarrange(figA,figB,figC,nrow=3,ncol=1,
                                    common.legend=FALSE,labels=c("A","B","C")),
                          
                          figD, nrow = 1, ncol = 2, common.legend = FALSE, legend = "bottom", labels = c("", "D"))

# Save the plot
ggsave(filename = paste0(path_family_aggregation_folder, "/figures/familial aggregation of hypertension by relation.png"), 
       plot = grouped_plot, width = 12, height = 8)

