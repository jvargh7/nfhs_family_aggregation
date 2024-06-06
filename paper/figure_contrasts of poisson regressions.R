library(tidyverse)

# Load and process the coefficients data
coefs <- read_csv("analysis/nfaan05_poisson regression of familial aggregation.csv") %>% 
  dplyr::filter(str_detect(term, "I\\(o_")) %>%
  dplyr::filter(model %in% c("M1", "N1", "P1", "Q1",
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
      str_detect(term, "I\\(o_diagnosedhtn >= 1\\)") & model == "P1" ~ "Aggregation of Diagnosed",
      str_detect(term, "I\\(o_undiagnosedhtn >= 1\\)") & model == "Q1" ~ "Aggregation of Undiagnosed",
      str_detect(term, "I\\(o_diagnosedhtn >= 1\\)") & model == "N1" ~ "Screening of Undiagnosed \nby Diagnosed",
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
      str_detect(term, "I\\(o_diagnosedhtn >= 1\\)") & model_type == "P" ~ "Aggregation of Diagnosed",
      str_detect(term, "I\\(o_undiagnosedhtn >= 1\\)") & model_type == "Q" ~ "Aggregation of Undiagnosed",
      str_detect(term, "I\\(o_diagnosedhtn >= 1\\)") & model_type == "N" ~ "Screening of Undiagnosed \nby Diagnosed",
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
  dplyr::filter(model_type %in% c("M", "N", "P", "Q",
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

# Combine coefficients and contrasts data
combined_data <- bind_rows(coefs, contrasts) %>% 
  mutate(level = factor(level,levels=c("Overall","Female","Male","18-39","40-64","65 plus","Rural","Urban")),
         exposure = factor(exposure,
                           levels = c("Any Family Member",
                                      "Consanguineal (Blood Relation)",
                                      "Affinal (Non-Blood Relation)",
                                      "Aggregation of Diagnosed",
                                      "Aggregation of Undiagnosed",
                                      "Screening of Undiagnosed \nby Diagnosed"
                                      )))

combined_data %>% 
  write_csv("paper/table_contrasts and coefficients of poisson regression.csv")

figA = combined_data %>% 
  dplyr::filter(model_type %in% c("M","U")) %>% 
  ggplot(., aes(x = Estimate, y = level, color = exposure)) +
  geom_point(size = 2,position=position_dodge(width=0.95)) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI), height = 0.2,position=position_dodge(width=0.95)) +
  labs(
    x = "Prevalence Ratio (95% CI) of Familial Aggregation",
    y = ""
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    axis.text.x = element_text(size = 12),   # Adjust x-axis text size
    legend.text = element_text(size = 12)
  ) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(limits=c(0.8,2.4),breaks=seq(0.8,2.4,by=0.2)) +
  geom_vline(xintercept=1.0,col="red",linetype=2) +
  scale_color_manual(name="",values=c("black","#FFB6AD","#A6EAD1")) +
  guides(color=guide_legend(nrow=2))

figA


figB = combined_data %>% 
  dplyr::filter(model_type %in% c("P","Q","N")) %>% 
  ggplot(., aes(x = Estimate, y = level, color = exposure)) +
  geom_point(size = 2,position=position_dodge(width=0.95)) +
  geom_errorbarh(aes(xmin = LCI, xmax = UCI),height = 0.2,position=position_dodge(width=0.95)) +
  labs(
    x = "Prevalence Ratio (95% CI) of Familial Aggregation",
    y = ""
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 14),  # Adjust y-axis text size
    axis.text.x = element_text(size = 12),   # Adjust x-axis text size
    legend.text = element_text(size = 12)
  ) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(limits=c(0.8,2.4),breaks=seq(0.8,2.4,by=0.2)) +
  geom_vline(xintercept=1.0,col="red",linetype=2) +
  scale_color_manual(name="",values=c("#98DB37","#FF7968","#98DBE7")) +
  guides(color=guide_legend(nrow=2))

figB

library(ggpubr)

grouped_plot = ggarrange(figA,
          figB,
          nrow=1,
          ncol=2,
          # common.legend = TRUE,
          legend = "bottom",
          labels=c("A","B")) 

# Save the plot
ggsave(filename=paste0(path_family_aggregation_folder,"/figures/combined_models_plot.png"), grouped_plot, width = 12, height = 6)

