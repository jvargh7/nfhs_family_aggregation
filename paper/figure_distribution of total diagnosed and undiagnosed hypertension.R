# Loading in surveys:
source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")

# Survey design for no family member having hypertension:
nohtn_analytic_svy <- all_adults_analytic_sample %>%
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_htn == 0) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

# Survey design for any family member having hypertension:
anyhtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_htn >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

# Survey design for any consanguineal member having hypertension:
bloodhtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_htn_blood_related >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

# Survey design for any affinal member having hypertension:
nonbloodhtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_htn_not_blood_related >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

# Survey design for any member having diagnosed hypertension:
diaghtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_diagnosedhtn >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

# Survey design for any member having undiagnosed hypertension:
undiaghtn_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::mutate(htn_undiagnosed = ifelse(htn_disease == 1 & htn_diagnosed == 0, 1, 0)) %>%
  dplyr::filter(o_undiagnosedhtn >= 1) %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

# Defining a formula to estimate prevalence and confidence intervals:
calculate_mean_ci_distribution <- function(svy_design, variable, subset_expr = NULL) {
  if (!is.null(subset_expr)) {
    svy_design <- subset(svy_design, eval(subset_expr))
  }
  mean_result <- svymean(as.formula(paste("~", variable)), svy_design)
  ci_result <- confint(mean_result)
  estimate <- coef(mean_result)[1] * 100  # Convert to percentage
  lower_ci <- ci_result[1, 1] * 100  # Convert to percentage
  upper_ci <- ci_result[1, 2] * 100  # Convert to percentage
  
  result <- list(estimate = 100-estimate, 
                 lower_ci = 100-lower_ci, 
                 upper_ci = 100-upper_ci)
  return(result)
}

# Calculating estimates: 
nohtn_htn <- calculate_mean_ci_distribution(nohtn_analytic_svy, "htn_disease == '1'")
nohtn_diag <- calculate_mean_ci_distribution(nohtn_analytic_svy, "htn_diagnosed == '1'")
nohtn_undiag <- calculate_mean_ci_distribution(nohtn_analytic_svy, "htn_undiagnosed == '1'")

anyhtn_htn <- calculate_mean_ci_distribution(anyhtn_analytic_svy, "htn_disease == '1'")
anyhtn_diag <- calculate_mean_ci_distribution(anyhtn_analytic_svy, "htn_diagnosed == '1'")
anyhtn_undiag <- calculate_mean_ci_distribution(anyhtn_analytic_svy, "htn_undiagnosed == '1'")

bloodhtn_htn <- calculate_mean_ci_distribution(bloodhtn_analytic_svy, "htn_disease == '1'")
bloodhtn_diag <- calculate_mean_ci_distribution(bloodhtn_analytic_svy, "htn_diagnosed == '1'")
bloodhtn_undiag <- calculate_mean_ci_distribution(bloodhtn_analytic_svy, "htn_undiagnosed == '1'")

nonbloodhtn_htn <- calculate_mean_ci_distribution(nonbloodhtn_analytic_svy, "htn_disease == '1'")
nonbloodhtn_diag <- calculate_mean_ci_distribution(nonbloodhtn_analytic_svy, "htn_diagnosed == '1'")
nonbloodhtn_undiag <- calculate_mean_ci_distribution(nonbloodhtn_analytic_svy, "htn_undiagnosed == '1'")

diaghtn_htn <- calculate_mean_ci_distribution(diaghtn_analytic_svy, "htn_disease == '1'")
diaghtn_diag <- calculate_mean_ci_distribution(diaghtn_analytic_svy, "htn_diagnosed == '1'")
diaghtn_undiag <- calculate_mean_ci_distribution(diaghtn_analytic_svy, "htn_undiagnosed == '1'")

undiaghtn_htn <- calculate_mean_ci_distribution(undiaghtn_analytic_svy, "htn_disease == '1'")
undiaghtn_diag <- calculate_mean_ci_distribution(undiaghtn_analytic_svy, "htn_diagnosed == '1'")
undiaghtn_undiag <- calculate_mean_ci_distribution(undiaghtn_analytic_svy, "htn_undiagnosed == '1'")

# Create a data frame from the calculated estimates
data <- data.frame(
  Family_Status = rep(c("No family\nmember has\nhypertension", "Any family\nmember has\nhypertension", 
                        "Any consanguineal\nmember has\nhypertension", "Any affinal\nmember has\nhypertension", 
                        "Any member\nhas diagnosed\nhypertension", "Any member\nhas undiagnosed\nhypertension"), each = 3),
  Type = rep(c("Hypertension", "Diagnosed", "Undiagnosed"), times = 6),
  Estimate = c(nohtn_htn$estimate, nohtn_diag$estimate, nohtn_undiag$estimate,
               anyhtn_htn$estimate, anyhtn_diag$estimate, anyhtn_undiag$estimate,
               bloodhtn_htn$estimate, bloodhtn_diag$estimate, bloodhtn_undiag$estimate,
               nonbloodhtn_htn$estimate, nonbloodhtn_diag$estimate, nonbloodhtn_undiag$estimate,
               diaghtn_htn$estimate, diaghtn_diag$estimate, diaghtn_undiag$estimate,
               undiaghtn_htn$estimate, undiaghtn_diag$estimate, undiaghtn_undiag$estimate),
  Lower_CI = c(nohtn_htn$lower_ci, nohtn_diag$lower_ci, nohtn_undiag$lower_ci,
               anyhtn_htn$lower_ci, anyhtn_diag$lower_ci, anyhtn_undiag$lower_ci,
               bloodhtn_htn$lower_ci, bloodhtn_diag$lower_ci, bloodhtn_undiag$lower_ci,
               nonbloodhtn_htn$lower_ci, nonbloodhtn_diag$lower_ci, nonbloodhtn_undiag$lower_ci,
               diaghtn_htn$lower_ci, diaghtn_diag$lower_ci, diaghtn_undiag$lower_ci,
               undiaghtn_htn$lower_ci, undiaghtn_diag$lower_ci, undiaghtn_undiag$lower_ci),
  Upper_CI = c(nohtn_htn$upper_ci, nohtn_diag$upper_ci, nohtn_undiag$upper_ci,
               anyhtn_htn$upper_ci, anyhtn_diag$upper_ci, anyhtn_undiag$upper_ci,
               bloodhtn_htn$upper_ci, bloodhtn_diag$upper_ci, bloodhtn_undiag$upper_ci,
               nonbloodhtn_htn$upper_ci, nonbloodhtn_diag$upper_ci, nonbloodhtn_undiag$upper_ci,
               diaghtn_htn$upper_ci, diaghtn_diag$upper_ci, diaghtn_undiag$upper_ci,
               undiaghtn_htn$upper_ci, undiaghtn_diag$upper_ci, undiaghtn_undiag$upper_ci)
)

# Convert Family_Status and Type to a factor with the desired order
data$Family_Status <- factor(data$Family_Status, levels = c("No family\nmember has\nhypertension", "Any family\nmember has\nhypertension", 
                                                            "Any consanguineal\nmember has\nhypertension", "Any affinal\nmember has\nhypertension", 
                                                            "Any member\nhas diagnosed\nhypertension", "Any member\nhas undiagnosed\nhypertension"))

data$Type<- factor(data$Type, levels = c("Hypertension", "Diagnosed", "Undiagnosed"))

# Define colors for the types
colors <- c("Hypertension" = "skyblue", "Diagnosed" = "orange", "Undiagnosed" = "red")

# Create the plot
ggplot(data, aes(x = Family_Status, y = Estimate, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                position = position_dodge(width = 0.8), width = 0.2) +
  scale_fill_manual(values = colors) +
  labs(x = "Family Status", y = "Prevalence (%)", fill = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")  # Move the legend to the bottom

# Calculate the number of observations in each group
denominators <- data.frame(
  Family_Status = c("No family\nmember has\nhypertension", "Any family\nmember has\nhypertension", 
                    "Any consanguineal\nmember has\nhypertension", "Any affinal\nmember has\nhypertension", 
                    "Any member\nhas diagnosed\nhypertension", "Any member\nhas undiagnosed\nhypertension"),
  N = c(nrow(dplyr::filter(all_adults_analytic_sample, o_htn == 0)),
        nrow(dplyr::filter(all_adults_analytic_sample, o_htn >= 1)),
        nrow(dplyr::filter(all_adults_analytic_sample, o_htn_blood_related >= 1)),
        nrow(dplyr::filter(all_adults_analytic_sample, o_htn_not_blood_related >= 1)),
        nrow(dplyr::filter(all_adults_analytic_sample, o_diagnosedhtn >= 1)),
        nrow(dplyr::filter(all_adults_analytic_sample, o_undiagnosedhtn >= 1)))
)

# Create the plot with footnotes and estimates
p <- ggplot(data, aes(x = Family_Status, y = Estimate, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                position = position_dodge(width = 0.8), width = 0.2) +
  geom_text(aes(label = round(Estimate / 100 * denominators$N[match(Family_Status, denominators$Family_Status)], 0), 
                y = Estimate + 1), 
            position = position_dodge(width = 0.8), size = 3, vjust = 0) +  # Add actual numbers on bars
  scale_fill_manual(values = colors) +
  labs(x = "Family Status", y = "Prevalence (%)", fill = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, size = 8, hjust = 0.5, vjust = 1, margin = margin(b = 5)),  # Add top margin
        legend.position = "bottom") +  # Move the legend to the bottom
  annotate("text", x = 1:6, y = -5, label = paste("N =", denominators$N), size = 3, hjust = 0.5, vjust = 1, color = "black", fontface = "bold")  # Adjust y for footnotes

# Print the plot
print(p)

# Save the plot
ggsave("paper/distributions_of_htn.png", plot = p, width = 8, height = 6)


