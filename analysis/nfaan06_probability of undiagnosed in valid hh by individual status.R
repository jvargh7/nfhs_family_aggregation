# Clean the environment
rm(list=ls()); gc(); source(".Rprofile")

# Load the data
source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")

# Fitting the svyglm model with interaction term
svy_model <- svyglm(o_undiagnosedhtn ~ htn_status*valid_size_cat, 
                    family = poisson(link = "log"), 
                    design = all_adults_analytic_svy)

# Extract the coefficient table from the svyglm model
# Apply the transformations to compute confidence intervals and exponentiated coefficients
all_coefs <- broom::tidy(svy_model) %>%
  mutate(Coef_CI = paste0(round(exp(estimate), 2), " (", 
                          round(exp(estimate - 1.96 * std.error), 2), ", ", 
                          round(exp(estimate + 1.96 * std.error), 2), ")"),
         PR = exp(estimate),  # Exponentiated coefficient
         lci = exp(estimate - 1.96 * std.error),  # Lower 95% CI
         uci = exp(estimate + 1.96 * std.error)   # Upper 95% CI
  )

# Prepare household count data (distinct clusters)
individual_proportions <- all_adults_analytic_svy$variables %>% 
  # JV: This is tricky because htn_status will be picked from the one observation that is left after distinct(). I removed distinct to get a count of no of individuals
  # distinct(cluster, hhid, .keep_all = TRUE) %>% 
  as_survey_design(.data = ., ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer") %>% 
  group_by(htn_status, valid_size_cat) %>%
  srvyr::summarize(n = survey_total(wt=sampleweight),
            p = survey_prop(vartype="ci"))

# # To calculate standard errors for household counts
individuals_unweighted <- all_adults_analytic_svy$variables %>%
  # JV: This is tricky because htn_status will be picked from the one observation that is left after distinct(). I removed distinct to get a count of no of individuals
  # distinct(cluster, hhid, .keep_all = TRUE) %>%
  group_by(htn_status, valid_size_cat) %>%
  summarize(unweighted_n = n(), wt_sum = sum(sampleweight))

# Estimate predicted probability of each category of htn_status*valid_size_cat
# Generating predictions along with 95% confidence intervals
data_matrix <- expand.grid(htn_status = c("d", "n", "u"),
                           valid_size_cat = c("2", "3-4", "5")) 

# Generate predicted probabilities and standard errors
predictions_with_se <- predict(svy_model, newdata = data_matrix, type = "response", se.fit = TRUE)
predictions_with_se <- as.data.frame(predictions_with_se)

# Mutate predictions_with_se to have 95% confidence intervals:
predictions_with_se <- predictions_with_se %>%
  mutate(
    lower_CI = response - 1.96 * SE,       # Calculate the lower bound of the 95% confidence interval
    upper_CI = response + 1.96 * SE        # Calculate the upper bound of the 95% confidence interval
  )

# Adding the datasets together:
data_matrix <- dplyr::bind_cols(data_matrix, predictions_with_se)

# Merge predicted probabilities with household counts
individual_proportions_with_prob <- individual_proportions %>%
  left_join(data_matrix, by = c("htn_status", "valid_size_cat")) %>%
  left_join(individuals_unweighted, by = c("htn_status", "valid_size_cat"))

# Save the results to a CSV file
write_csv(individual_proportions_with_prob, "analysis/nfaan06_individuals counts with predicted probability.csv")

# Summarize predicted probability of undiagnosed hypertension for each htn_status
individual_proportions_with_prob %>% 
  group_by(htn_status) %>% 
  summarize(prob_undiagnosed = sum(n * response) / sum(n))

individual_proportions_with_prob <- read_csv("analysis/nfaan06_individuals counts with predicted probability.csv")

