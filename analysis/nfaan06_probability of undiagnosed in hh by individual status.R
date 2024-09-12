rm(list=ls()); gc(); source(".Rprofile")

source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")

# Fitting the model
# Added interaction term - coefficients needed for each level of htn_size_cat
# geeglm: Works on a usual data frame, Clustering of observations within a level, need to specify covariance matrix and weights
# svyglm: Requires a survey object, accounts for clustering and weights as specified in the survey object
svy_model <- svyglm(o_undiagnosedhtn ~ htn_status*hh_size_cat, 
                    family = poisson(link = "log"), 
                    design = all_adults_analytic_svy)

# Extract the coefficient table from the GEE/Svyglm model
# Apply the transformations to compute confidence intervals and exponentiated coefficients
all_coefs <- broom::tidy(svy_model) %>%
  mutate(Coef_CI = paste0(round(exp(estimate), 2), " \t(",
                          round(exp(estimate - 1.96 * std.error), 2), ", ",
                          round(exp(estimate + 1.96 * std.error), 2), ")"),
         PR = exp(estimate), # Exponentiated coefficient
         lci = exp(estimate - 1.96 * std.error), # Lower 95% CI
         uci = exp(estimate + 1.96 * std.error)  # Upper 95% CI
  )


household_counts = all_adults_analytic_svy$variables %>% 
  distinct(cluster,hhid,.keep_all = TRUE) %>% 
  as_survey_design(.data = ., ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer") %>% 
  group_by(htn_status, hh_size_cat) %>% 
  survey_tally()

# Estimate predicted probability of each category of htn_status*hh_size_cat

# Check if unweighted predict() of svyglm() objects gives the right estimates
data_matrix <- expand.grid(htn_status = c("d","n","u"),
                           hh_size_cat = c("2","3-4","5")) %>% 
  mutate(pred_prob = predict(svy_model,newdata = .,type = "response"))


household_counts_with_prob = household_counts %>% 
  left_join(data_matrix,
            by =c("htn_status","hh_size_cat"))

write_csv(household_counts_with_prob,"analysis/nfaan06_household counts with predicted probability.csv")

household_counts_with_prob %>% 
  group_by(htn_status) %>% 
  summarize(prob_undiagnosed = sum(n*pred_prob)/sum(n))

