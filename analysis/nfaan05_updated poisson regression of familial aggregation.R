rm(list=ls()); gc(); source(".Rprofile")

source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")

## @KRISHNA - Please fit for S*, T*, U*, P*, Q*, N* for the single main exposure -------------
## IGNORE V*

source("analysis/nfaan_poisson regression equations.R")

# Define functions
fit_and_tidy_model <- function(formula, design, model_name) {
  model <- svyglm(formula = as.formula(formula), design = design, family = quasipoisson())
  tidy_model <- broom::tidy(model) %>% mutate(model = model_name)
  rm(model)
  gc()
  return(tidy_model)
}

calculate_contrasts <- function(svymodel, modifier, exposure, contrast_name) {
  contrasts <- contrasts_svyglm(svymodel = svymodel, modifier = modifier, exposure = exposure) %>% mutate(contrast = contrast_name)
  return(contrasts)
}

# Fit, tidy, and save models
models <- list(
  list(formula = m0, design = all_adults_analytic_svy, name = "M0"),
  list(formula = m1, design = all_adults_analytic_svy, name = "M1"),
  list(formula = m2, design = all_adults_analytic_svy, name = "M2"),
  list(formula = m3, design = all_adults_analytic_svy, name = "M3"),
  list(formula = m4, design = all_adults_analytic_svy, name = "M4"),
  list(formula = s0, design = all_adults_analytic_svy, name = "S0"),
  list(formula = s1, design = all_adults_analytic_svy, name = "S1"),
  list(formula = s2, design = all_adults_analytic_svy, name = "S2"),
  list(formula = s3, design = all_adults_analytic_svy, name = "S3"),
  list(formula = s4, design = all_adults_analytic_svy, name = "S4"),
  list(formula = t0, design = all_adults_analytic_svy, name = "T0"),
  list(formula = t1, design = all_adults_analytic_svy, name = "T1"),
  list(formula = t2, design = all_adults_analytic_svy, name = "T2"),
  list(formula = t3, design = all_adults_analytic_svy, name = "T3"),
  list(formula = t4, design = all_adults_analytic_svy, name = "T4"),
  list(formula = u0, design = all_adults_analytic_svy, name = "U0"),
  list(formula = u1, design = all_adults_analytic_svy, name = "U1"),
  list(formula = u2, design = all_adults_analytic_svy, name = "U2"),
  list(formula = u3, design = all_adults_analytic_svy, name = "U3"),
  list(formula = u4, design = all_adults_analytic_svy, name = "U4"),
  list(formula = p0, design = hypertension_all_adults_analytic_svy, name = "P0"),
  list(formula = p1, design = hypertension_all_adults_analytic_svy, name = "P1"),
  list(formula = p2, design = hypertension_all_adults_analytic_svy, name = "P2"),
  list(formula = p3, design = hypertension_all_adults_analytic_svy, name = "P3"),
  list(formula = p4, design = hypertension_all_adults_analytic_svy, name = "P4"),
  list(formula = q0, design = nondisease_all_adults_analytic_svy, name = "Q0"),
  list(formula = q1, design = nondisease_all_adults_analytic_svy, name = "Q1"),
  list(formula = q2, design = nondisease_all_adults_analytic_svy, name = "Q2"),
  list(formula = q3, design = nondisease_all_adults_analytic_svy, name = "Q3"),
  list(formula = q4, design = nondisease_all_adults_analytic_svy, name = "Q4"),
  list(formula = n0, design = nondisease_all_adults_analytic_svy, name = "N0"),
  list(formula = n1, design = nondisease_all_adults_analytic_svy, name = "N1"),
  list(formula = n2, design = nondisease_all_adults_analytic_svy, name = "N2"),
  list(formula = n3, design = nondisease_all_adults_analytic_svy, name = "N3"),
  list(formula = n4, design = nondisease_all_adults_analytic_svy, name = "N4")
)

all_coefs <- lapply(models, function(model) {
  fit_and_tidy_model(model$formula, model$design, model$name)
}) %>% bind_rows()

# Save all coefficients into a CSV
all_coefs %>%
  mutate(Coef_CI = paste0(round(exp(estimate),2)," \t(",
                          round(exp(estimate-1.96*std.error),2),", ",
                          round(exp(estimate+1.96*std.error),2),")"),
         PR = exp(estimate),
         lci = exp(estimate-1.96*std.error),
         uci = exp(estimate+1.96*std.error)
  ) %>%
  write_csv("analysis/nfaan05_poisson_regression_of_familial_aggregation.csv")

# Load contrast functions
source("/Users/krishnasanaka/Desktop/Public Health Research/prepare_contrasts.R")
source("/Users/krishnasanaka/Desktop/Public Health Research/contrasts_svyglm.R")
source("/Users/krishnasanaka/Desktop/Public Health Research/round_d.R")

# Define contrast list
contrast_list <- list(
  list(model_name = "model_m2", modifier = "sexMale", exposure = "I(o_htn >= 1)TRUE", name = "M2_sexMale"),
  list(model_name = "model_m3", modifier = "age_category40-64", exposure = "I(o_htn >= 1)TRUE", name = "M3_age_category40-64"),
  list(model_name = "model_m3", modifier = "age_category65 plus", exposure = "I(o_htn >= 1)TRUE", name = "M3_age_category65 plus"),
  list(model_name = "model_m4", modifier = "residenceUrban", exposure = "I(o_htn >= 1)TRUE", name = "M4_residenceUrban"),
  list(model_name = "model_s2", modifier = "sexMale", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "S2_sexMale"),
  list(model_name = "model_s3", modifier = "age_category40-64", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "S3_age_category40-64"),
  list(model_name = "model_s3", modifier = "age_category65 plus", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "S3_age_category65 plus"),
  list(model_name = "model_s4", modifier = "residenceUrban", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "S4_residenceUrban"),
  list(model_name = "model_t2", modifier = "sexMale", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "T2_sexMale"),
  list(model_name = "model_t3", modifier = "age_category40-64", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "T3_age_category40-64"),
  list(model_name = "model_t3", modifier = "age_category65 plus", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "T3_age_category65 plus"),
  list(model_name = "model_t4", modifier = "residenceUrban", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "T4_residenceUrban"),
  list(model_name = "model_p2", modifier = "sexMale", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "P2_sexMale"),
  list(model_name = "model_p3", modifier = "age_category40-64", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "P3_age_category40-64"),
  list(model_name = "model_p3", modifier = "age_category65 plus", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "P3_age_category65 plus"),
  list(model_name = "model_p4", modifier = "residenceUrban", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "P4_residenceUrban"),
  list(model_name = "model_q2", modifier = "sexMale", exposure = "I(o_undiagnosedhtn >= 1)TRUE", name = "Q2_sexMale"),
  list(model_name = "model_q3", modifier = "age_category40-64", exposure = "I(o_undiagnosedhtn >= 1)TRUE", name = "Q3_age_category40-64"),
  list(model_name = "model_q3", modifier = "age_category65 plus", exposure = "I(o_undiagnosedhtn >= 1)TRUE", name = "Q3_age_category65 plus"),
  list(model_name = "model_q4", modifier = "residenceUrban", exposure = "I(o_undiagnosedhtn >= 1)TRUE", name = "Q4_residenceUrban"),
  list(model_name = "model_n2", modifier = "sexMale", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "N2_sexMale"),
  list(model_name = "model_n3", modifier = "age_category40-64", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "N3_age_category40-64"),
  list(model_name = "model_n3", modifier = "age_category65 plus", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "N3_age_category65 plus"),
  list(model_name = "model_n4", modifier = "residenceUrban", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "N4_residenceUrban"),
  list(model_name = "model_u2", modifier = "sexMale", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "U2_sexMale"),
  list(model_name = "model_u3", modifier = "age_category40-64", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "U3_age_category40-64"),
  list(model_name = "model_u3", modifier = "age_category65 plus", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "U3_age_category65 plus"),
  list(model_name = "model_u4", modifier = "residenceUrban", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "U4_residenceUrban"),
  list(model_name = "model_u2", modifier = "sexMale", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "U2_sexMale"),
  list(model_name = "model_u3", modifier = "age_category40-64", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "U3_age_category40-64"),
  list(model_name = "model_u3", modifier = "age_category65 plus", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "U3_age_category65 plus"),
  list(model_name = "model_u4", modifier = "residenceUrban", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "U4_residenceUrban")
)

# Calculate and save contrasts
all_contrasts <- lapply(contrast_list, function(contrast) {
  svymodel <- get(contrast$model_name)
  calculate_contrasts(svymodel, contrast$modifier, contrast$exposure, contrast$name)
}) %>% bind_rows()

# Save all contrast estimates into a CSV
write_csv(all_contrasts, "analysis/nfaan05_contrasts_of_poisson_regression_of_familial_aggregation.csv")

