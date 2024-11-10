rm(list=ls()); gc(); source(".Rprofile")

source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")

## @KRISHNA - Please fit for S*, T*, U*, P*, Q*, N* for the single main exposure -------------
## IGNORE V*

source("analysis/nfaan_poisson regression equations.R")

if(Sys.info()["user"]=="JVARGH7"){
  source("C:/code/external/functions/survey/contrasts_svyglm.R")
  
  
} else if(Sys.info()["user"] == "krishnasanaka"){
  
  # Load contrast functions
  source("/Users/krishnasanaka/Desktop/Public Health Research/prepare_contrasts.R")
  source("/Users/krishnasanaka/Desktop/Public Health Research/contrasts_svyglm.R")
  source("/Users/krishnasanaka/Desktop/Public Health Research/round_d.R")
  
}

# Define function to fit, tidy, and calculate contrasts
fit_tidy_and_contrast_model <- function(formula, design, model_name, contrast_list) {
  model <- svyglm(formula = as.formula(formula), design = design, family = quasipoisson())
  tidy_model <- broom::tidy(model) %>% mutate(model = model_name)
  
  contrasts <- lapply(contrast_list, function(contrast) {
    if (contrast$model_name == model_name) {
      contrasts_svyglm(svymodel = model, modifier = contrast$modifier, exposure = contrast$exposure) %>% mutate(contrast = contrast$name)
    } else {
      NULL
    }
  }) %>% bind_rows()
  
  rm(model)
  gc()
  return(list(tidy_model = tidy_model, contrasts = contrasts))
}

# Define models and contrasts
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

contrast_list <- list(
  list(model_name = "M2", modifier = "sexMale", exposure = "I(o_htn >= 1)TRUE", name = "M2_sexMale"),
  list(model_name = "M3", modifier = "age_category40-64", exposure = "I(o_htn >= 1)TRUE", name = "M3_age_category40-64"),
  list(model_name = "M3", modifier = "age_category65 plus", exposure = "I(o_htn >= 1)TRUE", name = "M3_age_category65 plus"),
  list(model_name = "M4", modifier = "residenceUrban", exposure = "I(o_htn >= 1)TRUE", name = "M4_residenceUrban"),
  list(model_name = "S2", modifier = "sexMale", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "S2_sexMale"),
  list(model_name = "S3", modifier = "age_category40-64", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "S3_age_category40-64"),
  list(model_name = "S3", modifier = "age_category65 plus", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "S3_age_category65 plus"),
  list(model_name = "S4", modifier = "residenceUrban", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "S4_residenceUrban"),
  list(model_name = "T2", modifier = "sexMale", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "T2_sexMale"),
  list(model_name = "T3", modifier = "age_category40-64", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "T3_age_category40-64"),
  list(model_name = "T3", modifier = "age_category65 plus", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "T3_age_category65 plus"),
  list(model_name = "T4", modifier = "residenceUrban", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "T4_residenceUrban"),
  list(model_name = "P2", modifier = "sexMale", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "P2_sexMale"),
  list(model_name = "P3", modifier = "age_category40-64", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "P3_age_category40-64"),
  list(model_name = "P3", modifier = "age_category65 plus", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "P3_age_category65 plus"),
  list(model_name = "P4", modifier = "residenceUrban", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "P4_residenceUrban"),
  list(model_name = "Q2", modifier = "sexMale", exposure = "I(o_undiagnosedhtn >= 1)TRUE", name = "Q2_sexMale"),
  list(model_name = "Q3", modifier = "age_category40-64", exposure = "I(o_undiagnosedhtn >= 1)TRUE", name = "Q3_age_category40-64"),
  list(model_name = "Q3", modifier = "age_category65 plus", exposure = "I(o_undiagnosedhtn >= 1)TRUE", name = "Q3_age_category65 plus"),
  list(model_name = "Q4", modifier = "residenceUrban", exposure = "I(o_undiagnosedhtn >= 1)TRUE", name = "Q4_residenceUrban"),
  list(model_name = "N2", modifier = "sexMale", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "N2_sexMale"),
  list(model_name = "N3", modifier = "age_category40-64", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "N3_age_category40-64"),
  list(model_name = "N3", modifier = "age_category65 plus", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "N3_age_category65 plus"),
  list(model_name = "N4", modifier = "residenceUrban", exposure = "I(o_diagnosedhtn >= 1)TRUE", name = "N4_residenceUrban"),
  list(model_name = "U2", modifier = "sexMale", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "U2_sexMale"),
  list(model_name = "U3", modifier = "age_category40-64", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "U3_age_category40-64"),
  list(model_name = "U3", modifier = "age_category65 plus", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "U3_age_category65 plus"),
  list(model_name = "U4", modifier = "residenceUrban", exposure = "I(o_htn_blood_related >= 1)TRUE", name = "U4_residenceUrban"),
  list(model_name = "U2", modifier = "sexMale", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "U2_sexMale"),
  list(model_name = "U3", modifier = "age_category40-64", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "U3_age_category40-64"),
  list(model_name = "U3", modifier = "age_category65 plus", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "U3_age_category65 plus"),
  list(model_name = "U4", modifier = "residenceUrban", exposure = "I(o_htn_not_blood_related >= 1)TRUE", name = "U4_residenceUrban")
)

# Fit, tidy, and save models and contrasts
all_results <- lapply(models, function(model) {
  fit_tidy_and_contrast_model(model$formula, model$design, model$name, contrast_list)
})

all_coefs <- bind_rows(lapply(all_results, function(result) result$tidy_model))
all_contrasts <- bind_rows(lapply(all_results, function(result) result$contrasts))

# Save all coefficients into a CSV
all_coefs %>%
  mutate(Coef_CI = paste0(round(exp(estimate),2)," \t(",
                          round(exp(estimate-1.96*std.error),2),", ",
                          round(exp(estimate+1.96*std.error),2),")"),
         PR = exp(estimate),
         lci = exp(estimate-1.96*std.error),
         uci = exp(estimate+1.96*std.error)
  ) %>%
  write_csv("analysis/nfaan05_poisson regression_of familial aggregation.csv")

# Save all contrast estimates into a CSV

write_csv(all_contrasts, "analysis/nfaan05_contrasts of poisson regression of familial aggregation.csv")

