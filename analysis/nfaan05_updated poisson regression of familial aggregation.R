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

source("functions/fit_tidy_and_contrast_model.R")

# Define models and contrasts
models <- list(
  list(formula = m0, design = all_adults_analytic_svy, model_name = "M0",design_name = "all_adults_analytic_svy"),
  list(formula = m1, design = all_adults_analytic_svy, model_name = "M1",design_name = "all_adults_analytic_svy"),
  list(formula = m2, design = all_adults_analytic_svy, model_name = "M2",design_name = "all_adults_analytic_svy"),
  list(formula = m3, design = all_adults_analytic_svy, model_name = "M3",design_name = "all_adults_analytic_svy"),
  list(formula = m4, design = all_adults_analytic_svy, model_name = "M4",design_name = "all_adults_analytic_svy"),
  list(formula = s0, design = all_adults_analytic_svy, model_name = "S0",design_name = "all_adults_analytic_svy"),
  list(formula = s1, design = all_adults_analytic_svy, model_name = "S1",design_name = "all_adults_analytic_svy"),
  list(formula = s2, design = all_adults_analytic_svy, model_name = "S2",design_name = "all_adults_analytic_svy"),
  list(formula = s3, design = all_adults_analytic_svy, model_name = "S3",design_name = "all_adults_analytic_svy"),
  list(formula = s4, design = all_adults_analytic_svy, model_name = "S4",design_name = "all_adults_analytic_svy"),
  list(formula = t0, design = all_adults_analytic_svy, model_name = "T0",design_name = "all_adults_analytic_svy"),
  list(formula = t1, design = all_adults_analytic_svy, model_name = "T1",design_name = "all_adults_analytic_svy"),
  list(formula = t2, design = all_adults_analytic_svy, model_name = "T2",design_name = "all_adults_analytic_svy"),
  list(formula = t3, design = all_adults_analytic_svy, model_name = "T3",design_name = "all_adults_analytic_svy"),
  list(formula = t4, design = all_adults_analytic_svy, model_name = "T4",design_name = "all_adults_analytic_svy"),
  list(formula = u0, design = all_adults_analytic_svy, model_name = "U0",design_name = "all_adults_analytic_svy"),
  list(formula = u1, design = all_adults_analytic_svy, model_name = "U1",design_name = "all_adults_analytic_svy"),
  list(formula = u2, design = all_adults_analytic_svy, model_name = "U2",design_name = "all_adults_analytic_svy"),
  list(formula = u3, design = all_adults_analytic_svy, model_name = "U3",design_name = "all_adults_analytic_svy"),
  list(formula = u4, design = all_adults_analytic_svy, model_name = "U4",design_name = "all_adults_analytic_svy"),
  list(formula = p0, design = hypertension_all_adults_analytic_svy, model_name = "P0",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = p1, design = hypertension_all_adults_analytic_svy, model_name = "P1",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = p2, design = hypertension_all_adults_analytic_svy, model_name = "P2",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = p3, design = hypertension_all_adults_analytic_svy, model_name = "P3",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = p4, design = hypertension_all_adults_analytic_svy, model_name = "P4",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = q0, design = nondisease_all_adults_analytic_svy, model_name = "Q0",design_name = "nondisease_all_adults_analytic_svy"),
  list(formula = q1, design = nondisease_all_adults_analytic_svy, model_name = "Q1",design_name = "nondisease_all_adults_analytic_svy"),
  list(formula = q2, design = nondisease_all_adults_analytic_svy, model_name = "Q2",design_name = "nondisease_all_adults_analytic_svy"),
  list(formula = q3, design = nondisease_all_adults_analytic_svy, model_name = "Q3",design_name = "nondisease_all_adults_analytic_svy"),
  list(formula = q4, design = nondisease_all_adults_analytic_svy, model_name = "Q4",design_name = "nondisease_all_adults_analytic_svy"),
  list(formula = n0, design = nondisease_all_adults_analytic_svy, model_name = "N0",design_name = "nondisease_all_adults_analytic_svy"),
  list(formula = n1, design = nondisease_all_adults_analytic_svy, model_name = "N1",design_name = "nondisease_all_adults_analytic_svy"),
  list(formula = n2, design = nondisease_all_adults_analytic_svy, model_name = "N2",design_name = "nondisease_all_adults_analytic_svy"),
  list(formula = n3, design = nondisease_all_adults_analytic_svy, model_name = "N3",design_name = "nondisease_all_adults_analytic_svy"),
  list(formula = n4, design = nondisease_all_adults_analytic_svy, model_name = "N4",design_name = "nondisease_all_adults_analytic_svy")
)

contrast_list <- list(
  list(model_name = "M2", modifier = "sexMale", exposure = "I(o_htn >= 1)TRUE", contrast_name = "M2_sexMale"),
  list(model_name = "M3", modifier = "age_category40-64", exposure = "I(o_htn >= 1)TRUE", contrast_name = "M3_age_category40-64"),
  list(model_name = "M3", modifier = "age_category65 plus", exposure = "I(o_htn >= 1)TRUE", contrast_name = "M3_age_category65 plus"),
  list(model_name = "M4", modifier = "residenceUrban", exposure = "I(o_htn >= 1)TRUE", contrast_name = "M4_residenceUrban"),
  list(model_name = "S2", modifier = "sexMale", exposure = "I(o_htn_blood_related >= 1)TRUE", contrast_name = "S2_sexMale"),
  list(model_name = "S3", modifier = "age_category40-64", exposure = "I(o_htn_blood_related >= 1)TRUE", contrast_name = "S3_age_category40-64"),
  list(model_name = "S3", modifier = "age_category65 plus", exposure = "I(o_htn_blood_related >= 1)TRUE", contrast_name = "S3_age_category65 plus"),
  list(model_name = "S4", modifier = "residenceUrban", exposure = "I(o_htn_blood_related >= 1)TRUE", contrast_name = "S4_residenceUrban"),
  list(model_name = "T2", modifier = "sexMale", exposure = "I(o_htn_not_blood_related >= 1)TRUE", contrast_name = "T2_sexMale"),
  list(model_name = "T3", modifier = "age_category40-64", exposure = "I(o_htn_not_blood_related >= 1)TRUE", contrast_name = "T3_age_category40-64"),
  list(model_name = "T3", modifier = "age_category65 plus", exposure = "I(o_htn_not_blood_related >= 1)TRUE", contrast_name = "T3_age_category65 plus"),
  list(model_name = "T4", modifier = "residenceUrban", exposure = "I(o_htn_not_blood_related >= 1)TRUE", contrast_name = "T4_residenceUrban"),
  list(model_name = "P2", modifier = "sexMale", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "P2_sexMale"),
  list(model_name = "P3", modifier = "age_category40-64", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "P3_age_category40-64"),
  list(model_name = "P3", modifier = "age_category65 plus", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "P3_age_category65 plus"),
  list(model_name = "P4", modifier = "residenceUrban", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "P4_residenceUrban"),
  list(model_name = "Q2", modifier = "sexMale", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "Q2_sexMale"),
  list(model_name = "Q3", modifier = "age_category40-64", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "Q3_age_category40-64"),
  list(model_name = "Q3", modifier = "age_category65 plus", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "Q3_age_category65 plus"),
  list(model_name = "Q4", modifier = "residenceUrban", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "Q4_residenceUrban"),
  list(model_name = "N2", modifier = "sexMale", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "N2_sexMale"),
  list(model_name = "N3", modifier = "age_category40-64", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "N3_age_category40-64"),
  list(model_name = "N3", modifier = "age_category65 plus", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "N3_age_category65 plus"),
  list(model_name = "N4", modifier = "residenceUrban", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "N4_residenceUrban"),
  list(model_name = "U2", modifier = "sexMale", exposure = "I(o_htn_blood_related >= 1)TRUE", contrast_name = "U2_sexMale"),
  list(model_name = "U3", modifier = "age_category40-64", exposure = "I(o_htn_blood_related >= 1)TRUE", contrast_name = "U3_age_category40-64"),
  list(model_name = "U3", modifier = "age_category65 plus", exposure = "I(o_htn_blood_related >= 1)TRUE", contrast_name = "U3_age_category65 plus"),
  list(model_name = "U4", modifier = "residenceUrban", exposure = "I(o_htn_blood_related >= 1)TRUE", contrast_name = "U4_residenceUrban"),
  list(model_name = "U2", modifier = "sexMale", exposure = "I(o_htn_not_blood_related >= 1)TRUE", contrast_name = "U2_sexMale"),
  list(model_name = "U3", modifier = "age_category40-64", exposure = "I(o_htn_not_blood_related >= 1)TRUE", contrast_name = "U3_age_category40-64"),
  list(model_name = "U3", modifier = "age_category65 plus", exposure = "I(o_htn_not_blood_related >= 1)TRUE", contrast_name = "U3_age_category65 plus"),
  list(model_name = "U4", modifier = "residenceUrban", exposure = "I(o_htn_not_blood_related >= 1)TRUE", contrast_name = "U4_residenceUrban")
) %>% 
  bind_rows()

# Fit, tidy, and save models and contrasts
all_results <- lapply(models, function(model) {
  print(paste0(model$design_name,": ",model$model_name))
  fit_tidy_and_contrast_model(formula = model$formula, design = model$design, current_model_name = model$model_name,
                              contrast_list = contrast_list,design_name = model$design_name)
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
  write_csv("analysis/nfaan05_poisson regression of familial aggregation.csv")

# Save all contrast estimates into a CSV

write_csv(all_contrasts, "analysis/nfaan05_contrasts of poisson regression of familial aggregation.csv")

