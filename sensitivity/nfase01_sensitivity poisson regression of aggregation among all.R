rm(list=ls()); gc(); source(".Rprofile")

source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")

## @KRISHNA - Please fit for S*, T*, U*, P*, Q*, N* for the single main exposure -------------
## IGNORE V*

source("analysis/nfaan_poisson regression equations.R")
source("functions/fit_tidy_and_contrast_model.R")
source("C:/code/external/functions/survey/contrasts_svyglm.R")

# Define models and contrasts
models <- list(
  list(formula = r0, design = all_adults_analytic_svy, model_name = "R0",design_name = "all_adults_analytic_svy"),
  list(formula = r1, design = all_adults_analytic_svy, model_name = "R1",design_name = "all_adults_analytic_svy"),
  list(formula = w0, design = all_adults_analytic_svy, model_name = "W0",design_name = "all_adults_analytic_svy"),
  list(formula = w1, design = all_adults_analytic_svy, model_name = "W1",design_name = "all_adults_analytic_svy"),
  list(formula = r0, design = hypertension_all_adults_analytic_svy, model_name = "R0",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = r1, design = hypertension_all_adults_analytic_svy, model_name = "R1",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = w0, design = hypertension_all_adults_analytic_svy, model_name = "W0",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = w1, design = hypertension_all_adults_analytic_svy, model_name = "W1",design_name = "hypertension_all_adults_analytic_svy"),
  
  list(formula = r2, design = all_adults_analytic_svy, model_name = "R2",design_name = "all_adults_analytic_svy"),
  list(formula = r3, design = all_adults_analytic_svy, model_name = "R3",design_name = "all_adults_analytic_svy"),
  list(formula = r4, design = all_adults_analytic_svy, model_name = "R4",design_name = "all_adults_analytic_svy"),
  list(formula = w2, design = all_adults_analytic_svy, model_name = "W2",design_name = "all_adults_analytic_svy"),
  list(formula = w3, design = all_adults_analytic_svy, model_name = "W3",design_name = "all_adults_analytic_svy"),
  list(formula = w4, design = all_adults_analytic_svy, model_name = "W4",design_name = "all_adults_analytic_svy"),
  
  list(formula = r2, design = hypertension_all_adults_analytic_svy, model_name = "R2",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = r3, design = hypertension_all_adults_analytic_svy, model_name = "R3",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = r4, design = hypertension_all_adults_analytic_svy, model_name = "R4",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = w2, design = hypertension_all_adults_analytic_svy, model_name = "W2",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = w3, design = hypertension_all_adults_analytic_svy, model_name = "W3",design_name = "hypertension_all_adults_analytic_svy"),
  list(formula = w4, design = hypertension_all_adults_analytic_svy, model_name = "W4",design_name = "hypertension_all_adults_analytic_svy")
  

  
)

contrast_list = list(
  list(model_name = "R2", modifier = "sexMale", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "R2_sexMale"),
  list(model_name = "R3", modifier = "age_category40-64", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "R3_age_category40-64"),
  list(model_name = "R3", modifier = "age_category65 plus", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "R3_age_category65 plus"),
  list(model_name = "R4", modifier = "residenceUrban", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "R4_residenceUrban"),
  list(model_name = "R2", modifier = "sexMale", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "R2_sexMale"),
  list(model_name = "R3", modifier = "age_category40-64", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "R3_age_category40-64"),
  list(model_name = "R3", modifier = "age_category65 plus", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "R3_age_category65 plus"),
  list(model_name = "R4", modifier = "residenceUrban", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "R4_residenceUrban"),
  
  list(model_name = "W2", modifier = "sexMale", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "W2_sexMale"),
  list(model_name = "W3", modifier = "age_category40-64", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "W3_age_category40-64"),
  list(model_name = "W3", modifier = "age_category65 plus", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "W3_age_category65 plus"),
  list(model_name = "W4", modifier = "residenceUrban", exposure = "I(o_diagnosedhtn >= 1)TRUE", contrast_name = "W4_residenceUrban"),
  list(model_name = "W2", modifier = "sexMale", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "W2_sexMale"),
  list(model_name = "W3", modifier = "age_category40-64", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "W3_age_category40-64"),
  list(model_name = "W3", modifier = "age_category65 plus", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "W3_age_category65 plus"),
  list(model_name = "W4", modifier = "residenceUrban", exposure = "I(o_undiagnosedhtn >= 1)TRUE", contrast_name = "W4_residenceUrban")
  
) %>% 
  bind_rows()


# fit_tidy_and_contrast_model(formula = models[[9]]$formula, design = models[[9]]$design, current_model_name = models[[9]]$model_name,
#                             contrast_list = contrast_list,design_name = models[[9]]$design_name)
# 


# Fit, tidy, and save models and contrasts
all_results <- map(models, function(model) {
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
  write_csv("sensitivity/nfase01_poisson regression of familial aggregation.csv")

write_csv(all_contrasts, "sensitivity/nfase01_contrasts of poisson regression of familial aggregation.csv")
