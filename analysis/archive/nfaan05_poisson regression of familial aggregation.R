rm(list=ls()); gc(); source(".Rprofile")

source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")

## @KRISHNA - Please fit for S*, T*, U*, P*, Q*, N* for the single main exposure -------------
## IGNORE V*

source("analysis/nfaan_poisson regression equations.R")


model_m0 = svyglm(formula = as.formula(m0),design = all_adults_analytic_svy,
                  family = quasipoisson())
model_m1 = svyglm(formula = as.formula(m1),design = all_adults_analytic_svy,
                  family = quasipoisson())
model_m2 = svyglm(formula = as.formula(m2),design = all_adults_analytic_svy,
                  family = quasipoisson())
model_m3 = svyglm(formula = as.formula(m3),design = all_adults_analytic_svy,
                  family = quasipoisson())
model_m4 = svyglm(formula = as.formula(m4),design = all_adults_analytic_svy,
                  family = quasipoisson())


model_s0 = svyglm(formula = as.formula(s0),design = all_adults_analytic_svy,
                  family = quasipoisson())
model_s1 = svyglm(formula = as.formula(s1),design = all_adults_analytic_svy,
                  family = quasipoisson())

model_t0 = svyglm(formula = as.formula(t0),design = all_adults_analytic_svy,
                  family = quasipoisson())
model_t1 = svyglm(formula = as.formula(t1),design = all_adults_analytic_svy,
                  family = quasipoisson())

model_u0 = svyglm(formula = as.formula(u0),design = all_adults_analytic_svy,
                  family = quasipoisson())
model_u1 = svyglm(formula = as.formula(u1),design = all_adults_analytic_svy,
                  family = quasipoisson())

model_v0 = svyglm(formula = as.formula(v0),design = all_adults_analytic_svy,
                  family = quasipoisson())
model_v1 = svyglm(formula = as.formula(v1),design = all_adults_analytic_svy,
                  family = quasipoisson())


model_p0 = svyglm(formula = as.formula(p0),design = hypertension_all_adults_analytic_svy,
                  family = quasipoisson())
model_p1 = svyglm(formula = as.formula(p1),design = hypertension_all_adults_analytic_svy,
                  family = quasipoisson())

model_q0 = svyglm(formula = as.formula(q0),design = nondisease_all_adults_analytic_svy,
                  family = quasipoisson())
model_q1 = svyglm(formula = as.formula(q1),design = nondisease_all_adults_analytic_svy,
                  family = quasipoisson())

model_n0 = svyglm(formula = as.formula(n0),design = nondisease_all_adults_analytic_svy,
                  family = quasipoisson())
model_n1 = svyglm(formula = as.formula(n1),design = nondisease_all_adults_analytic_svy,
                  family = quasipoisson())


## @KRISHNA - Please complete for S to N - sAVE ALL ESTIMATES INTO A CSV -------------

all_coefs = bind_rows(
  broom::tidy(model_m0) %>% mutate(model = "M0"),
  broom::tidy(model_m1) %>% mutate(model = "M1"),
  broom::tidy(model_m2) %>% mutate(model = "M2"),
  broom::tidy(model_m3) %>% mutate(model = "M3"),
  broom::tidy(model_m4) %>% mutate(model = "M4"),
  
  broom::tidy(model_s0) %>% mutate(model = "S0"),
  broom::tidy(model_s1) %>% mutate(model = "S1"),
  
  broom::tidy(model_t0) %>% mutate(model = "T0"),
  broom::tidy(model_t1) %>% mutate(model = "T1"),
  
  broom::tidy(model_u0) %>% mutate(model = "U0"),
  broom::tidy(model_u1) %>% mutate(model = "U1"),
  
  broom::tidy(model_v0) %>% mutate(model = "V0"),
  broom::tidy(model_v1) %>% mutate(model = "V1"),
  
  broom::tidy(model_p0) %>% mutate(model = "P0"),
  broom::tidy(model_p1) %>% mutate(model = "P1"),
  
  broom::tidy(model_q0) %>% mutate(model = "Q0"),
  broom::tidy(model_q1) %>% mutate(model = "Q1"),
  
  broom::tidy(model_n0) %>% mutate(model = "N0"),
  broom::tidy(model_n1) %>% mutate(model = "N1")) 
  
all_coefs %>% 
  mutate(Coef_CI = paste0(round(exp(estimate),2)," \t(",
                          round(exp(estimate-1.96*std.error),2),", ",
                          round(exp(estimate+1.96*std.error),2),")"),
         PR = exp(estimate),
         lci = exp(estimate-1.96*std.error),
         uci = exp(estimate+1.96*std.error)
  ) %>% 
  write_csv(.,"analysis/nfaan05_poisson regression of familial aggregation.csv")


# Contrasts for interaction terms ---------
## @KRISHNA: Please complete for S*, T*, P*, Q*, N*
## Please complete for U* for both exposures, i.e. I(o_htn_blood_related >= 1) and I(o_htn_not_blood_related >= 1) separately

# You can download these from 
if(Sys.info()["user"] =="JVARGH7"){
  source("C:/code/external/functions/survey/contrasts_svyglm.R")
} else{
  print("Please download contrasts_svyglm.R, prepare_contrasts.R and round_d.R from https://github.com/jvargh7/functions/preprocessing")
  print("After you have downloaded them, please load them using source()")
  
}

contrasts_m2_out = contrasts_svyglm(svymodel = model_m2,modifier = "sexMale",exposure = "I(o_htn >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))
contrasts_m3_out_2 = contrasts_svyglm(svymodel = model_m3,modifier = "age_category40-64",exposure = "I(o_htn >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))
contrasts_m3_out_3 = contrasts_svyglm(svymodel = model_m3,modifier = "age_category65 plus",exposure = "I(o_htn >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))
contrasts_m4_out = contrasts_svyglm(svymodel = model_m4,modifier = "residenceUrban",exposure = "I(o_htn >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))


contrasts_u2_out_a = contrasts_svyglm(svymodel = model_m2,modifier = "sexMale",exposure = "I(o_htn_blood_related >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))
contrasts_u3_out_2_a = contrasts_svyglm(svymodel = model_m3,modifier = "age_category40-64",exposure = "I(o_htn_blood_related >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))
contrasts_u3_out_3_a = contrasts_svyglm(svymodel = model_m3,modifier = "age_category65 plus",exposure = "I(o_htn_blood_related >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))
contrasts_u4_out_a = contrasts_svyglm(svymodel = model_m4,modifier = "residenceUrban",exposure = "I(o_htn_blood_related >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))

contrasts_u2_out_b = contrasts_svyglm(svymodel = model_m2,modifier = "sexMale",exposure = "I(o_htn_not_blood_related >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))
contrasts_u3_out_2_b = contrasts_svyglm(svymodel = model_m3,modifier = "age_category40-64",exposure = "I(o_htn_not_blood_related >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))
contrasts_u3_out_3_b = contrasts_svyglm(svymodel = model_m3,modifier = "age_category65 plus",exposure = "I(o_htn_not_blood_related >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))
contrasts_u4_out_b = contrasts_svyglm(svymodel = model_m4,modifier = "residenceUrban",exposure = "I(o_htn_not_blood_related >= 1)TRUE") %>% mutate(contrast = paste0("Contrast ",1:n()))



# Save contrast estimates into a CSV --------

bind_rows(contrasts_m2_out %>% mutate(model = "M2", modifier = "sexMale",exposure = "I(o_htn >= 1)TRUE"),
          contrasts_m3_out_2 %>% mutate(model = "M3", modifier = "age_category40-64",exposure = "I(o_htn >= 1)TRUE"),
          contrasts_m3_out_3 %>% mutate(model = "M3", modifier = "age_category65 plus",exposure = "I(o_htn >= 1)TRUE"),
          contrasts_m2_out %>% mutate(model = "M4", modifier = "residenceUrban",exposure = "I(o_htn >= 1)TRUE"),
          
          # PLEASE COMPLETE FOR OTHERS
          ) %>% 
  write_csv(.,"analysis/nfaan05_contrasts of poisson regression of familial aggregation.csv")
