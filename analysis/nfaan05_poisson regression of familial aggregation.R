rm(list=ls()); gc(); source(".Rprofile")

source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")
source("analysis/nfaan_poisson regression equations.R")


model_m0 = svyglm(formula = as.formula(m0),design = all_adults_analytic_svy,
                  family = quasipoisson())
model_m1 = svyglm(formula = as.formula(m1),design = all_adults_analytic_svy,
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


all_coefs = bind_rows(
  broom::tidy(model_m0) %>% mutate(model = "M0"),
  broom::tidy(model_m1) %>% mutate(model = "M1"),
  broom::tidy(model_s0) %>% mutate(model = "S0"),
  broom::tidy(model_s1) %>% mutate(model = "S1"),
  broom::tidy(model_t0) %>% mutate(model = "T0"),
  broom::tidy(model_t1) %>% mutate(model = "T1"),
  broom::tidy(model_u0) %>% mutate(model = "U0"),
  broom::tidy(model_u1) %>% mutate(model = "U1"),
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



