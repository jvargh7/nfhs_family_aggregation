# Unweighted Logistic Regression: 
n0a <- glm(htn_disease ~ I(o_diagnosedhtn>=1) + age + sex,
           data = undiagnosed_all_adults_analytic_sample,
           family = binomial())

# Unweighted Poisson Regression with Robust Standard Errors:
n0b <- glm(htn_disease ~ I(o_diagnosedhtn>=1) + age + sex,
           data = undiagnosed_all_adults_analytic_sample,
           family = poisson())
n0b_robust <- coeftest(n0b, vcov = sandwich)

undiagnosed_all_adults_analytic_svy <- undiagnosed_all_adults_analytic_sample %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

# Survey Weighted Poisson Regression:
n0c <- svyglm(htn_disease ~ I(o_diagnosedhtn>=1) + poly(age, 2) + sex,
              design = undiagnosed_all_adults_analytic_svy,
              family = quasipoisson())

# Generalized Linear Mixed Model Without Weights:
n0d <- glmer(htn_disease ~ I(o_diagnosedhtn>=1) + age + sex + swealthq_ur + (1 | cluster_hhid),
             data = undiagnosed_all_adults_analytic_sample,
             family = poisson())

# Generalized Estimating Equation: 
n0e <- geeglm(htn_disease ~ I(o_diagnosedhtn>=1) + age + sex,
              data = undiagnosed_all_adults_analytic_sample,
              family = poisson(),
              weights = sampleweight,
              id = cluster_hhid,
              corstr = "exchangeable")

n0_model_summary <- bind_rows(
  broom::tidy(n0a) %>% mutate(model = "A"),
  n0b_robust[,] %>% data.frame() %>% mutate(model = "B") %>% 
    rename(estimate = Estimate,
           std.error = 'Std..Error',
           statistic  = 'z.value',
           p.value = 'Pr...z..'
    ),
  broom::tidy(n0c) %>% mutate(model = "C"),
  broom.mixed::tidy(n0d) %>% mutate(model = "D"),
  broom::tidy(n0e) %>% mutate(model = "E")
  
) %>% 
  mutate(exp_estimate = exp(estimate),
         exp_lci = exp(estimate - 1.96*std.error),
         exp_uci = exp(estimate + 1.96*std.error))
