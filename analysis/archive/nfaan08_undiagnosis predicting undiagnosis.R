# Unweighted Logistic Regression (Need to figure this out):
q0a <- glm(htn_disease ~ I(o_undiagnosedhtn>=1) + age + sex,
           data = undiagnosed_all_adults_analytic_sample,
           family = binomial())

# Unweighted Poisson Regression with Robust Standard Errors:
q0b <- glm(htn_disease ~ I(o_undiagnosedhtn>=1) + age + sex,
           data = undiagnosed_all_adults_analytic_sample,
           family = poisson())
q0b_robust <- coeftest(q0b, vcov = sandwich(q0b))

undiagnosed_all_adults_analytic_svy <- undiagnosed_all_adults_analytic_sample %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

# Assuming 'undiagnosed_all_adults_analytic_svy' is the survey design object for the data:
q0c <- svyglm(htn_disease ~ I(o_undiagnosedhtn>=1) + age + sex,
              design = undiagnosed_all_adults_analytic_svy,
              family = quasipoisson())

# Generalized Linear Mixed Model Without Weights:
q0d <- glmer(htn_disease ~ I(o_undiagnosedhtn>=1) + age + sex + (1 | cluster_hhid),
             data = undiagnosed_all_adults_analytic_sample,
             family = poisson())

# Generalized Estimating Equation:
q0e <- geeglm(htn_disease ~ I(o_undiagnosedhtn>=1) + age + sex,
              data = undiagnosed_all_adults_analytic_sample,
              family = poisson(),
              weights = sampleweight,
              id = cluster_hhid,
              corstr = "exchangeable")

# Create summary table:
q0_model_summary <- bind_rows(
  broom::tidy(q0a) %>% mutate(model = "A"),
  q0b_robust[,] %>% data.frame() %>% mutate(model = "B") %>% 
    rename(estimate = Estimate,
           std.error = 'Std..Error',
           statistic  = 'z.value',
           p.value = 'Pr...z..'
    ),
  broom::tidy(q0c) %>% mutate(model = "C"),
  broom.mixed::tidy(q0d) %>% mutate(model = "D"),
  broom::tidy(q0e) %>% mutate(model = "E")
  
) %>% 
  mutate(exp_estimate = exp(estimate),
         exp_lci = exp(estimate - 1.96*std.error),
         exp_uci = exp(estimate + 1.96*std.error))
