# Unweighted Logistic Regression:
p0a <- glm(htn_diagnosed ~ I(o_diagnosedhtn>=1) + age + sex,
           data = hypertension_all_adults_analytic_sample,
           family = binomial())

# Unweighted Poisson Regression with Robust Standard Errors:
p0b <- glm(htn_diagnosed ~ I(o_diagnosedhtn>=1) + age + sex,
           data = hypertension_all_adults_analytic_sample,
           family = poisson())
p0b_robust <- coeftest(p0b, vcov = sandwich(p0b))

hypertension_all_adults_analytic_svy <- hypertension_all_adults_analytic_sample %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

# Assuming 'hypertension_all_adults_analytic_svy' is the survey design object for the data:
p0c <- svyglm(htn_diagnosed ~ I(o_diagnosedhtn>=1) + age + sex,
              design = all_adults_analytic_svy,
              family = quasipoisson())

# Generalized Linear Mixed Model Without Weights:
p0d <- glmer(htn_diagnosed ~ I(o_diagnosedhtn>=1) + age + sex + (1 | cluster_hhid),
             data = hypertension_all_adults_analytic_sample,
             family = poisson())

# Generalized Estimating Equation:
p0e <- geeglm(htn_diagnosed ~ I(o_diagnosedhtn>=1) + age + sex,
              data = hypertension_all_adults_analytic_sample,
              family = poisson(),
              weights = sampleweight,
              id = cluster_hhid,
              corstr = "exchangeable")

# Create summary table:
p0_model_summary <- bind_rows(
  broom::tidy(p0a) %>% mutate(model = "A"),
  p0b_robust[,] %>% data.frame() %>% mutate(model = "B") %>% 
    rename(estimate = Estimate,
           std.error = 'Std..Error',
           statistic  = 'z.value',
           p.value = 'Pr...z..'
    ),
  broom::tidy(p0c) %>% mutate(model = "C"),
  broom.mixed::tidy(p0d) %>% mutate(model = "D"),
  broom::tidy(p0e) %>% mutate(model = "E")
  
) %>% 
  mutate(exp_estimate = exp(estimate),
         exp_lci = exp(estimate - 1.96*std.error),
         exp_uci = exp(estimate + 1.96*std.error))
