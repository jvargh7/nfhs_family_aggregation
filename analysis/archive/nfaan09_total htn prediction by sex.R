# Unweighted Logistic Regression:
r0a <- glm(htn_disease ~ I(o_htn >= 1) * sex + age * sex,
           data = all_adults_analytic_sample,
           family = binomial())

# Unweighted Poisson Regression with Robust Standard Errors:
r0b <- glm(htn_disease ~ I(o_htn >= 1) * sex + age * sex,
           data = all_adults_analytic_sample,
           family = poisson())
r0b_robust <- coeftest(r0b, vcov = sandwich(r0b))

# Assuming 'all_adults_analytic_svy' is the survey design object for the data:
r0c <- svyglm(htn_disease ~ I(o_htn >= 1) * sex + age * sex,
              design = all_adults_analytic_svy,
              family = quasipoisson())

# Generalized Linear Mixed Model Without Weights:
r0d <- glmer(htn_disease ~ I(o_htn >= 1) * sex + age * sex + (1 | cluster_hhid),
             data = all_adults_analytic_sample,
             family = poisson())

# Generalized Estimating Equation:
r0e <- geeglm(htn_disease ~ I(o_htn >= 1) * sex + age * sex,
              data = all_adults_analytic_sample,
              family = poisson(),
              weights = sampleweight,
              id = cluster_hhid,
              corstr = "exchangeable")

# Create summary table:
r0_model_summary <- bind_rows(
  broom::tidy(r0a) %>% mutate(model = "A"),
  r0b_robust[,] %>% data.frame() %>% mutate(model = "B") %>% 
    rename(estimate = Estimate,
           std.error = 'Std..Error',
           statistic  = 'z.value',
           p.value = 'Pr...z..'
    ),
  broom::tidy(r0c) %>% mutate(model = "C"),
  broom.mixed::tidy(r0d) %>% mutate(model = "D"),
  broom::tidy(r0e) %>% mutate(model = "E")
  
) %>% 
  mutate(exp_estimate = exp(estimate),
         exp_lci = exp(estimate - 1.96*std.error),
         exp_uci = exp(estimate + 1.96*std.error))
