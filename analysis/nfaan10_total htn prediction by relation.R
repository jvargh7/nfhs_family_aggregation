# Unweighted Logistic Regression:
s0a <- glm(htn_disease ~ I(o_htn >= 1) * blood_relation + age * blood_relation,
           data = all_adults_analytic_sample,
           family = binomial())

# Unweighted Poisson Regression with Robust Standard Errors:
s0b <- glm(htn_disease ~ I(o_htn >= 1) * blood_relation + age * blood_relation,
           data = all_adults_analytic_sample,
           family = poisson())
s0b_robust <- coeftest(s0b, vcov = sandwich(s0b))

# Assuming 'all_adults_analytic_svy' is the survey design object for the data:
s0c <- svyglm(htn_disease ~ I(o_htn >= 1) * blood_relation + age * blood_relation,
              design = all_adults_analytic_svy,
              family = quasipoisson())

# Generalized Linear Mixed Model Without Weights:
s0d <- glmer(htn_disease ~ I(o_htn >= 1) * blood_relation + age * blood_relation + (1 | cluster_hhid),
             data = all_adults_analytic_sample,
             family = poisson())

# Generalized Estimating Equation:
s0e <- geeglm(htn_disease ~ I(o_htn >= 1) * blood_relation + age * blood_relation,
              data = all_adults_analytic_sample,
              family = poisson(),
              weights = sampleweight,
              id = cluster_hhid,
              corstr = "exchangeable")

# Create summary table:
s0_model_summary <- bind_rows(
  broom::tidy(s0a) %>% mutate(model = "A"),
  s0b_robust[,] %>% data.frame() %>% mutate(model = "B") %>% 
    rename(estimate = Estimate,
           std.error = 'Std..Error',
           statistic  = 'z.value',
           p.value = 'Pr...z..'
    ),
  broom::tidy(s0c) %>% mutate(model = "C"),
  broom.mixed::tidy(s0d) %>% mutate(model = "D"),
  broom::tidy(s0e) %>% mutate(model = "E")
  
) %>% 
  mutate(exp_estimate = exp(estimate),
         exp_lci = exp(estimate - 1.96*std.error),
         exp_uci = exp(estimate + 1.96*std.error))
