# Unweighted Logistic Regression:
t0a <- glm(htn_disease ~ I(o_htn_not_blood_related >= 1) + age + sex,
           data = all_adults_analytic_sample,
           family = binomial())

# Unweighted Poisson Regression with Robust Standard Errors:
t0b <- glm(htn_disease ~ I(o_htn_not_blood_related >= 1) + age + sex,
           data = all_adults_analytic_sample,
           family = poisson())
t0b_robust <- coeftest(t0b, vcov = sandwich(t0b))

# Assuming 'all_adults_analytic_svy' is the survey design object for the data:
t0c <- svyglm(htn_disease ~ I(o_htn_not_blood_related >= 1) + age + sex,
              design = all_adults_analytic_svy,
              family = quasipoisson())

# Generalized Linear Mixed Model Without Weights:
t0d <- glmer(htn_disease ~ I(o_htn_not_blood_related >= 1) + age + sex + (1|cluster/hhid),
             data = all_adults_analytic_sample,
             family = poisson())

# Generalized Estimating Equation:
t0e <- geeglm(htn_disease ~ I(o_htn_not_blood_related >= 1) + age + sex,
              data = all_adults_analytic_sample,
              family = poisson(),
              weights = sampleweight,
              id = cluster_hhid,
              corstr = "exchangeable")

# Create summary table:
t0_model_summary <- bind_rows(
  broom::tidy(t0a) %>% mutate(model = "A"),
  t0b_robust[,] %>% data.frame() %>% mutate(model = "B") %>% 
    rename(estimate = Estimate,
           std.error = 'Std..Error',
           statistic  = 'z.value',
           p.value = 'Pr...z..'
    ),
  broom::tidy(t0c) %>% mutate(model = "C"),
  broom.mixed::tidy(t0d) %>% mutate(model = "D"),
  broom::tidy(t0e) %>% mutate(model = "E")
  
) %>% 
  mutate(exp_estimate = exp(estimate),
         exp_lci = exp(estimate - 1.96*std.error),
         exp_uci = exp(estimate + 1.96*std.error))
