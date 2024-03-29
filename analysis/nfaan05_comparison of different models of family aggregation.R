# R uses the working directory as a reference
source("preprocessing/nfapre02_nfhs5 all adults svydesign.R")

# Unweighted Logistic Regression

A <- glm(htn_disease ~ I(o_htn>=1) + age + sex,data=all_adults_analytic_sample,
             family = binomial())

# Unweighted Poisson Regression with Robust Standard Errors
B <- glm(htn_disease ~ I(o_htn>=1) + age + sex,data=all_adults_analytic_sample,
         family = poisson())

# https://stats.stackexchange.com/questions/520662/how-to-add-robust-error-variances-in-glm-poisson-model-in-r
library("sandwich")
library("lmtest")
B_robust <- coeftest(B, vcov = sandwich)

# Survey weighted Poisson Regression
# Q: Does it use weights?
# Q: How is it different from Model B
# Q: Does it use Robust Standard Errors?
C <- svyglm(htn_disease ~ I(o_htn>=1) + age + sex,data=all_adults_analytic_svy,
         family = poisson())

# Generalized Linear Mixed Model without weights
library(lme4)
D <- glmer(htn_disease ~ I(o_htn>=1) + age + sex + (1|cluster/hhid),data=all_adults_analytic_sample,family = poisson())


# Generalized Estimating Equation
library(geepack)
E <- geeglm(htn_disease ~ I(o_htn>=1) + age + sex,data=all_adults_analytic_sample,
            family = poisson(),
            weights = sampleweight,
            id = cluster_hhid,corstr="exchangeable")


# Summary of models
model_summary <- bind_rows(
  broom::tidy(A) %>% mutate(model = "A"),
  B_robust[,] %>% data.frame() %>% mutate(model = "B") %>% 
    rename(estimate = Estimate,
           std.error = 'Std..Error',
           statistic  = 'z.value',
           p.value = 'Pr...z..'
           ),
  broom::tidy(C) %>% mutate(model = "C"),
  broom.mixed::tidy(D) %>% mutate(model = "D"),
  broom::tidy(E) %>% mutate(model = "D")
  
) %>% 
  mutate(exp_estimate = exp(estimate),
         exp_lci = exp(estimate - 1.96*std.error),
         exp_uci = exp(estimate + 1.96*std.error))