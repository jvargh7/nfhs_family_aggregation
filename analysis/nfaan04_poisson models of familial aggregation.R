# R uses the working directory as a reference
source("preprocessing/nfapre02_nfhs5 all adults svydesign.R")

library(geepack)
with(all_adults_analytic_sample,table(htn_disease,o_htn))

t = Sys.time()
m0 <- geeglm(htn_disease ~ I(o_htn>=1) + age + sex,data=all_adults_analytic_sample,
       family = poisson(),
       weights = sampleweight,
       id = cluster_hhid,corstr="exchangeable")
m1 <- geeglm(htn_disease ~ I(o_htn>=1)*I(swealthq_ur>=4) + age + sex,data=all_adults_analytic_sample,
       family = poisson(),
       weights = sampleweight,
       id = cluster_hhid,corstr="exchangeable")
t_f = Sys.time()

t_f - t

m0 %>% 
  broom::tidy(exponentiate = TRUE)
m1 %>% 
  broom::tidy(exponentiate = TRUE)

t = Sys.time()
undiagnosed_all_adults_analytic_sample <- all_adults_analytic_sample %>% 
  dplyr::filter(htn_diagnosed == 0) 

with(undiagnosed_all_adults_analytic_sample,table(htn_disease,o_diagnosedhtn))

n0 <- geeglm(htn_disease ~ I(o_diagnosedhtn>=1) + age + sex,data=undiagnosed_all_adults_analytic_sample,
             family = poisson(),
             weights = sampleweight,
             id = cluster_hhid,corstr="exchangeable")

n1 <- geeglm(htn_disease ~ I(o_diagnosedhtn>=1)*I(swealthq_ur>=4) + age + sex,data=undiagnosed_all_adults_analytic_sample,
             family = poisson(),
             weights = sampleweight,
             id = cluster_hhid,corstr="exchangeable")
t_f = Sys.time()

n0 %>% 
  broom::tidy(exponentiate = TRUE)
n1 %>% 
  broom::tidy(exponentiate = TRUE)


t = Sys.time()
hypertension_all_adults_analytic_sample <- all_adults_analytic_sample %>% 
  dplyr::filter(htn_disease == 1) 

with(hypertension_all_adults_analytic_sample,table(htn_diagnosed,o_diagnosedhtn))

p0 <- geeglm(htn_diagnosed ~ I(o_diagnosedhtn>=1) + age + sex,data=hypertension_all_adults_analytic_sample,
             family = poisson(),
             weights = sampleweight,
             id = cluster_hhid,corstr="exchangeable")

p1 <- geeglm(htn_diagnosed ~ I(o_diagnosedhtn>=1)*I(swealthq_ur>=4) + age + sex,data=hypertension_all_adults_analytic_sample,
             family = poisson(),
             weights = sampleweight,
             id = cluster_hhid,corstr="exchangeable")
t_f = Sys.time()

p0 %>% 
  broom::tidy(exponentiate = TRUE)
p1 %>% 
  broom::tidy(exponentiate = TRUE)
