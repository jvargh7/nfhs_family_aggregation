source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")
source("C:/code/external/functions/survey/svysummary.R") 


eligible_htn_by_hh <- eligible_analytic_sample %>% 
  dplyr::filter(htn_disease == 1) %>% 
  group_by(cluster,hhid) %>% 
  summarize(eligible_htn_male = sum(sex == "Male"),
            eligible_htn_female = sum(sex == "Female"),
            eligible_htn_total = sum(!is.na(sex))) %>% 
  ungroup()

hh_iapr_with_eligible <- hh_iapr %>% 
  left_join(eligible_htn_by_hh,
            by = c("cluster","hhid")) %>% 
  mutate(across(one_of(c("eligible_htn_male","eligible_htn_female","eligible_htn_total")),.fns=function(x) case_when(is.na(x) ~ 0,
                                                       TRUE ~ x))) %>% 
  # eligible_htn_total is probably getting reclassified as hypertension based on other self-reported question
  mutate(n_other_htn = case_when(n_htn < eligible_htn_total ~ 0,
                                 TRUE ~ n_htn - eligible_htn_total),
         n_other = (n_valid - n_eligible)) %>% 
  mutate(prop_other_htn = (n_other_htn)/n_other) %>% 
  dplyr::filter(n_valid > 1)

hh_iapr_with_eligible %>% 
  ggplot(data=.,aes(x = n_other,y=prop_other_htn,group=n_other)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Number of non-eligible household members") +
  ylab("Proportion") +
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,by=5))
