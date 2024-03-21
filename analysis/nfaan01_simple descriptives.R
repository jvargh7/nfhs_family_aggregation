

source("preprocessing/nfapre02_nfhs5 eligible svydesign.R")
source("C:/code/external/functions/survey/svysummary.R")

svysummary(eligible_analytic_svy,
           c_vars = "age"
           # p_vars = proportion_vars,
           # g_vars = grouped_vars,
           # id_vars = i_v
)

group_vars <- c("sex")

continuous_vars <- c("bmi","age","sbp","dbp")
proportion_vars <- c("highwc","htn","highbp","diaghtn",
                     "htn_screened","htn_disease","htn_diagnosed","htn_treated","htn_controlled")
grouped_vars <- c("age_category","age_category10","age_category5","education",
                  "caste","religion","swealthq_ur","bmi_category","bp_group")

id_vars = list(c(group_vars),
               c("residence",group_vars));

analytic_sample_summary <- map_dfr(id_vars,
                                   function(i_v){
                                     
                                     n5_sy <- svysummary(eligible_analytic_svy,
                                                         c_vars = continuous_vars,
                                                         p_vars = proportion_vars,
                                                         g_vars = grouped_vars,
                                                         id_vars = i_v
                                     ) %>% 
                                       mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                       mutate(est_ci = paste0(estimate," (",
                                                              lci,", ",uci,")"));
                                     
                                     # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                     n5_ct <- eligible_analytic_sample %>% 
                                       group_by_at(vars(one_of(i_v))) %>% 
                                       summarize_at(vars(one_of(c(
                                         continuous_vars,
                                         proportion_vars,
                                         grouped_vars
                                       ))),
                                       list(n = ~sum(!is.na(.)))) %>% 
                                       pivot_longer(names_to="variable",values_to="n",cols=-one_of(i_v)) %>% 
                                       mutate(variable = str_replace(variable,"_n$",""));
                                     
                                     n5_out <- left_join(n5_sy,
                                                         n5_ct,
                                                         by=c(i_v[i_v!=""],"variable")) %>% 
                                       
                                       # Restrict to those cells with more than 100 observations -- not applicable for us
                                       # dplyr::filter(n > 100) %>% 
                                       mutate(stratification = group_vars) %>% 
                                       rename_at(vars(one_of(group_vars)),~c("strata")) %>% 
                                       mutate_at(vars(one_of("strata")),~as.character(.))
                                     
                                     return(n5_out)
                                     
                                   })



analytic_sample_summary %>% 
  mutate(residence = case_when(is.na(residence) ~ "Total",
                               TRUE ~ residence)) %>% 
  
  write_csv(.,file = "analysis/nfaan01_analytic sample characteristics.csv")
