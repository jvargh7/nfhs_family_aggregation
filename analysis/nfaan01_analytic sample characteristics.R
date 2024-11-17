
# Notice how we don't have to specify the full path to the file
# R uses the working directory as a reference
source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")
# --> you would need to download this script from github.com/jvargh7/functions and place it in your local system
source("C:/code/external/functions/survey/svysummary.R") 

svysummary(all_adults_analytic_svy,
           c_vars = "age"
           # p_vars = proportion_vars,
           # g_vars = grouped_vars,
           # id_vars = i_v
)



continuous_vars <- c(
                     # "bmi",
                     "age","sbp","dbp","smokecount")
proportion_vars <- c(
                     # "highwc",
                     "htn",
                     "htn_screened","htn_disease","htn_diagnosed","htn_treated","htn_controlled",
                     "smokecurr","alcohol")
grouped_vars <- c("age_category","age_category10","age_category5","education",
                  "caste","religion","swealthq_ur","htn_status","hh_size_cat",
                  # "bmi_category",
                  "bp_group","marital3")

# Specifying that we want:
id_vars = list("", # 1. Total estimates
               c("sex"), # 2. Stratified by sex (male/female)
               c("residence") # 3. Stratified by residence (urban/rural)
               );

analytic_sample_summary <- map_dfr(id_vars,
                                   function(i_v){
                                     
                                     if(length(i_v) == 1 & i_v == ""){
                                       n5_sy <- svysummary(all_adults_analytic_svy,
                                                           c_vars = continuous_vars,
                                                           p_vars = proportion_vars,
                                                           g_vars = grouped_vars
                                                           # id_vars = i_v
                                       ) %>% 
                                         mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                         mutate(est_ci = paste0(estimate," (",
                                                                lci,", ",uci,")"));
                                       
                                     } else{
                                       
                                       n5_sy <- svysummary(all_adults_analytic_svy,
                                                           c_vars = continuous_vars,
                                                           p_vars = proportion_vars,
                                                           g_vars = grouped_vars,
                                                           id_vars = i_v
                                       ) %>% 
                                         mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
                                         mutate(est_ci = paste0(estimate," (",
                                                                lci,", ",uci,")"));
                                     }
                                     
                                     
                                     # Count of non-NA values at intersection of id_vars and each variable in proportion_vars
                                     n5_ct <- all_adults_analytic_sample %>% 
                                       group_by_at(vars(one_of(i_v))) %>% 
                                       summarize_at(vars(one_of(c(
                                         continuous_vars,
                                         proportion_vars,
                                         grouped_vars
                                       ))),
                                       list(n = ~sum(!is.na(.)))) %>% 
                                       pivot_longer(names_to="variable",values_to="n",cols=-one_of(i_v)) %>% 
                                       mutate(variable = str_replace(variable,"_n$",""));
                                     
                                     
                                     # stratification: in this case it's the first variable in the vector i_V
                                     
                                     n5_out <- left_join(n5_sy,
                                                         n5_ct,
                                                         by=c(i_v[i_v!=""],"variable")) %>% 
                                       
                                       # Restrict to those cells with more than 100 observations -- not applicable for us
                                       # dplyr::filter(n > 100) %>% 
                                       mutate(stratification = i_v[1]) %>% 
                                       rename_at(vars(one_of(i_v[1])),~c("strata")) %>% 
                                       mutate_at(vars(one_of("strata")),~as.character(.))
                                     
                                     return(n5_out)
                                     
                                   })






analytic_sample_summary %>% 
  mutate(stratification = case_when(stratification == "" ~ "Total",
                               TRUE ~ stratification),
         strata = case_when(is.na(strata) ~ "Total",
                         TRUE ~ strata)) %>% 
  write_csv(.,file = "analysis/nfaan01_analytic sample characteristics.csv")
