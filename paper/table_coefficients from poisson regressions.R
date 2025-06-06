read_csv("analysis/nfaan05_poisson regression of familial aggregation.csv") %>% 
  dplyr::filter(str_detect(term,"I\\(")) %>% 
  dplyr::select(term,model,Coef_CI) %>% 
  separate(model,into=c("type","adjustment"),sep=1) %>% 
  mutate(adjustment = case_when(adjustment == 0 ~ "Unadjusted",
                                adjustment == 1 ~ "Adjusted",
                                TRUE ~ NA_character_),
         type_long = case_when(type == "M" ~ "htn_disease ~ o_htn",
                               type == "S" ~ "htn_disease ~ o_htn_blood_related",
                               type == "T" ~ "htn_disease ~ o_htn_not_blood_related",
                               type == "U" ~ "htn_disease ~ o_htn_blood_related + o_htn_not_blood_related",
                               type == "P" ~ "htn_diagnosed ~ o_diagnosedhtn",
                               type == "Q" ~ "htn_disease ~ o_undiagnosedhtn",
                               type == "N" ~ "htn_disease ~ o_diagnosedhtn",
                               TRUE ~ NA_character_)) %>% 
  pivot_wider(names_from=adjustment,values_from=Coef_CI) %>% 
  write_csv(.,"paper/table_coefficients from poisson regressions.csv")




bind_rows(
           read_csv("paper/table_contrasts and coefficients of poisson regression by relationship.csv"),
          read_csv("paper/table_contrasts and coefficients of poisson regression by diagnosis status.csv")) %>% 
  mutate(coef_ci = paste0(round(Estimate,2), "(",
                          round(LCI,2),", ",
                          round(UCI,2),")")) %>% 
  dplyr::select(level,exposure,coef_ci) %>% 
  pivot_wider(names_from=exposure,values_from=coef_ci) %>% 
  write_csv("paper/table_familial aggregation among adults by sociodemographic.csv")
