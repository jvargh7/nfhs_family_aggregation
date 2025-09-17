read_csv("sensitivity/nfase01_poisson regression of familial aggregation.csv") %>% 
  dplyr::filter(str_detect(term,"I\\(")) %>% 
  dplyr::select(design, term,model,Coef_CI) %>% 
  separate(model,into=c("type","adjustment"),sep=1) %>% 
  mutate(adjustment = case_when(adjustment == 0 ~ "Unadjusted",
                                adjustment == 1 ~ "Adjusted",
                                TRUE ~ NA_character_),
         type_long = case_when(type == "R" ~ "htn_diagnosed ~ o_diagnosedhtn + o_undiagnosedhtn",
                               type == "W" ~ "htn_undiagnosed ~ o_diagnosedhtn + o_undiagnosedhtn",
                               TRUE ~ NA_character_),
         exposure = case_when(term == "I(o_diagnosedhtn >= 1)TRUE" ~ "o_diagnosed_htn",
                              term == "I(o_undiagnosedhtn >= 1)TRUE" ~ "o_undiagnosed_htn",
                              TRUE ~ NA_character_)) %>%
  pivot_wider(names_from=c(adjustment),values_from=Coef_CI) %>% 
  write_csv(.,"paper/table_sensitivity coefficients from poisson regressions.csv")




