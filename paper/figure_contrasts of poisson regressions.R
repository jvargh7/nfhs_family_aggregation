
coefs = read_csv("analysis/nfaan05_poisson regression of familial aggregation.csv") %>% 
  dplyr::filter(str_detect(term,"I\\(o_"))


contrasts = read_csv("analysis/nfaan05_contrasts of poisson regression of familial aggregation.csv") %>% 
  dplyr::filter(contrast != "Contrast 3") %>% 
  mutate(level = case_when(contrast == "Contrast 1" & str_detect(modifier,"sex") ~ "Female",
                           contrast == "Contrast 2" & str_detect(modifier,"sex") ~ "Male",
                           contrast == "Contrast 1" & str_detect(modifier,"age_category") ~ "18-39",
                           contrast == "Contrast 2" & str_detect(modifier,"age_category") ~ str_replace(modifier,"age_category",""),
                           contrast == "Contrast 1" & str_detect(modifier,"residence") ~ "Rural",
                           contrast == "Contrast 2" & str_detect(modifier,"residence") ~ "Urban",
                           TRUE  ~ NA_character_
                           )) %>% 
  distinct(contrast,model,level,.keep_all=TRUE) %>% 
  dplyr::select(level,exposure,model,Estimate,LCI,UCI)

# Exponentiate coefficients and LCI, UCI


# Plot separately for each model 

# X axis: Prevalence ratio + Confidence intervals
# Y axis: Model type
# Color: Level
# geom_point
# geom_errorbarh
