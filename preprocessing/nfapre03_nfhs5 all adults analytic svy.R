rm(list=ls());gc();source(".Rprofile")



all_adults_analytic_sample <- readRDS(paste0(path_family_aggregation_folder,"/working/cleaned/nfapre02_all_adults_analytic_sample.RDS")) 

# Merge all_adults with household dataset and create the survey design object

# Does not account for missingness in analytic sample

# Create the survey design object
all_adults_analytic_svy <- all_adults_analytic_sample %>% 
  as_survey_design(.data = ., ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer") %>%
  # Creating a new variable for hypertension status
  mutate(htn_status = case_when(
    htn_diagnosed == 1 ~ "d",  # Diagnosed hypertension
    htn_disease == 1 & htn_diagnosed == 0 ~ "u",  # Undiagnosed hypertension
    htn_disease == 0 ~ "n"  # No hypertension
  )) %>%
  # Creating a new variable for household size category
  mutate(hh_size_cat = case_when(
    nmembers == 2 ~ "2",
    nmembers >= 3 & nmembers <= 4 ~ "3-4",
    nmembers >= 5 ~ "5"
  ))

hypertension_all_adults_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::filter(htn_disease == 1) %>% 
  as_survey_design(.data = ., ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer") 

nondisease_all_adults_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::filter(htn_diagnosed == 0) %>% 
  as_survey_design(.data = ., ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer") 
