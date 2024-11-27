rm(list=ls());gc();source(".Rprofile")

source("C:/code/external/functions/survey/svysummary.R") 

hh_iapr <- readRDS(paste0(path_family_aggregation_folder, "/working/cleaned/nfapre02_hh_iapr.RDS")) %>% 
  dplyr::filter(n_valid >= 2) %>%
  # Creating a new variable for household size category
  mutate(valid_size_cat = case_when(
    n_valid == 2 ~ "2",
    n_valid >= 3 & n_valid <= 4 ~ "3-4",
    n_valid >= 5 ~ "5"
  ),
  cluster_hhid = paste0(sprintf("%05d", cluster), sprintf("%03d", hhid))) %>% 
  mutate(htype = case_when(n_htn == 0 ~ 0,
                           n_htn == n_diagnosedhtn ~ 1,
                           n_htn == n_undiagnosedhtn ~ 2,
                           TRUE ~ 3
  )) %>% 
  mutate(htype = factor(htype,levels=c(0:3),labels=c("No Hypertension","All Diagnosed","All Undiagnosed","Discordance")))


with(hh_iapr,table(valid_size_cat,htype))

composition = hh_iapr %>% 
  bind_rows(.,
            {.} %>% mutate(valid_size_cat = "Total")) %>% 
  as_survey_design(.data = ., 
                   ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer") %>% 
  svysummary(g_vars = "htype",
             id_vars = "valid_size_cat")


proportions = hh_iapr %>% 
  as_survey_design(.data = ., 
                   ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer") %>% 
  svysummary(g_vars = "valid_size_cat")


write_csv(composition,"analysis/nfaan04_household type by valid number of members.csv")
write_csv(proportions,"analysis/nfaan04_proportions of households by valid number of members.csv")

rm(hh_iapr);gc();

# INDIVIDUAL -----------------

all_adults_analytic_sample <- readRDS(paste0(path_family_aggregation_folder,"/working/cleaned/nfapre02_all_adults_analytic_sample.RDS")) %>%
  # Creating a new variable for hypertension status
  mutate(htn_status = case_when(
    htn_diagnosed == 1 ~ "d",  # Diagnosed hypertension
    htn_disease == 1 & htn_diagnosed == 0 ~ "u",  # Undiagnosed hypertension
    htn_disease == 0 ~ "n"  # No hypertension
  )) %>%
  # Creating a new variable for household size category
  mutate(valid_size_cat = case_when(
    n_valid == 2 ~ "2",
    n_valid >= 3 & n_valid <= 4 ~ "3-4",
    n_valid >= 5 ~ "5"
  ),
  cluster_hhid = paste0(sprintf("%05d", cluster), sprintf("%03d", hhid))) %>% 
  mutate(o_htype = case_when(o_htn == 0 ~ 0,
                             o_htn == o_diagnosedhtn ~ 1,
                             o_htn == o_undiagnosedhtn ~ 2,
                             TRUE ~ 3
  )) %>% 
  mutate(o_htype = factor(o_htype,levels=c(0:3),labels=c("No Hypertension","All Diagnosed","All Undiagnosed","Discordance")))

with(all_adults_analytic_sample,table(htn_status,o_htype))
with(all_adults_analytic_sample,table(valid_size_cat,o_htype))


composition_individual = all_adults_analytic_sample %>% 
  bind_rows(.,
            {.} %>% mutate(htn_status = "Total")) %>% 
  as_survey_design(.data = ., 
                   ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer") %>% 
  svysummary(g_vars = "o_htype",
             id_vars = "htn_status")

composition_individual_by_size_cat = all_adults_analytic_sample %>% 
  bind_rows(.,
            {.} %>% mutate(htn_status = "Total",
                           valid_size_cat = "Total")) %>% 
  as_survey_design(.data = ., 
                   ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer") %>% 
  svysummary(g_vars = "o_htype",
             id_vars = c("htn_status","valid_size_cat"))


write_csv(composition_individual,"analysis/nfaan04_household type by hypertension status of individual.csv")
write_csv(composition_individual_by_size_cat,"analysis/nfaan04_household type by hypertension status of individual and valid number of members.csv")

proportions_individual_by_size_cat = all_adults_analytic_sample %>% 
  bind_rows(.,
            {.} %>% mutate(htn_status = "Total")) %>% 
  as_survey_design(.data = ., 
                   ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer") %>% 
  svysummary(g_vars = "valid_size_cat",
             id_vars = "htn_status")

proportions_individual = all_adults_analytic_sample %>% 
  as_survey_design(.data = ., 
                   ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer") %>% 
  svysummary(p_vars = "htn_disease",
             g_vars = "htn_status")


write_csv(proportions_individual_by_size_cat,"analysis/nfaan04_proportions of individuals by valid number of members.csv")
write_csv(proportions_individual,"analysis/nfaan04_proportions of individuals.csv")
