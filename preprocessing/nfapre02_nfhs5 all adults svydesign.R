rm(list=ls());gc();source(".Rprofile")


# 1, Read in cleaned datasets of IAPR for women, IAPR for men with only the below columns:
iapr7e_female_cleaned <- readRDS(paste0(path_family_aggregation_folder,"/working/cleaned/iapr7e_female_cleaned.RDS"))
iapr7e_male_cleaned <- readRDS(paste0(path_family_aggregation_folder,"/working/cleaned/iapr7e_male_cleaned.RDS"))
iair7e_cleaned <- readRDS(paste0(path_family_aggregation_folder,"/working/cleaned/iair7e_cleaned.RDS"))
iamr7e_cleaned <- readRDS(paste0(path_family_aggregation_folder,"/working/cleaned/iamr7e_cleaned.RDS"))

# Identify eligible adults (pregnant/non-pregnant + men) irrespective of whether they have measured blood pressure
eligible_adults <- bind_rows(iair7e_cleaned,
                             iamr7e_cleaned) %>% 
  dplyr::filter(age >= 18) %>% 
  distinct(cluster,hhid,linenumber,.keep_all=TRUE)

# 2, Make sure names are similar across datasets
table(names(iapr7e_female_cleaned) %in% names(iapr7e_male_cleaned))
# 3. Restrict to adults (>= 18 years) and use bind_rows() from the tidyverse 
# They also need to have valid BP measurements to be part of our dataset

all_adults <- bind_rows(iapr7e_female_cleaned,
                        iapr7e_male_cleaned) %>% 
  dplyr::filter(age >= 18, pregnant %in% c(0,NA_real_)) %>% 
  # Filter on eligible adult households
  inner_join(eligible_adults %>% 
               distinct(cluster,hhid),
             by = c("cluster","hhid")) %>% 
  distinct(cluster,hhid,linenumber,.keep_all=TRUE) %>% 
  dplyr::filter(relationship_hh_head %in% c(1,2,3,4,5,6,
                                            7,8,9,10,11,13,14,15,16))


# 4. Create a variable for systolic BP (name: sbp) and diastolic BP (name: dbp)
# Refer lines 103-111 of https://github.com/jvargh7/nfhs_cascade/blob/main/preprocessing/ncp_preprocessing.R


# 5. Create a variable for hypertension status
# # 1. 'Told had high BP on two or more occassions by ...' OR
# 2. SBP > 140 mmHg OR
# 3. DBP > 90 Hg
# Set variable to missing if any of the above are missing


# 6. Summarize at the household level using group_by(cluster,hhid), the following:
# n_sampled: Number of adults sampled --> n()
# n_valid: Number of adults with valid hypertension status --> sum(!is.na(htn))
# n_htn: Number of adults with hypertension --> sum(htn,na.rm=TRUE)
# prop_htn: Weighted proportion of adults with hypertension --> mean(htn, na.rm=TRUE)

hh_iapr <- all_adults  %>% 
  # Counting eligible adults in each household
  left_join(eligible_adults %>% 
              dplyr::select(cluster,hhid,linenumber) %>% 
              mutate(is_eligible = 1),
            by = c("cluster","hhid","linenumber")) %>% 
  group_by(cluster,hhid) %>% 
  summarize(
            nmembers = max(nmembers),
            total_adults_measured = max(total_adults_measured),
            ndejure_members = max(ndejure_members),
            ndefacto_members = max(ndefacto_members),
            n_sampled = n(),
            n_eligible = sum(is_eligible,na.rm = TRUE),
            n_valid = sum(!is.na(htn_disease)),
            n_htn = sum(htn_disease,na.rm=TRUE),
            n_diagnosedhtn = sum(htn_diagnosed,na.rm=TRUE),
            prop_htn = mean(htn_disease,na.rm=TRUE)
  ) %>% 
  mutate(htn_ge2 = case_when(n_valid == 0 ~ NA_real_,
                             n_htn >= 2 ~ 1,
                             n_htn < 2 ~ 0))

# How many households with at least 2 valid BP measurements
hh_iapr %>% 
  dplyr::filter(n_valid > 1) %>% 
  nrow()

# Create a new column (n_htn_ge2) 
# htn_ge2: Number of households with at least 2 adults having hypertension 
# htn_ge2 = case_when(n_valid == 0 ~ NA_real_,
#           n_htn >= 2 ~ 1,
#           n_htn < 2 ~ 0)
# Target dataframe of 7 columns: cluster, hhid, n_sampled, n_valid, n_htn, prop_htn, htn_ge2


# 7. Merge all_adults with with household dataset
# Are there duplicates

# 8. Create a survey design object
# Restrict to households with at least 2 adults with valid hypertension status dplyr::filter(n_valid > 1)
# Use as_survey_design, Refer Line 19+: https://github.com/jvargh7/nfhs_cascade/blob/main/preprocessing/ncpre03_nfhs5%20total%20svydesign.R
# RESULT 1: There were XXX,XXX households where more than one adult provided valid blood pressure measurements in NFHS-5. 


all_adults_analytic_sample <- all_adults %>% 
  inner_join(hh_iapr, by = c("cluster", "hhid")) %>% 
  dplyr::filter(n_valid > 1) %>%
  mutate(
    residence = case_when(
      residence == 1 ~ "Urban",
      residence == 2 ~ "Rural"
    ),
    age_category2 = case_when(
      age %in% c(18:39) ~ 1,
      age %in% c(40:64) ~ 2,
      age >= 65 ~ 2,
      TRUE ~ NA_real_
    ),
    age_category2 = factor(age_category2, levels = c(1:2), labels = c("18-39", "40 plus")),
    relationship_hh_head = case_when(
      relationship_hh_head == 1 ~ "Head",
      relationship_hh_head == 2 ~ "Wife or husband",
      relationship_hh_head == 3 ~ "Son/daughter",
      relationship_hh_head == 4 ~ "Son/daughter in law",
      relationship_hh_head == 5 ~ "Grandchild",
      relationship_hh_head == 6 ~ "Parent",
      relationship_hh_head == 7 ~ "Parent-In-Law",
      relationship_hh_head == 8 ~ "Brother/sister",
      relationship_hh_head == 9 ~ "Co-spouse",
      relationship_hh_head == 10 ~ "Other relative",
      relationship_hh_head == 11 ~ "Adopted/foster child",
      relationship_hh_head == 12 ~ "Not related",
      relationship_hh_head == 13 ~ "Niece/nephew by blood",
      relationship_hh_head == 14 ~ "Niece/nephew by marriage",
      relationship_hh_head == 15 ~ "Brother-in-law or sister-in-law",
      relationship_hh_head == 16 ~ "Niece/nephew",
      relationship_hh_head == 17 ~ "Domestic servant"
    ),
    blood_relation = case_when(
      relationship_hh_head %in% c("Head", "Son/daughter", "Grandchild", "Parent", "Brother/sister", "Niece/nephew by blood") ~ "1",
      TRUE ~ "0"
    ),
    bp_group = case_when(
      sbp >= 160 | dbp >= 100 ~ 4,
      sbp >= 140 | dbp >= 90 ~ 3,
      sbp >= 120 | dbp >= 80 ~ 2,
      sbp < 120 | dbp < 80 ~ 1,
      TRUE ~ NA_real_
    ),
    bp_group = factor(bp_group, levels = c(1:4), labels = c("<120/80", "<140/90", "<160/100", ">=160/100")),
    o_htn = n_htn - htn_disease,
    o_diagnosedhtn = n_diagnosedhtn - htn_diagnosed,
    htn_undiagnosed = 1 - htn_diagnosed,
    o_undiagnosedhtn = o_htn-o_diagnosedhtn,
    cluster_hhid = paste0(sprintf("%05d", cluster), sprintf("%03d", hhid))
  ) %>%
  dplyr::filter(!is.na(htn_disease)) %>% 
  arrange(cluster, hhid, linenumber)


all_adults_analytic_svy <- all_adults_analytic_sample %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

# Use survey_mean(htn_ge2,proportion=TRUE,na.rm=TRUE,vartype ="ci")
# RESULT 2: Of these households, XX.X% had at least two adults who had hypertension.
rm(all_adults,
   eligible_adults,
   iair7e_cleaned,
   iamr7e_cleaned,
   iapr7e_female_cleaned,
   iapr7e_male_cleaned)

