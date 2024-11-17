# Clean up the environment and read in datasets
rm(list=ls()); gc(); source(".Rprofile")

iapr7e_female_cleaned <- readRDS(paste0(path_family_aggregation_folder, "/working/cleaned/iapr7e_female_cleaned.RDS"))
iapr7e_male_cleaned <- readRDS(paste0(path_family_aggregation_folder, "/working/cleaned/iapr7e_male_cleaned.RDS"))
iair7e_cleaned <- readRDS(paste0(path_family_aggregation_folder, "/working/cleaned/iair7e_cleaned.RDS"))
iamr7e_cleaned <- readRDS(paste0(path_family_aggregation_folder, "/working/cleaned/iamr7e_cleaned.RDS"))

# Identify eligible adults
eligible_adults <- bind_rows(iair7e_cleaned, iamr7e_cleaned) %>%
  dplyr::filter(age >= 18) %>%
  distinct(cluster, hhid, linenumber, .keep_all = TRUE)

# Ensure names are similar across datasets
table(names(iapr7e_female_cleaned) %in% names(iapr7e_male_cleaned))

# Create a dataset to determine the total number of adults we started with
total_adults <- bind_rows(iapr7e_female_cleaned, iapr7e_male_cleaned) %>%
  inner_join(eligible_adults %>% distinct(cluster, hhid), by = c("cluster", "hhid")) %>%
  distinct(cluster, hhid, linenumber, .keep_all = TRUE)

# Restrict to adults (>= 18 years) and bind rows
all_adults <- bind_rows(iapr7e_female_cleaned, iapr7e_male_cleaned) %>%
  dplyr::filter(age >= 18, pregnant %in% c(0, NA_real_)) %>%
  inner_join(eligible_adults %>% distinct(cluster, hhid), by = c("cluster", "hhid")) %>%
  distinct(cluster, hhid, linenumber, .keep_all = TRUE) %>%
  dplyr::filter(relationship_hh_head %in% c(1,2,3,4,5,6,7,8,9,10,11,13,14,15,16)) %>%
  mutate(
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
    )
  ) %>%
  # Step 1: Classify relationships as 'blood_relation = 0" or "blood_relation = 1"
  
  mutate(
    blood_relation = case_when(
      relationship_hh_head == "Head" ~ "1",  # Household head is consanguineal
      relationship_hh_head == "Wife or husband" ~ "0",  # Spouse is affinal to household head
      relationship_hh_head %in% c("Son/daughter", "Grandchild", "Parent", "Brother/sister", "Niece/nephew by blood") ~ "1",  # Other consanguineal relations
      TRUE ~ "0"  # All others are non-consanguineal
    )
  )


# Step 2. Calculate total hypertension counts for blood-related and non-blood-related members at the household level
hh_iapr <- all_adults %>%
  left_join(eligible_adults %>% select(cluster, hhid, linenumber) %>% mutate(is_eligible = 1), 
            by = c("cluster", "hhid", "linenumber")) %>%
  group_by(cluster, hhid) %>%
  mutate(
    # Create spouse_hh_htn variable: 1 if the spouse has hypertension, 0 otherwise
    head_hh_htn = sum(htn_disease == 1 & relationship_hh_head == "Head", na.rm = TRUE),
    spouse_hh_htn = sum(htn_disease == 1 & relationship_hh_head == "Wife or husband", na.rm = TRUE),
    kids_hh_htn = sum(htn_disease == 1 & relationship_hh_head %in% c("Son/daughter", "Grandchild"), na.rm = TRUE)) %>% 
    ungroup() %>%
  group_by(cluster, hhid) %>%
  summarize(
    nmembers = max(nmembers),
    total_adults_measured = max(total_adults_measured),
    ndejure_members = max(ndejure_members),
    ndefacto_members = max(ndefacto_members),
    n_sampled = n(),
    n_eligible = sum(is_eligible, na.rm = TRUE), # Number of people in IAIR or IAMR
    n_valid = sum(!is.na(htn_disease)), # Number of people with valid measurements of SBP, DBP
    n_men = sum(sex == "Male", na.rm = TRUE), # Number of Male in IAPR
    n_women = sum(sex == "Female", na.rm = TRUE), # Number of Female in IAPR
    n_htn = sum(htn_disease, na.rm = TRUE), # Number of people with HTN 
    n_diagnosedhtn = sum(htn_diagnosed, na.rm = TRUE), # Number of people with diagnosed HTN
    n_undiagnosedhtn = n_htn - n_diagnosedhtn, # Number of people with undiagnosed HTN
    prop_htn = mean(htn_disease, na.rm = TRUE), # Proportion with HTN
    n_htn_blood_related = sum(htn_disease == 1 & blood_relation == "1", na.rm = TRUE), # How many people related to the head have htn? - Consanguineal to the head
    n_htn_not_blood_related = sum(htn_disease == 1 & blood_relation == "0", na.rm = TRUE), # How many people not related to the head have htn? - Affinal to the head
    head_hh_htn = max(head_hh_htn,na.rm=TRUE), # Does the head have HTN?
    spouse_hh_htn = max(spouse_hh_htn, na.rm = TRUE),  # Does the spouse of head have HTN?
    kids_hh_htn = max(kids_hh_htn,na.rm=TRUE) # Do the kids or grandkids of head have HTN?
  ) %>%
  mutate(htn_ge2 = case_when(n_valid == 0 ~ NA_real_, n_htn >= 2 ~ 1, n_htn < 2 ~ 0)) # Are there >=2 people with HTN?



# Step 3: Identify households where the spouse has hypertension
hh_with_spouse_htn <- all_adults %>%
  group_by(cluster, hhid) %>%
  summarize(spouse_hh_htn = sum(htn_disease == 1 & relationship_hh_head == "Wife or husband", na.rm = TRUE),
            kids_hh_htn = sum(htn_disease ==1 & relationship_hh_head %in% c("Son/daughter", "Grandchild"), na.rm = TRUE)) %>%
  ungroup()


# Create final dataset with analytic sample
all_adults_analytic_sample <- all_adults %>%
  inner_join(hh_iapr %>% dplyr::select(-nmembers), by = c("cluster", "hhid")) %>%
  dplyr::filter(n_valid > 1) %>%
  mutate(
    residence = case_when(residence == 1 ~ "Urban", residence == 2 ~ "Rural"),
    age_category2 = case_when(age %in% c(18:39) ~ 1, age %in% c(40:64) ~ 2, age >= 65 ~ 2, TRUE ~ NA_real_),
    age_category2 = factor(age_category2, levels = c(1:2), labels = c("18-39", "40 plus")),
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
    htn_undiagnosed = case_when(htn_disease == 1 ~ 1 - htn_diagnosed,
                                TRUE ~ NA_real_),
    o_undiagnosedhtn = o_htn - o_diagnosedhtn,
    cluster_hhid = paste0(sprintf("%05d", cluster), sprintf("%03d", hhid))
  ) %>%
  # Create variables for counts of blood relation
  # Adjust hypertension counts for blood-related and non-blood-related members,
  # considering spouse's biological children or grandchildren as consanguineal
    
  
  # n_htn_blood_related: How many people related to the head have htn? - Consanguineal to the head
  # n_htn_not_blood_related: How many people not related to the head have htn? - Affinal to the head


  
  # ind_htn_blood_related: For each individual, how many people are consanguineal to them?
  # ind_htn_not_blood_related: For each individual, how many people are affinal to them?
  mutate(ind_htn_blood_related = case_when(relationship_hh_head %in% c("Head","Son/daughter", "Grandchild", "Parent", "Brother/sister", "Niece/nephew by blood") ~ n_htn_blood_related,
                                           TRUE ~ 0),
         ind_htn_not_blood_related = case_when(relationship_hh_head %in% c("Head","Son/daughter", "Grandchild", "Parent", "Brother/sister", "Niece/nephew by blood") ~ n_htn_not_blood_related,
                                               TRUE ~ n_htn_blood_related + n_htn_not_blood_related)
         ) %>% 
  
  # adjustment_htn_blood_related: What do you need to adjust ind_htn_blood_related with
  # adjustment_htn_not_blood_related: What do you need to adjust ind_htn_not_blood_related with
  mutate(adjustment_htn_blood_related = case_when(relationship_hh_head %in% c("Son/daughter", "Grandchild") ~ spouse_hh_htn,
                                                  relationship_hh_head %in% c("Wife or husband") ~ kids_hh_htn,
                                                  TRUE ~ 0),
         adjustment_htn_not_blood_related = case_when(relationship_hh_head %in% c("Son/daughter", "Grandchild") ~ -spouse_hh_htn,
                                                 relationship_hh_head %in% c("Wife or husband") ~ -kids_hh_htn,
                                                 TRUE ~ 0)) %>% 
  mutate(
    # Consanguineal + Adjustment - Own Status accounting for blood relationship 
    o_htn_blood_related = ind_htn_blood_related + adjustment_htn_blood_related - ifelse(htn_disease == 1 & blood_relation == "1", 1, 0),
    # Affinal + Adjustment - Own Status accounting for blood relationship 
    o_htn_not_blood_related = ind_htn_not_blood_related + adjustment_htn_not_blood_related - ifelse(htn_disease == 1 & blood_relation == "0", 1, 0)
    
  ) %>% 
  dplyr::filter(!is.na(htn_disease)) %>%
  arrange(cluster, hhid, linenumber)


all_adults_analytic_sample %>% dplyr::select(cluster_hhid,spouse_hh_htn,kids_hh_htn,relationship_hh_head,
                                                  htn_disease,htn_diagnosed,htn_undiagnosed,
                                                  n_htn,n_htn_blood_related, n_htn_not_blood_related, 
                                                  ind_htn_blood_related, ind_htn_not_blood_related, 
                                                  adjustment_htn_blood_related,adjustment_htn_not_blood_related,
                                                  o_htn,o_diagnosedhtn,o_undiagnosedhtn,o_htn_blood_related,o_htn_not_blood_related) %>% 
       head(n = 10000) %>%
       # dplyr::filter(cluster_hhid == "00125017") %>% 
       # head(n = 10000) %>% 

  View()

# # Save the results
saveRDS(all_adults, paste0(path_family_aggregation_folder, "/working/cleaned/nfapre02_all_adults.RDS"))
saveRDS(total_adults, paste0(path_family_aggregation_folder, "/working/cleaned/nfapre02_total_adults.RDS"))
saveRDS(hh_iapr, paste0(path_family_aggregation_folder, "/working/cleaned/nfapre02_hh_iapr.RDS"))
saveRDS(all_adults_analytic_sample, paste0(path_family_aggregation_folder, "/working/cleaned/nfapre02_all_adults_analytic_sample.RDS"))
