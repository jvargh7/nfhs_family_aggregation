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
  )

# Step 1: Identify households where the spouse has hypertension
hh_with_spouse_htn <- all_adults %>%
  group_by(cluster, hhid) %>%
  summarize(spouse_hh_htn = sum(htn_disease == 1 & relationship_hh_head == "Wife or husband", na.rm = TRUE)) %>%
  ungroup()

# Step 2: Update the blood_relation for children and grandchildren
all_adults <- all_adults %>%
  left_join(hh_with_spouse_htn, by = c("cluster", "hhid")) %>%
  mutate(
    blood_relation = case_when(
      relationship_hh_head == "Head" ~ "1",  # Household head is consanguineal
      relationship_hh_head == "Wife or husband" ~ "0",  # Spouse is affinal to household head
      relationship_hh_head %in% c("Son/daughter", "Grandchild") &
        spouse_hh_htn > 0 & htn_disease == 1 ~ "1",  # Spouse's biological children/grandchildren are consanguineal if spouse has hypertension
      relationship_hh_head %in% c("Son/daughter", "Grandchild", "Parent", "Brother/sister", "Niece/nephew by blood") ~ "1",  # Other consanguineal relations
      TRUE ~ "0"  # All others are non-consanguineal
    )
  )

# Calculate total hypertension counts for blood-related and non-blood-related members at the household level
hh_iapr <- all_adults %>%
  left_join(eligible_adults %>% select(cluster, hhid, linenumber) %>% mutate(is_eligible = 1), 
            by = c("cluster", "hhid", "linenumber")) %>%
  group_by(cluster, hhid) %>%
  mutate(
    # Create spouse_hh_htn variable: 1 if the spouse has hypertension, 0 otherwise
    spouse_hh_htn = sum(htn_disease == 1 & relationship_hh_head == "Wife or husband", na.rm = TRUE),
    # Adjust hypertension counts for blood-related and non-blood-related members,
    # considering spouse's biological children or grandchildren as consanguineal
    total_htn_blood_related = sum(htn_disease == 1 & blood_relation == "1", na.rm = TRUE) + 
      ifelse(spouse_hh_htn > 0 & sum(htn_disease == 1 & blood_relation == "0", na.rm = TRUE) > 0, 1, 0),  # Count spouse's biological children/grandchildren as consanguineal if spouse has hypertension
    total_htn_not_blood_related = sum(htn_disease == 1 & blood_relation == "0", na.rm = TRUE) - 
      ifelse(spouse_hh_htn > 0 & sum(htn_disease == 1 & blood_relation == "0", na.rm = TRUE) > 0, 1, 0)  # Adjust non-blood-related counts accordingly
  ) %>%
  ungroup() %>%
  group_by(cluster, hhid) %>%
  summarize(
    nmembers = max(nmembers),
    total_adults_measured = max(total_adults_measured),
    ndejure_members = max(ndejure_members),
    ndefacto_members = max(ndefacto_members),
    n_sampled = n(),
    n_eligible = sum(is_eligible, na.rm = TRUE),
    n_valid = sum(!is.na(htn_disease)),
    n_men = sum(sex == "Male", na.rm = TRUE),
    n_women = sum(sex == "Female", na.rm = TRUE),
    n_htn = sum(htn_disease, na.rm = TRUE),
    n_diagnosedhtn = sum(htn_diagnosed, na.rm = TRUE),
    n_undiagnosedhtn = n_htn - n_diagnosedhtn,
    prop_htn = mean(htn_disease, na.rm = TRUE),
    n_htn_blood_related = sum(htn_disease == 1 & blood_relation == "1", na.rm = TRUE),
    n_htn_not_blood_related = sum(htn_disease == 1 & blood_relation == "0", na.rm = TRUE),
    total_htn_blood_related = first(total_htn_blood_related),
    total_htn_not_blood_related = first(total_htn_not_blood_related),
    spouse_hh_htn = max(spouse_hh_htn, na.rm = TRUE)  # Add spouse hypertension to the summary
  ) %>%
  mutate(htn_ge2 = case_when(n_valid == 0 ~ NA_real_, n_htn >= 2 ~ 1, n_htn < 2 ~ 0))

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
    htn_undiagnosed = 1 - htn_diagnosed,
    o_undiagnosedhtn = o_htn - o_diagnosedhtn,
    o_htn_blood_related = total_htn_blood_related - ifelse(htn_disease == 1 & blood_relation == "1", 1, 0),
    o_htn_not_blood_related = total_htn_not_blood_related - ifelse(htn_disease == 1 & blood_relation == "0", 1, 0),
    cluster_hhid = paste0(sprintf("%05d", cluster), sprintf("%03d", hhid))
  ) %>%
  dplyr::filter(!is.na(htn_disease)) %>%
  arrange(cluster, hhid, linenumber)

# Save the results
saveRDS(all_adults, paste0(path_family_aggregation_folder, "/working/cleaned/nfapre02_all_adults.RDS"))
saveRDS(total_adults, paste0(path_family_aggregation_folder, "/working/cleaned/nfapre02_total_adults.RDS"))
saveRDS(hh_iapr, paste0(path_family_aggregation_folder, "/working/cleaned/nfapre02_hh_iapr.RDS"))
saveRDS(all_adults_analytic_sample, paste0(path_family_aggregation_folder, "/working/cleaned/nfapre02_all_adults_analytic_sample.RDS"))