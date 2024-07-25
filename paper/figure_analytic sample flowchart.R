# Creating a data frame for ALL respondents:
# Read in cleaned datasets
iapr7e_female_cleaned <- readRDS(paste0(path_family_aggregation_folder, "/working/cleaned/iapr7e_female_cleaned.RDS"))
iapr7e_male_cleaned <- readRDS(paste0(path_family_aggregation_folder, "/working/cleaned/iapr7e_male_cleaned.RDS"))
iair7e_cleaned <- readRDS(paste0(path_family_aggregation_folder, "/working/cleaned/iair7e_cleaned.RDS"))
iamr7e_cleaned <- readRDS(paste0(path_family_aggregation_folder, "/working/cleaned/iamr7e_cleaned.RDS"))

# Identify eligible adults
eligible_adults <- bind_rows(iair7e_cleaned, iamr7e_cleaned) %>% 
  distinct(cluster, hhid, linenumber, .keep_all = TRUE)

# Ensure names are similar across datasets
table(names(iapr7e_female_cleaned) %in% names(iapr7e_male_cleaned))

# Create a data set to determine the total number of adults we started with:
total_adults <- bind_rows(iapr7e_female_cleaned, iapr7e_male_cleaned) %>% 
  inner_join(eligible_adults %>% distinct(cluster, hhid), by = c("cluster", "hhid")) %>% 
  distinct(cluster, hhid, linenumber, .keep_all = TRUE)

# Determine the number of men and women by residence:
sex_residence_table <- table(total_adults$sex, total_adults$residence)
print(sex_residence_table)

# Determine the total number of urban and rural households in the ALL respondents sample:
total_adults <- total_adults %>%
  mutate(household_id = paste(cluster, hhid, sep = "_"))
unique_households <- total_adults %>%
  distinct(household_id, residence) %>%
  count(residence)
print(unique_households)

# The all_adults data set should be re-created:
eligible_adults <- bind_rows(iair7e_cleaned, iamr7e_cleaned) %>% 
  dplyr::filter(age >= 18) %>% 
  distinct(cluster, hhid, linenumber, .keep_all = TRUE)

all_adults <- bind_rows(iapr7e_female_cleaned, iapr7e_male_cleaned) %>% 
  dplyr::filter(age >= 18, pregnant %in% c(0, NA_real_)) %>% 
  inner_join(eligible_adults %>% distinct(cluster, hhid), by = c("cluster", "hhid")) %>% 
  distinct(cluster, hhid, linenumber, .keep_all = TRUE) %>% 
  dplyr::filter(relationship_hh_head %in% c(1,2,3,4,5,6,7,8,9,10,11,13,14,15,16)) %>% 
  mutate(relationship_hh_head = case_when(
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
  ))
aa_sex_residence_table <- table(all_adults$sex, all_adults$residence)
print(aa_sex_residence_table)

# Determine the total number of urban and rural households in the all_adults sample:
all_adults <- all_adults %>%
  mutate(household_id = paste(cluster, hhid, sep = "_"))
aa_unique_households <- all_adults %>%
  distinct(household_id, residence) %>%
  count(residence)
print(aa_unique_households)

# Now making a data frame for only adults with available blood pressure measurements:
bp_all_adults <- all_adults %>%
  dplyr::filter(!is.na(htn_disease))

# Determine the number of men and women by residence:
bp_sex_residence_table <- table(bp_all_adults$sex, bp_all_adults$residence)
print(bp_sex_residence_table)

# Determine the total number of urban and rural households in the bp_all_adults data frame:
bp_all_adults <- bp_all_adults %>%
  mutate(household_id = paste(cluster, hhid, sep = "_"))
bp_unique_households <- bp_all_adults %>%
  distinct(household_id, residence) %>%
  count(residence)
print(bp_unique_households)

# Finally, we can use the already created all_adults analytic sample to determine the final sex and residence breakdown:
analytic_sex_residence_table <- table(all_adults_analytic_sample$sex, all_adults_analytic_sample$residence)
print(analytic_sex_residence_table)

# We can also determine the breakdown of unique households by residence:
all_adults_analytic_sample <- all_adults_analytic_sample %>%
  mutate(household_id = paste(cluster, hhid, sep = "_"))
all_adults_unique_households <- all_adults_analytic_sample %>%
  distinct(household_id, residence) %>%
  count(residence)
print(all_adults_unique_households)

