# Call in previous survey design code.
source("/Users/krishnasanaka/Desktop/Public Health Research/Simple Descriptives Task_3-1-2024_KS.R")

# Call in the IAIR data set.
iair7a_variables <- readxl::read_excel("/Users/krishnasanaka/Desktop/Public Health Research/IAIR and IAPR Basic Identifiers.xlsx",sheet="7a variables") %>% 
  rename("selected" = iair7a) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

IAIR <- read_dta(paste0("/Users/krishnasanaka/Desktop/Public Health Research/IAIR7EDT/IAIR7EFL.DTA"),col_select = iair7a_variables$selected) %>% 
  rename_with(~ iair7a_variables$new_var[which(iair7a_variables$selected == .x)], 
              .cols = iair7a_variables$selected) 

# Checking how many people in restricted_dataset are in IAIR:
# restricted_dataset$unique_id <- paste(restricted_dataset$cluster, restricted_dataset$hhid, restricted_dataset$psu, restricted_dataset$strata, restricted_dataset$state, sep = "_")
# IAIR$unique_id <- paste(IAIR$cluster, IAIR$hhid, IAIR$psu, IAIR$strata, IAIR$state, sep = "_")
# 
# overlap_IAIR <- restricted_dataset$unique_id %in% IAIR$unique_id
# 
# num_overlap_IAIR <- sum(overlap_IAIR)
# print(num_overlap_IAIR)
# 449,207 people from the restricted_dataset are in IAIR.

# JV: What you did is fine when merging from IAIR to IAPR, but not the other way around. Showing a better way below
# Assuming the restricted_dataset is at the household level...

iair_in_restricted <- IAIR %>% 
  inner_join(restricted_dataset %>% 
               dplyr::select(cluster,hhid),
             by = c("cluster","hhid")) 

print(nrow(iair_in_restricted))


# Call in the IAMR data set.
iamr7a_variables <- readxl::read_excel("/Users/krishnasanaka/Desktop/Public Health Research/IAIR and IAPR Basic Identifiers.xlsx",sheet="7a variables") %>% 
  rename("selected" = iamr7a) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

IAMR <- read_dta(paste0("/Users/krishnasanaka/Desktop/Public Health Research/IAMR7EDT/IAMR7EFL.DTA"),col_select = iamr7a_variables$selected) %>% 
  rename_with(~ iamr7a_variables$new_var[which(iamr7a_variables$selected == .x)], 
              .cols = iamr7a_variables$selected) 

# # Checking how many people in restricted_dataset are in IAMR:
# restricted_dataset$unique_id <- paste(restricted_dataset$cluster, restricted_dataset$hhid, restricted_dataset$psu, restricted_dataset$strata, restricted_dataset$state, sep = "_")
# IAMR$unique_id <- paste(IAMR$cluster, IAMR$hhid, IAMR$psu, IAMR$strata, IAMR$state, sep = "_")
# 
# overlap_IAMR <- restricted_dataset$unique_id %in% IAMR$unique_id
# 
# num_overlap_IAMR <- sum(overlap_IAMR)
# print(num_overlap_IAMR)
# # 65,745 people from the restricted_dataset are in IAIR.

iamr_in_restricted <- IAMR %>% 
  inner_join(restricted_dataset %>% 
               dplyr::select(cluster,hhid),
             by = c("cluster","hhid")) 
print(nrow(iamr_in_restricted))

# Figuring out the sex distribution of the 1.6 million adults (actual number 1,604,795):

sex_nfhs5_men_mutated <- nfhs5_men_mutated %>%
  mutate(sex = "Male")
sex_nfhs5_women_mutated <- nfhs5_women_mutated %>%
  mutate(sex = "Female")

selected_sex_nfhs5_men_mutated <- dplyr::select(sex_nfhs5_men_mutated,cluster,hhid,linenumber,strata,psu,state,sampleweight,sex)
selected_sex_nfhs5_women_mutated <- dplyr::select(sex_nfhs5_women_mutated,cluster,hhid,linenumber,strata,psu,state,sampleweight,sex)

sex_combined_dataset <- bind_rows(selected_sex_nfhs5_men_mutated,selected_sex_nfhs5_women_mutated)

sex_combined_dataset$unique_id <- paste(sex_combined_dataset$cluster, sex_combined_dataset$hhid, sex_combined_dataset$psu, sex_combined_dataset$strata, sex_combined_dataset$state, sep = "_")

sex_dataset <- inner_join(sex_combined_dataset,restricted_dataset,by="unique_id")

sex_unique_ids <- unique(sex_dataset$unique_id)
restricted_unique_ids <- unique(restricted_dataset$unique_id)

unique_ids_only_in_sex <- setdiff(sex_unique_ids, restricted_unique_ids)

print(length(unique_ids_only_in_sex))
# It does not appear that there are any extra unique_ids in the sex_dataset...

prop.table(table(sex_dataset$sex))

# The sample is 50.3% female and 49.7% male. However, the data set does not appear to be the correct size; it has 1,712,152 observations, or about 107,357 more than the restricted_dataset.

# JV: To find the sex distribution


iapr_restricted_dataset <- bind_rows(nfhs5_women %>% mutate(female = 1),
                                     nfhs5_men %>% mutate(female = 0)) %>% 
  inner_join(restricted_dataset %>% 
               dplyr::select(cluster,hhid),
             by = c("cluster","hhid"))

iapr_survey_object = iapr_restricted_dataset %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

iapr_survey_object %>% 
  summarize(female = survey_prop(female,vartype = "ci"))

