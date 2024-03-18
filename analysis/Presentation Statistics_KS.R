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
restricted_dataset$unique_id <- paste(restricted_dataset$cluster, restricted_dataset$hhid, restricted_dataset$psu, restricted_dataset$strata, restricted_dataset$state, sep = "_")
IAIR$unique_id <- paste(IAIR$cluster, IAIR$hhid, IAIR$psu, IAIR$strata, IAIR$state, sep = "_")

overlap_IAIR <- restricted_dataset$unique_id %in% IAIR$unique_id

num_overlap_IAIR <- sum(overlap_IAIR)
print(num_overlap_IAIR)

prop_overlap_IAIR <- num_overlap_IAIR/length(IAIR$linenumber)
print(prop_overlap_IAIR)
# 449,207 people from the restricted_dataset are in IAIR, which represents 62.1% of IAIR respondents.

# Call in the IAMR data set.
iamr7a_variables <- readxl::read_excel("/Users/krishnasanaka/Desktop/Public Health Research/IAIR and IAPR Basic Identifiers.xlsx",sheet="7a variables") %>% 
  rename("selected" = iamr7a) %>% 
  dplyr::select(new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

IAMR <- read_dta(paste0("/Users/krishnasanaka/Desktop/Public Health Research/IAMR7EDT/IAMR7EFL.DTA"),col_select = iamr7a_variables$selected) %>% 
  rename_with(~ iamr7a_variables$new_var[which(iamr7a_variables$selected == .x)], 
              .cols = iamr7a_variables$selected) 

# Checking how many people in restricted_dataset are in IAMR:
restricted_dataset$unique_id <- paste(restricted_dataset$cluster, restricted_dataset$hhid, restricted_dataset$psu, restricted_dataset$strata, restricted_dataset$state, sep = "_")
IAMR$unique_id <- paste(IAMR$cluster, IAMR$hhid, IAMR$psu, IAMR$strata, IAMR$state, sep = "_")

overlap_IAMR <- restricted_dataset$unique_id %in% IAMR$unique_id

num_overlap_IAMR <- sum(overlap_IAMR)
print(num_overlap_IAMR)

prop_overlap_IAMR <- num_overlap_IAMR/length(IAMR$linenumber)
print(prop_overlap_IAMR)

# 65,745 people from the restricted_dataset are in IAIR, which represents 64.6% of IAIR respondents.

# Now creating survey object for running more descriptive statistics:

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

# This survey object contains 1,712,152 individuals.

# Finding sex distribution: 

sex_result <- iapr_survey_object %>%
  group_by(female) %>% summarize(p = survey_prop(vartype="ci",na.rm=TRUE))
print(sex_result)

# According to this calculation, the sample is 49.6% male and 50.4% female.

# Finding age distribution:
iapr_survey_object <- mutate(iapr_survey_object,
                             age_category = case_when(
                               age >= 18 & age < 30 ~ "18-29",
                               age >= 30 & age < 40 ~ "30-39",
                               age >= 40 & age < 50 ~ "40-49",
                               age >= 50 & age < 60 ~ "50-59",
                               age >= 60 & age < 70 ~ "60-69",
                               age >= 70 ~ "70 and above"
                             ))

age_result <- iapr_survey_object %>%
  group_by(age_category) %>% summarize(p = survey_prop(vartype="ci",na.rm=TRUE))
print(age_result)

# According to this calculation, 30.1% of respondents were aged 18-29, 20.4% 30-39, 17.8% 40-49, 14.3% 50-59, 11.1% 60-69, and 6.3% 70 or older.

# Finding urban/rural distribution:
residence_result <- iapr_survey_object %>%
  group_by(residence) %>% summarize(p = survey_prop(vartype="ci",na.rm=TRUE))
print(residence_result)

# According to this calculation, 32.3% of respondents lived in an urban area and 67.7% of respondents lived in a rural area. 