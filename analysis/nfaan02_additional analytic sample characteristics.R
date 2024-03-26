source("/Users/krishnasanaka/Desktop/Public Health Research/nfhs_family_aggregation-main/analysis/nfaan01_analytic sample characteristics.R")

# Determining the Rural/Urban distribution of ALL eligible female respondents:
all_eligible_female_residence_table <- table(iair7e_cleaned$residence)
print(all_eligible_female_residence_table)

# Determining the Rural/Urban distribution of ALL eligible male respondents:
all_eligible_male_residence_table <- table(iamr7e_cleaned$residence)
print(all_eligible_male_residence_table)

# Determining the total number of urban and rural households in these data sets:
iair7e_cleaned$unique_id <- paste(iair7e_cleaned$cluster, iair7e_cleaned$hhid, sep = "_")
iamr7e_cleaned$unique_id <- paste(iamr7e_cleaned$cluster, iamr7e_cleaned$hhid, sep = "_")
pre_eligible_adults <- bind_rows(iair7e_cleaned,
                             iamr7e_cleaned)
urban_pre_eligible_adults <- pre_eligible_adults %>% 
  dplyr::filter(pre_eligible_adults$residence == 1)
rural_pre_eligible_adults <- pre_eligible_adults %>% 
  dplyr::filter(pre_eligible_adults$residence == 2)
urban_pre_eligible_hh <- length(unique(urban_pre_eligible_adults$unique_id))
print(urban_pre_eligible_hh)
rural_pre_eligible_hh <- length(unique(rural_pre_eligible_adults$unique_id))
print(rural_pre_eligible_hh)

# Determining the Rural/Urban distribution of adult, non-pregnant respondents:
sex_urbanicity_table <- table(eligible_adults$sex, eligible_adults$residence)
print(sex_urbanicity_table)

# Determining the total number of urban and rural households in this data set:
eligible_adults$unique_id <- paste(eligible_adults$cluster, eligible_adults$hhid, sep = "_")
urban_eligible_adults <- eligible_adults %>% 
  dplyr::filter(eligible_adults$residence == 1)
rural_eligible_adults <- eligible_adults %>% 
  dplyr::filter(eligible_adults$residence == 2)
urban_eligible_hh <- length(unique(urban_eligible_adults$unique_id))
print(urban_eligible_hh)
rural_eligible_hh <- length(unique(rural_eligible_adults$unique_id))
print(rural_eligible_hh)

# Determining the Rural/Urban distribution of respondents with valid BPs:
# Run BP code from nfapre02 file
eligible_adults_validhtn <- eligible_adults %>%
  dplyr::filter(!is.na(sbp) & !is.na(dbp) & !is.na(toldhigh_bp))
validhtn_sex_urbanicity_table <- table(eligible_adults_validhtn$sex,eligible_adults_validhtn$residence)
print(validhtn_sex_urbanicity_table)

# Determining the total number of urban and rural households in this data set:
eligible_adults_validhtn$unique_id <- paste(eligible_adults_validhtn$cluster, eligible_adults_validhtn$hhid, sep = "_")
urban_eligible_adults_validhtn <- eligible_adults_validhtn %>% 
  dplyr::filter(eligible_adults_validhtn$residence == 1)
rural_eligible_adults_validhtn <- eligible_adults_validhtn %>% 
  dplyr::filter(eligible_adults_validhtn$residence == 2)
urban_eligible_adults_validhtn_hh <- length(unique(urban_eligible_adults_validhtn$unique_id))
print(urban_eligible_adults_validhtn_hh)
rural_eligible_adults_validhtn_hh <- length(unique(rural_eligible_adults_validhtn$unique_id))
print(rural_eligible_adults_validhtn_hh)

# Determining the Rural/Urban distribution of respondents with at least one other household member with a valid BP measurement:
analytic_sex_urbanicity_table <- table(eligible_analytic_sample$sex, eligible_analytic_sample$residence)
print(analytic_sex_urbanicity_table)

# Determining the total number of urban and rural households in this data set:
eligible_analytic_sample$unique_id <- paste(eligible_analytic_sample$cluster, eligible_analytic_sample$hhid, sep = "_")
urban_eligible_analytic_sample <- eligible_analytic_sample %>% 
  dplyr::filter(eligible_analytic_sample$residence == "Urban")
rural_eligible_analytic_sample <- eligible_analytic_sample %>% 
  dplyr::filter(eligible_analytic_sample$residence == "Rural")
urban_eligible_analytic_sample_hh <- length(unique(urban_eligible_analytic_sample$unique_id))
print(urban_eligible_analytic_sample_hh)
rural_eligible_analytic_sample_hh <- length(unique(rural_eligible_analytic_sample$unique_id))
print(rural_eligible_analytic_sample_hh)
