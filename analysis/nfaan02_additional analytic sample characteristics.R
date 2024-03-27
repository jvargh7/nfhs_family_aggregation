source("/Users/krishnasanaka/Desktop/Public Health Research/nfhs_family_aggregation-main/analysis/nfaan01_analytic sample characteristics.R")

# Determining the Rural/Urban distribution of ALL eligible female respondents:
all_eligible_female_residence_table <- table(iair7e_cleaned$residence)
print(all_eligible_female_residence_table)

# Determining the Rural/Urban distribution of ALL eligible male respondents:
all_eligible_male_residence_table <- table(iamr7e_cleaned$residence)
print(all_eligible_male_residence_table)

# Determining the total number of urban and rural households in these data sets:
pre_eligible_adults_hh <- pre_eligible_adults%>% 
  group_by(cluster,hhid)%>% 
  distinct(cluster,hhid,.keep_all = TRUE)
urbanicity_pre_eligible_hh <- table(pre_eligible_adults_hh$residence)
print(urbanicity_pre_eligible_hh)

# Other method:
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
eligible_adults <- bind_rows(iair7e_cleaned,
                             iamr7e_cleaned) %>% 
  dplyr::filter(age >= 18, pregnant %in% c(0,NA_real_))

sex_urbanicity_table <- table(eligible_adults$sex, eligible_adults$residence)
print(sex_urbanicity_table)

# Determining the total number of urban and rural households in this data set:
eligible_adults_hh <- eligible_adults%>% 
  group_by(cluster,hhid)%>% 
  distinct(cluster,hhid,.keep_all = TRUE)
urbanicity_eligible_hh <- table(eligible_adults_hh$residence)
print(urbanicity_eligible_hh)

# Other method:
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
eligible_adults_validhtn_hh <- eligible_adults_validhtn%>% 
  group_by(cluster,hhid)%>% 
  distinct(cluster,hhid,.keep_all = TRUE)
valid_urbanicity_eligible_hh <- table(eligible_adults_validhtn_hh$residence)
print(valid_urbanicity_eligible_hh)

# Other method:
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

# Determining the total number of respondents, male/female respondents, and urban/rural respondents in the eligible analytic sample:
total_weighted_respondents <- sum(weights(eligible_analytic_svy))
print(total_weighted_respondents)

sex_totals <- svytotal(~sex,eligible_analytic_svy)
print(sex_totals)

urbanicity_totals <- svytotal(~residence,eligible_analytic_svy)
print(urbanicity_totals)

# Determining the household size distribution:
svyhist(~nmembers, design = all_adults_analytic_svy, main="Distribution of Household Size", xlab = "Household Members")
mean_hh <- svymean(~nmembers, design = all_adults_analytic_svy)
print(mean_hh)
quantiles_hh <- svyquantile(~nmembers, design = all_adults_analytic_svy, 
                                quantiles = c(0.25, 0.5, 0.75), 
                                method = "linear")
print(quantiles_hh)

# Determining the proportion of household members having hypertension by household size:
svyboxplot(prop_htn ~ nmembers, design = all_adults_analytic_svy,
           xlab = "Household Size", ylab = "Proportion with Hypertension",
           main = "Proportion of Hypertension by Household Size")
library(ggplot2)
avg_hypertension <- svyby(~prop_htn, ~nmembers, all_adults_analytic_svy, svymean)
avg_hypertension$nmembers <- as.factor(avg_hypertension$nmembers)

ggplot(avg_hypertension, aes(x = nmembers, y = prop_htn)) +
  geom_boxplot() +
  labs(title = "Average Proportion of Household Members with Hypertension by Household Size",
       x = "Household Size",
       y = "Average Proportion with Hypertension")

avg_prop_htn <- svymean(~prop_htn, design = all_adults_analytic_svy)
print(avg_prop_htn)

avg_num_htn <- svymean(~n_htn, design = all_adults_analytic_svy)
print(avg_num_htn)
