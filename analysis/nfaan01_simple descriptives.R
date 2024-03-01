# 1, Read in cleaned datasets of IAPR for women, IAPR for men with only the below columns:
# - hv001 --> cluster
# - hv002 --> hhid
# - hv003 --> linenumber
# - hv022 --> strata
# - hv021 --> psu
# - hv024 --> state
# - hv005 --> sampleweight --> divide this by 10^6 (Check Line 9: https://github.com/jvargh7/nfhs_cascade/blob/main/preprocessing/ncp_preprocessing.R)
# - systolic bp (3 variables)
# - diastolic bp (3 variables)
# - Was your blood pressure ever checked previously?
# - Were you told 2+ times you have high BP?
# - Are you currently taking medication for high BP?

# 2, Make sure names are similar across datasets

# 3. Restrict to adults (>= 18 years) and use bind_rows() from the tidyverse 


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

# Create a new column (n_htn_ge2) 
# htn_ge2: Number of households with at least 2 adults having hypertension 
# htn_ge2 = case_when(n_valid == 0 ~ NA_real_,
#           n_htn >= 2 ~ 1,
#           n_htn < 2 ~ 0)
# Target dataframe of 7 columns: cluster, hhid, n_sampled, n_valid, n_htn, prop_htn, htn_ge2


# 7. Merge with household dataset
# Take person recode dataset, and use distinct(cluster, hhid, strata, state, psu, sampleweight)
# Use left_join on the dataset from #6



# 8. Create a survey design object
# Restrict to households with at least 2 adults with valid hypertension status dplyr::filter(n_valid > 1)
# Use as_survey_design, Refer Line 19+: https://github.com/jvargh7/nfhs_cascade/blob/main/preprocessing/ncpre03_nfhs5%20total%20svydesign.R
# RESULT 1: There were XXX,XXX households where more than one adult provided valid blood pressure measurements in NFHS-5. 

# Use survey_mean(htn_ge2,proportion=TRUE,na.rm=TRUE,vartype ="ci")
# RESULT 2: Of these households, XX.X% had at least two adults who had hypertension.

