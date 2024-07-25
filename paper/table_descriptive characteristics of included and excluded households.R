# We can remake the all_adults_analytic sample but include respondents who did not have another household member with a valid BP measurement:
bp_adults_analytic_sample <- all_adults %>% 
  inner_join(hh_iapr %>% dplyr::select(-nmembers), by = c("cluster", "hhid")) %>% 
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

# Now, we can make an excluded sample consisting of the households that were lost between bp_adults_analytic_sample and all_adults_analytic_sample:
excluded_adults_analytic_sample <- bp_adults_analytic_sample %>%
  anti_join(all_adults_analytic_sample, by = c("cluster", "hhid"))

# Now, we can create a survey design:
excluded_adults_analytic_svy <- excluded_adults_analytic_sample %>% 
  as_survey_design(.data = ., ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

# We should create survey designs for just heads of households:
heads_all_adults_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::filter(relationship_hh_head == "Head") %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

heads_excluded_adults_analytic_svy <- excluded_adults_analytic_sample %>% 
  dplyr::filter(relationship_hh_head == "Head") %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

# Now, we can create a unique identifier variable and begin to define the formulas that we will use:
# Create a unique identifier for each combination of cluster and hhid:
all_adults_analytic_sample$household <- paste0(all_adults_analytic_sample$cluster, "_", all_adults_analytic_sample$hhid)

excluded_adults_analytic_sample$household <- paste0(excluded_adults_analytic_sample$cluster, "_", excluded_adults_analytic_sample$hhid)

# Creating the formulas that we will use:
# Function to calculate weighted N for unique combinations
calculate_weighted_unique_household_ids <- function(svy_design, subset = NULL) {
  # Subset the survey design if a subset condition is provided
  if (!is.null(subset)) {
    svy_design <- subset(svy_design, eval(subset))
  }
  
  # Extract the data from the survey design
  survey_data <- svy_design$variables
  
  # Get the weights from the survey design
  weights <- weights(svy_design)
  
  # Extract the unique household IDs and their corresponding weights
  unique_households <- unique(survey_data$cluster_hhid)
  unique_weights <- sapply(unique_households, function(hh_id) {
    sum(weights[survey_data$cluster_hhid == hh_id])
  })
  
  # Calculate the weighted total of unique household IDs
  weighted_total_unique_households <- sum(unique_weights)
  
  return(weighted_total_unique_households)
}

# Function to calculate mean and confidence interval for age
calculate_mean_ci_age <- function(svy_design, variable, subset_expr = NULL) {
  if (!is.null(subset_expr)) {
    svy_design <- subset(svy_design, eval(subset_expr))
  }
  mean_result <- svymean(as.formula(paste("~", variable)), svy_design)
  ci_result <- confint(mean_result)
  estimate <- coef(mean_result)[1]
  lower_ci <- ci_result[1, 1]
  upper_ci <- ci_result[1, 2]
  result <- paste0(round(estimate, 4), " (", round(lower_ci, 4), ", ", round(upper_ci, 4), ")")
  return(result)
}

# Function to calculate mean and confidence interval
calculate_mean_ci <- function(svy_design, variable, subset_expr = NULL) {
  if (!is.null(subset_expr)) {
    svy_design <- subset(svy_design, eval(subset_expr))
  }
  mean_result <- svymean(as.formula(paste("~", variable)), svy_design)
  ci_result <- confint(mean_result)
  estimate <- coef(mean_result)[1]
  lower_ci <- ci_result[1, 1]
  upper_ci <- ci_result[1, 2]
  
  # Calculate the inverse proportion
  inverse_estimate <- 1 - estimate
  inverse_lower_ci <- 1 - upper_ci
  inverse_upper_ci <- 1 - lower_ci
  
  result <- paste0(round(inverse_estimate, 4), " (", round(inverse_lower_ci, 4), ", ", round(inverse_upper_ci, 4), ")")
  return(result)
}

# Function to calculate median and IQR
calculate_median_iqr <- function(svy_design, variable, subset_expr = NULL) {
  if (!is.null(subset_expr)) {
    svy_design <- subset(svy_design, eval(subset_expr))
  }
  median_result <- svyquantile(as.formula(paste("~", variable)), svy_design, quantiles = 0.5, ci = TRUE)
  median_value <- as.numeric(median_result[[1]])
  iqr_result <- svyquantile(as.formula(paste("~", variable)), svy_design, quantiles = c(0.25, 0.75), ci = TRUE)
  iqr_25 <- as.numeric(iqr_result[[1]][1])
  iqr_75 <- as.numeric(iqr_result[[1]][2])
  result <- paste0(round(median_value, 4), " (", round(iqr_25, 4), ", ", round(iqr_75, 4), ")")
  return(result)
}

# Function to extract weighted N and format it for included and excluded groups
extract_weighted_n_row <- function(weighted_n_included, weighted_n_excluded, variable_name) {
  included_value <- coef(weighted_n_included)
  excluded_value <- coef(weighted_n_excluded)
  formatted_row <- data.frame(Variable = variable_name, Included = as.character(included_value), Excluded = as.character(excluded_value))
  return(formatted_row)
}

# Function to extract the specified row and format it for included and excluded groups
extract_formatted_row <- function(df_included, df_excluded, variable_name) {
  included_value <- as.character(df_included)
  excluded_value <- as.character(df_excluded)
  formatted_row <- data.frame(Variable = variable_name, Included = included_value, Excluded = excluded_value)
  return(formatted_row)
}

# Calculate number of households:
hh_included <- length(unique(all_adults_analytic_sample$household))
hh_excluded <- length(unique(excluded_adults_analytic_sample$household))

# Perform other calculations and store results
head_age_included <- calculate_mean_ci_age(heads_all_adults_analytic_svy, "age")
head_age_excluded <- calculate_mean_ci_age(heads_excluded_adults_analytic_svy, "age")

head_female_included <- calculate_mean_ci(heads_all_adults_analytic_svy, "sex == 'Female'")
head_female_excluded <- calculate_mean_ci(heads_excluded_adults_analytic_svy, "sex == 'Female'")

hhsize_included <- calculate_median_iqr(all_adults_analytic_svy, "nmembers")
hhsize_excluded <- calculate_median_iqr(excluded_adults_analytic_svy, "nmembers")

analytic_hhsize_included <- calculate_median_iqr(all_adults_analytic_svy, "n_valid")
analytic_hhsize_excluded <- calculate_median_iqr(excluded_adults_analytic_svy, "n_valid")

men_hhsize_included <- calculate_median_iqr(all_adults_analytic_svy, "n_men")
men_hhsize_excluded <- calculate_median_iqr(excluded_adults_analytic_svy, "n_men")

women_hhsize_included <- calculate_median_iqr(all_adults_analytic_svy, "n_women")
women_hhsize_excluded <- calculate_median_iqr(excluded_adults_analytic_svy, "n_women")

consanguineous_included <- calculate_mean_ci(all_adults_analytic_svy, "blood_relation == '1'")
consanguineous_excluded <- calculate_mean_ci(excluded_adults_analytic_svy, "blood_relation == '1'")

affinal_included <- calculate_mean_ci(all_adults_analytic_svy, "blood_relation == '0'")
affinal_excluded <- calculate_mean_ci(excluded_adults_analytic_svy, "blood_relation == '0'")

hypertension_included <- calculate_mean_ci(all_adults_analytic_svy, "htn_disease == '1'")
hypertension_excluded <- calculate_mean_ci(excluded_adults_analytic_svy, "htn_disease == '1'")

self_report_hypertension_included <- calculate_mean_ci(all_adults_analytic_svy, "diagnosed_bp == 1")
self_report_hypertension_excluded <- calculate_mean_ci(excluded_adults_analytic_svy, "diagnosed_bp == 1")

lowest_included <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 1")
lowest_excluded <- calculate_mean_ci(excluded_adults_analytic_svy, "swealthq_ur == 1")

low_included <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 2")
low_excluded <- calculate_mean_ci(excluded_adults_analytic_svy, "swealthq_ur == 2")

middle_included <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 3")
middle_excluded <- calculate_mean_ci(excluded_adults_analytic_svy, "swealthq_ur == 3")

high_included <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 4")
high_excluded <- calculate_mean_ci(excluded_adults_analytic_svy, "swealthq_ur == 4")

highest_included <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 5")
highest_excluded <- calculate_mean_ci(excluded_adults_analytic_svy, "swealthq_ur == 5")

# Combine the results into the master table
master_table <- data.frame(Variable = c(
  "N",
  "Age of head of household",
  "Female head of household (%)",
  "Household size (median [IQR])",
  "Household members in analytic sample (median [IQR])",
  "Men (median [IQR])",
  "Women (median [IQR])",
  "Consanguineous",
  "Affinal",
  "Hypertension",
  "Self-reported hypertension",
  "Lowest wealth quintile",
  "Low wealth quintile",
  "Middle wealth quintile",
  "High wealth quintile",
  "Highest wealth quintile"
))

master_table <- rbind(
  extract_weighted_n_row(hh_included, hh_excluded, "N"),
  extract_formatted_row(head_age_included, head_age_excluded, "Age of head of household"),
  extract_formatted_row(head_female_included, head_female_excluded, "Female head of household (%)"),
  extract_formatted_row(hhsize_included, hhsize_excluded, "Household size (median [IQR])"),
  extract_formatted_row(analytic_hhsize_included, analytic_hhsize_excluded, "Household members in analytic sample (median [IQR])"),
  extract_formatted_row(men_hhsize_included, men_hhsize_excluded, "Men (median [IQR])"),
  extract_formatted_row(women_hhsize_included, women_hhsize_excluded, "Women (median [IQR])"),
  extract_formatted_row(consanguineous_included, consanguineous_excluded, "Consanguineous"),
  extract_formatted_row(affinal_included, affinal_excluded, "Affinal"),
  extract_formatted_row(hypertension_included, hypertension_excluded, "Hypertension"),
  extract_formatted_row(self_report_hypertension_included, self_report_hypertension_excluded, "Self-reported hypertension"),
  extract_formatted_row(lowest_included, lowest_excluded, "Lowest wealth quintile"),
  extract_formatted_row(low_included, low_excluded, "Low wealth quintile"),
  extract_formatted_row(middle_included, middle_excluded, "Middle wealth quintile"),
  extract_formatted_row(high_included, high_excluded, "High wealth quintile"),
  extract_formatted_row(highest_included, highest_excluded, "Highest wealth quintile")
)

# Set column names
colnames(master_table) <- c("Variable", "Included", "Excluded")

# View the master table
print(master_table)

# Optionally save the master table to a CSV file
write.csv(master_table, "paper/table_excluded_family_characteristics.csv", row.names = FALSE)



