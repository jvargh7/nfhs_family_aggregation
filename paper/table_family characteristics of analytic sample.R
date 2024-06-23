# Loading in surveys:
source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")

# Create a survey design for just heads of households
heads_all_adults_analytic_svy <- all_adults_analytic_sample %>% 
  dplyr::filter(relationship_hh_head == "Head") %>% 
  as_survey_design(ids = cluster_hhid, strata = state, weight = sampleweight, nest = TRUE, variance = "YG", pps = "brewer")

# Define categories
categories <- c("N",
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
                "Household wealth (%)",
                "Lowest", 
                "Low", 
                "Middle", 
                "High", 
                "Highest")

# Create a unique identifier for each combination of caseid and linenumber
all_adults_analytic_svy$variables$unique_id <- paste0(all_adults_analytic_svy$variables$caseid, "_", all_adults_analytic_svy$variables$linenumber)

# Function to calculate weighted N for unique combinations
calculate_weighted_unique_n <- function(svy_design, subset = NULL) {
  if (!is.null(subset)) {
    svy_design <- subset(svy_design, eval(subset))
  }
  svytotal(~I(unique_id != ""), svy_design)
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

# Function to extract weighted N and format it
extract_weighted_n_row <- function(weighted_n_total, weighted_n_urban, weighted_n_rural, variable_name) {
  total_value <- coef(weighted_n_total)
  urban_value <- coef(weighted_n_urban)
  rural_value <- coef(weighted_n_rural)
  formatted_row <- data.frame(Variable = variable_name, Total = as.character(total_value), Urban = as.character(urban_value), Rural = as.character(rural_value))
  return(formatted_row)
}

# Function to extract the specified row and format it
extract_formatted_row <- function(df_total, df_urban, df_rural, variable_name, row_num = 1) {
  total_value <- as.character(df_total)
  urban_value <- as.character(df_urban)
  rural_value <- as.character(df_rural)
  formatted_row <- data.frame(Variable = variable_name, Total = total_value, Urban = urban_value, Rural = rural_value)
  return(formatted_row)
}

# Calculate weighted N
weighted_n_total <- calculate_weighted_unique_n(all_adults_analytic_svy)
weighted_n_urban <- calculate_weighted_unique_n(all_adults_analytic_svy, quote(residence == "Urban"))
weighted_n_rural <- calculate_weighted_unique_n(all_adults_analytic_svy, quote(residence == "Rural"))

# Perform other calculations and store results
head_age_total <- calculate_mean_ci_age(heads_all_adults_analytic_svy, "age")
head_age_urban <- calculate_mean_ci_age(heads_all_adults_analytic_svy, "age", quote(residence == "Urban"))
head_age_rural <- calculate_mean_ci_age(heads_all_adults_analytic_svy, "age", quote(residence == "Rural"))

head_female_total <- calculate_mean_ci(heads_all_adults_analytic_svy, "sex == 'Female'")
head_female_urban <- calculate_mean_ci(heads_all_adults_analytic_svy, "sex == 'Female'", quote(residence == "Urban"))
head_female_rural <- calculate_mean_ci(heads_all_adults_analytic_svy, "sex == 'Female'", quote(residence == "Rural"))

hhsize_total <- calculate_median_iqr(all_adults_analytic_svy, "nmembers")
hhsize_urban <- calculate_median_iqr(all_adults_analytic_svy, "nmembers", quote(residence == "Urban"))
hhsize_rural <- calculate_median_iqr(all_adults_analytic_svy, "nmembers", quote(residence == "Rural"))

analytic_hhsize_total <- calculate_median_iqr(all_adults_analytic_svy, "n_valid")
analytic_hhsize_urban <- calculate_median_iqr(all_adults_analytic_svy, "n_valid", quote(residence == "Urban"))
analytic_hhsize_rural <- calculate_median_iqr(all_adults_analytic_svy, "n_valid", quote(residence == "Rural"))

men_hhsize_total <- calculate_median_iqr(all_adults_analytic_svy, "n_men")
men_hhsize_urban <- calculate_median_iqr(all_adults_analytic_svy, "n_men", quote(residence == "Urban"))
men_hhsize_rural <- calculate_median_iqr(all_adults_analytic_svy, "n_men", quote(residence == "Rural"))

women_hhsize_total <- calculate_median_iqr(all_adults_analytic_svy, "n_women")
women_hhsize_urban <- calculate_median_iqr(all_adults_analytic_svy, "n_women", quote(residence == "Urban"))
women_hhsize_rural <- calculate_median_iqr(all_adults_analytic_svy, "n_women", quote(residence == "Rural"))

consanguineous_total <- calculate_mean_ci(all_adults_analytic_svy, "blood_relation == '1'")
consanguineous_urban <- calculate_mean_ci(all_adults_analytic_svy, "blood_relation == '1'", quote(residence == "Urban"))
consanguineous_rural <- calculate_mean_ci(all_adults_analytic_svy, "blood_relation == '1'", quote(residence == "Rural"))

affinal_total <- calculate_mean_ci(all_adults_analytic_svy, "blood_relation == '0'")
affinal_urban <- calculate_mean_ci(all_adults_analytic_svy, "blood_relation == '0'", quote(residence == "Urban"))
affinal_rural <- calculate_mean_ci(all_adults_analytic_svy, "blood_relation == '0'", quote(residence == "Rural"))

hypertension_total <- calculate_mean_ci(all_adults_analytic_svy, "htn_disease == '1'")
hypertension_urban <- calculate_mean_ci(all_adults_analytic_svy, "htn_disease == '1'", quote(residence == "Urban"))
hypertension_rural <- calculate_mean_ci(all_adults_analytic_svy, "htn_disease == '1'", quote(residence == "Rural"))

self_report_hypertension_total <- calculate_mean_ci(all_adults_analytic_svy, "diagnosed_bp == 1")
self_report_hypertension_urban <- calculate_mean_ci(all_adults_analytic_svy, "diagnosed_bp == 1", quote(residence == "Urban"))
self_report_hypertension_rural <- calculate_mean_ci(all_adults_analytic_svy, "diagnosed_bp == 1", quote(residence == "Rural"))

lowest_total <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 1")
lowest_urban <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 1", quote(residence == "Urban"))
lowest_rural <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 1", quote(residence == "Rural"))

low_total <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 2")
low_urban <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 2", quote(residence == "Urban"))
low_rural <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 2", quote(residence == "Rural"))

middle_total <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 3")
middle_urban <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 3", quote(residence == "Urban"))
middle_rural <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 3", quote(residence == "Rural"))

high_total <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 4")
high_urban <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 4", quote(residence == "Urban"))
high_rural <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 4", quote(residence == "Rural"))

highest_total <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 5")
highest_urban <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 5", quote(residence == "Urban"))
highest_rural <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 5", quote(residence == "Rural"))

# Combine the results into the master table
master_table <- data.frame(Variable = categories)

master_table <- rbind(
  extract_weighted_n_row(weighted_n_total, weighted_n_urban, weighted_n_rural, "N"),
  extract_formatted_row(head_age_total, head_age_urban, head_age_rural, "Age of head of household"),
  extract_formatted_row(head_female_total, head_female_urban, head_female_rural, "Female head of household (%)"),
  extract_formatted_row(hhsize_total, hhsize_urban, hhsize_rural, "Household size (median [IQR])"),
  extract_formatted_row(analytic_hhsize_total, analytic_hhsize_urban, analytic_hhsize_rural, "Household members in analytic sample (median [IQR])"),
  extract_formatted_row(men_hhsize_total, men_hhsize_urban, men_hhsize_rural, "Men (median [IQR])"),
  extract_formatted_row(women_hhsize_total, women_hhsize_urban, women_hhsize_rural, "Women (median [IQR])"),
  extract_formatted_row(consanguineous_total, consanguineous_urban, consanguineous_rural, "Consanguineous"),
  extract_formatted_row(affinal_total, affinal_urban, affinal_rural, "Affinal"),
  extract_formatted_row(hypertension_total, hypertension_urban, hypertension_rural, "Hypertension"),
  extract_formatted_row(self_report_hypertension_total, self_report_hypertension_urban, self_report_hypertension_rural, "Self-reported hypertension"),
  extract_formatted_row(lowest_total, lowest_urban, lowest_rural, "Lowest"),
  extract_formatted_row(low_total, low_urban, low_rural, "Low"),
  extract_formatted_row(middle_total, middle_urban, middle_rural, "Middle"),
  extract_formatted_row(high_total, high_urban, high_rural, "High"),
  extract_formatted_row(highest_total, highest_urban, highest_rural, "Highest")
)

# Set column names
colnames(master_table) <- c("Variable", "Total", "Urban", "Rural")

# View the master table
print(master_table)

# Optionally save the master table to a CSV file
write.csv(master_table, "paper/table_family characteristics.csv", row.names = FALSE)