# Loading in surveys:
source("preprocessing/nfapre03_nfhs5 all adults analytic svy.R")


# Define categories
categories <- c("Age 18-39",
                "Age 40-64", 
                "Age 65 and older", 
                "Female (%)", 
                "No education",
                "Primary", 
                "Secondary", 
                "Higher", 
                "Tobacco Use", 
                "Consumes Alcohol", 
                "Urban Residence (%)", 
                "Lowest", 
                "Low", 
                "Middle", 
                "High", 
                "Highest",
                "Systolic BP",
                "Diastolic BP",
                "Self-Reported Hypertension",
                "Another member has hypertension",
                "Another member has diagnosed hypertension",
                "A consanguineal member has hypertension",
                "An affinal member has hypertension")

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

# Function to extract the specified row and format it
extract_formatted_row <- function(df_total, df_urban, df_rural, variable_name, row_num = 1) {
  total_value <- as.character(df_total)
  urban_value <- as.character(df_urban)
  rural_value <- as.character(df_rural)
  formatted_row <- data.frame(Variable = variable_name, Total = total_value, Urban = urban_value, Rural = rural_value)
  return(formatted_row)
}

# Calculations of survey means:
age_total <- calculate_mean_ci_age(all_adults_analytic_svy, "age")
age_htn <- calculate_mean_ci_age(all_adults_analytic_svy, "age", quote(htn_disease == "1"))
age_nohtn <- calculate_mean_ci_age(all_adults_analytic_svy, "age", quote(htn_disease == "0"))

age_18_39_total <- calculate_mean_ci(all_adults_analytic_svy, "age_category == '18-39'")
age_18_39_htn <- calculate_mean_ci(all_adults_analytic_svy, "age_category == '18-39'", quote(htn_disease == "1"))
age_18_39_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "age_category == '18-39'", quote(htn_disease == "0"))

age_40_64_total <- calculate_mean_ci(all_adults_analytic_svy, "age_category == '40-64'")
age_40_64_htn <- calculate_mean_ci(all_adults_analytic_svy, "age_category == '40-64'", quote(htn_disease == "1"))
age_40_64_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "age_category == '40-64'", quote(htn_disease == "0"))

age_65plus_total <- calculate_mean_ci(all_adults_analytic_svy, "age_category == '65 plus'")
age_65plus_htn <- calculate_mean_ci(all_adults_analytic_svy, "age_category == '65 plus'", quote(htn_disease == "1"))
age_65plus_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "age_category == '65 plus'", quote(htn_disease == "0"))

female_total <- calculate_mean_ci(all_adults_analytic_svy, "sex == 'Female'")
female_htn <- calculate_mean_ci(all_adults_analytic_svy, "sex == 'Female'", quote(htn_disease == "1"))
female_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "sex == 'Female'", quote(htn_disease == "0"))

noeducation_total <- calculate_mean_ci(all_adults_analytic_svy, "education == 'No education'")
noeducation_htn <- calculate_mean_ci(all_adults_analytic_svy, "education == 'No education'", quote(htn_disease == "1"))
noeducation_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "education == 'No education'", quote(htn_disease == "0"))

primary_total <- calculate_mean_ci(all_adults_analytic_svy, "education == 'Primary'")
primary_htn <- calculate_mean_ci(all_adults_analytic_svy, "education == 'Primary'", quote(htn_disease == "1"))
primary_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "education == 'Primary'", quote(htn_disease == "0"))

secondary_total <- calculate_mean_ci(all_adults_analytic_svy, "education == 'Secondary'")
secondary_htn <- calculate_mean_ci(all_adults_analytic_svy, "education == 'Secondary'", quote(htn_disease == "1"))
secondary_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "education == 'Secondary'", quote(htn_disease == "0"))

higher_total <- calculate_mean_ci(all_adults_analytic_svy, "education == 'Higher'")
higher_htn <- calculate_mean_ci(all_adults_analytic_svy, "education == 'Higher'", quote(htn_disease == "1"))
higher_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "education == 'Higher'", quote(htn_disease == "0"))

tobacco_total <- calculate_mean_ci(all_adults_analytic_svy, "smokecurr == 1")
tobacco_htn <- calculate_mean_ci(all_adults_analytic_svy, "smokecurr == 1", quote(htn_disease == 1))
tobacco_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "smokecurr == 1", quote(htn_disease == 0))

alcohol_total <- calculate_mean_ci(all_adults_analytic_svy, "alcohol == 1")
alcohol_htn <- calculate_mean_ci(all_adults_analytic_svy, "alcohol == 1", quote(htn_disease == 1))
alcohol_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "alcohol == 1", quote(htn_disease == 0))

urban_total <- calculate_mean_ci(all_adults_analytic_svy, "residence == 'Urban'")
urban_htn <- calculate_mean_ci(all_adults_analytic_svy, "residence == 'Urban'", quote(htn_disease == "1"))
urban_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "residence == 'Urban'", quote(htn_disease == "0"))

lowest_total <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 1")
lowest_htn <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 1", quote(htn_disease == "1"))
lowest_rural <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 1", quote(htn_disease == "0"))

low_total <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 2")
low_htn <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 2", quote(htn_disease == "1"))
low_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 2", quote(htn_disease == "0"))

middle_total <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 3")
middle_htn <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 3", quote(htn_disease == "1"))
middle_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 3", quote(htn_disease == "0"))

high_total <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 4")
high_htn <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 4", quote(htn_disease == "1"))
high_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 4", quote(htn_disease == "0"))

highest_total <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 5")
highest_htn <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 5", quote(htn_disease == "1"))
highest_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "swealthq_ur == 5", quote(htn_disease == "0"))

sbp_total <- calculate_mean_ci_age(all_adults_analytic_svy, "sbp")
sbp_htn <- calculate_mean_ci_age(all_adults_analytic_svy, "sbp", quote(htn_disease == "1"))
sbp_nohtn <- calculate_mean_ci_age(all_adults_analytic_svy, "sbp", quote(htn_disease == "0"))

dbp_total <- calculate_mean_ci_age(all_adults_analytic_svy, "dbp")
dbp_htn <- calculate_mean_ci_age(all_adults_analytic_svy, "dbp", quote(htn_disease == "1"))
dbp_nohtn <- calculate_mean_ci_age(all_adults_analytic_svy, "dbp", quote(htn_disease == "0"))

self_reported_total <- calculate_mean_ci(all_adults_analytic_svy, "diagnosed_bp == 1")
self_reported_htn <- calculate_mean_ci(all_adults_analytic_svy, "diagnosed_bp == 1", quote(htn_disease == "1"))
self_reported_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "diagnosed_bp == 1", quote(htn_disease == "0"))

o_htn_total <- calculate_mean_ci(all_adults_analytic_svy, "o_htn >= 1")
o_htn_htn <- calculate_mean_ci(all_adults_analytic_svy, "o_htn >= 1", quote(htn_disease == "1"))
o_htn_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "o_htn >= 1", quote(htn_disease == "0"))

o_diagnosedhtn_total <- calculate_mean_ci(all_adults_analytic_svy, "o_diagnosedhtn >= 1")
o_diagnosedhtn_htn <- calculate_mean_ci(all_adults_analytic_svy, "o_diagnosedhtn >= 1", quote(htn_disease == "1"))
o_diagnosedhtn_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "o_diagnosedhtn >= 1", quote(htn_disease == "0"))

o_htn_blood_related_total <- calculate_mean_ci(all_adults_analytic_svy, "o_htn_blood_related >= 1")
o_htn_blood_related_htn <- calculate_mean_ci(all_adults_analytic_svy, "o_htn_blood_related >= 1", quote(htn_disease == "1"))
o_htn_blood_related_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "o_htn_blood_related >= 1", quote(htn_disease == "0"))

o_htn_not_blood_related_total <- calculate_mean_ci(all_adults_analytic_svy, "o_htn_not_blood_related >= 1")
o_htn_not_blood_related_htn <- calculate_mean_ci(all_adults_analytic_svy, "o_htn_not_blood_related >= 1", quote(htn_disease == "1"))
o_htn_not_blood_related_nohtn <- calculate_mean_ci(all_adults_analytic_svy, "o_htn_not_blood_related >= 1", quote(htn_disease == "0"))

# Combine the results into the master table
master_table <- data.frame(Variable = categories)

master_table <- rbind(
  extract_formatted_row(age_18_39_total, age_18_39_htn, age_18_39_nohtn, "Age 18-39"),
  extract_formatted_row(age_40_64_total, age_40_64_htn, age_40_64_nohtn, "Age 40-64"),
  extract_formatted_row(age_65plus_total, age_65plus_htn, age_65plus_nohtn, "Age 65 and older"),
  extract_formatted_row(female_total, female_htn, female_nohtn, "Female (%)"),
  extract_formatted_row(noeducation_total, noeducation_htn, noeducation_nohtn, "No education"),
  extract_formatted_row(primary_total, primary_htn, primary_nohtn, "Primary"),
  extract_formatted_row(secondary_total, secondary_htn, secondary_nohtn, "Secondary"),
  extract_formatted_row(higher_total, higher_htn, higher_nohtn, "Higher"),
  extract_formatted_row(tobacco_total, tobacco_htn, tobacco_nohtn, "Tobacco Use"),
  extract_formatted_row(alcohol_total, alcohol_htn, alcohol_nohtn, "Consumes Alcohol"),
  extract_formatted_row(urban_total, urban_htn, urban_nohtn, "Urban Residence (%)"),
  extract_formatted_row(lowest_total, lowest_htn, lowest_rural, "Lowest"),
  extract_formatted_row(low_total, low_htn, low_nohtn, "Low"),
  extract_formatted_row(middle_total, middle_htn, middle_nohtn, "Middle"),
  extract_formatted_row(high_total, high_htn, high_nohtn, "High"),
  extract_formatted_row(highest_total, highest_htn, highest_nohtn, "Highest"),
  extract_formatted_row(sbp_total, sbp_htn, sbp_nohtn, "Systolic BP"),
  extract_formatted_row(dbp_total, dbp_htn, dbp_nohtn, "Diastolic BP"),
  extract_formatted_row(self_reported_total, self_reported_htn, self_reported_nohtn, "Self-Reported Hypertension"),
  extract_formatted_row(o_htn_total, o_htn_htn, o_htn_nohtn, "Another member has hypertension"),
  extract_formatted_row(o_diagnosedhtn_total, o_diagnosedhtn_htn, o_diagnosedhtn_nohtn, "Another member has diagnosed hypertension"),
  extract_formatted_row(o_htn_blood_related_total, o_htn_blood_related_htn, o_htn_blood_related_nohtn, "A consanguineal member has hypertension"),
  extract_formatted_row(o_htn_not_blood_related_total, o_htn_not_blood_related_htn, o_htn_not_blood_related_nohtn, "An affinal member has hypertension")
)

# Ensure the entire data frame is character strings
master_table <- master_table %>%
  mutate(across(everything(), as.character))

# Set column names
colnames(master_table) <- c("Variable", "Total", "Hypertension", "No Hypertension")

# View the master table
print(master_table)

# Optionally save the master table to a CSV file
write.csv(master_table, "paper/table_individual characteristics.csv", row.names = FALSE)

