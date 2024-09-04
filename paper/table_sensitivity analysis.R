# Create a binary variable for o_htn > 0
all_adults_analytic_svy$variables$n_htn_gt_1 <- ifelse(all_adults_analytic_svy$variables$o_htn > 0, TRUE, FALSE)

# Create the 2x2 table
htn_table <- table(all_adults_analytic_svy$variables$htn, all_adults_analytic_svy$variables$n_htn_gt_1)

# Sensitivity and specificity values from the source
sensitivity <- 0.61
specificity <- 0.85

# Extract counts from the 2x2 table
TP <- htn_table[2, 2]  # True positives (HTN = TRUE, n_htn_gt_1 = TRUE)
FP <- htn_table[1, 2]  # False positives (HTN = FALSE, n_htn_gt_1 = TRUE)
TN <- htn_table[1, 1]  # True negatives (HTN = FALSE, n_htn_gt_1 = FALSE)
FN <- htn_table[2, 1]  # False negatives (HTN = TRUE, n_htn_gt_1 = FALSE)

# Apply sensitivity and specificity to adjust the counts
adjusted_TP <- sensitivity * (TP + FN)
adjusted_FP <- (1 - specificity) * (TN + FP)
adjusted_FN <- (1 - sensitivity) * (TP + FN)
adjusted_TN <- specificity * (TN + FP)

# Calculate PPV, NPV, and NNS
PPV <- adjusted_TP / (adjusted_TP + adjusted_FP)
NPV <- adjusted_TN / (adjusted_TN + adjusted_FN)
NNS <- 1 / PPV

# Output PPV, NPV, and NNS
cat("Positive Predictive Value (PPV):", PPV, "\n")
cat("Negative Predictive Value (NPV):", NPV, "\n")
cat("Number Needed to Screen (NNS):", NNS, "\n")

# Assume a certain number of families are screened
n_htn_estimate <- svymean(~n_htn, all_adults_analytic_svy)
avg_htn_per_family <- 0.94542  # Average number of people with hypertension per family
undiagnosed_prevalence <- svymean(~htn, all_adults_analytic_svy) - svymean(~htn_diagnosed, all_adults_analytic_svy)
prevalence_undiagnosed <- 0.15872  # Prevalence of undiagnosed hypertension among hypertensive individuals
total_families <- 1000  # Number of families to screen

# Calculate the expected number of hypertensive individuals across all families
total_htn_individuals <- avg_htn_per_family * total_families

# Calculate the expected number of undiagnosed cases across all families
total_undiagnosed_cases <- total_htn_individuals * prevalence_undiagnosed

# Use PPV to estimate the number of true positives detected
expected_true_positives <- PPV * total_undiagnosed_cases

# Output the expected true positives among 1,000 screened families
cat("Expected true positives among", total_families, "screened families:", expected_true_positives, "\n")

# Confidence Interval for Expected True Positives
SE_true_positives <- sqrt(expected_true_positives * (1 - PPV))
CI_true_positives <- c(
  expected_true_positives - 1.96 * SE_true_positives,
  expected_true_positives + 1.96 * SE_true_positives
)

# Output the confidence interval for expected true positives
cat("95% Confidence Interval for Expected True Positives:", CI_true_positives, "\n")

# Calculate the standard errors for PPV and NPV
SE_PPV <- sqrt((PPV * (1 - PPV)) / (adjusted_TP + adjusted_FP))
SE_NPV <- sqrt((NPV * (1 - NPV)) / (adjusted_TN + adjusted_FN))

# Calculate the 95% confidence intervals for PPV and NPV
PPV_CI <- c(PPV - 1.96 * SE_PPV, PPV + 1.96 * SE_PPV)
NPV_CI <- c(NPV - 1.96 * SE_NPV, NPV + 1.96 * SE_NPV)

# Calculate NNS confidence intervals based on the PPV confidence intervals
NNS_CI <- c(1 / PPV_CI[2], 1 / PPV_CI[1])

# Output the confidence intervals for PPV, NPV, and NNS
cat("PPV:", PPV, "95% CI:", PPV_CI, "\n")
cat("NPV:", NPV, "95% CI:", NPV_CI, "\n")
cat("NNS:", NNS, "95% CI:", NNS_CI, "\n")


