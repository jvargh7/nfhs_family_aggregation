library(geepack)

# Creating a new variable for hypertension status
all_adults_analytic_svy <- all_adults_analytic_svy %>%
  mutate(htn_status = case_when(
    htn_diagnosed == 1 ~ "d",  # Diagnosed hypertension
    htn_disease == 1 & htn_diagnosed == 0 ~ "u",  # Undiagnosed hypertension
    htn_disease == 0 ~ "n"  # No hypertension
  ))

# Creating a new variable for household size category
all_adults_analytic_svy <- all_adults_analytic_svy %>%
  mutate(hh_size_cat = case_when(
    nmembers == 2 ~ "2",
    nmembers >= 3 & nmembers <= 4 ~ "3-4",
    nmembers >= 5 ~ "5"
  ))

# Fitting the model
gee_model <- geeglm(o_undiagnosedhtn ~ htn_status + hh_size_cat, 
                    id = hhid, 
                    family = poisson(link = "log"), 
                    data = all_adults_analytic_svy)

# Extract the coefficient table from the GEE model
all_coefs <- tidy(gee_model)

# Apply the transformations to compute confidence intervals and exponentiated coefficients
all_coefs <- all_coefs %>%
  mutate(Coef_CI = paste0(round(exp(estimate), 2), " \t(",
                          round(exp(estimate - 1.96 * std.error), 2), ", ",
                          round(exp(estimate + 1.96 * std.error), 2), ")"),
         PR = exp(estimate), # Exponentiated coefficient
         lci = exp(estimate - 1.96 * std.error), # Lower 95% CI
         uci = exp(estimate + 1.96 * std.error)  # Upper 95% CI
  )

# Print the modified coefficients
print(all_coefs)

# 1. Predict individual probabilities of undiagnosed hypertension
# Converting the survey object to a regular tibble temporarily for prediction
all_adults_analytic_svy <- all_adults_analytic_svy %>%
  as_tibble()  # Convert to a regular tibble for manipulation

# Step to check predicted probabilities
# Ensure that the probabilities are within a reasonable range (0 to 1)
all_adults_analytic_svy$predicted_prob <- predict(gee_model, type = "response")

# Debug: Check the distribution of predicted probabilities
summary(all_adults_analytic_svy$predicted_prob)

# 2. Calculate the probability of at least one undiagnosed case in each household
# Using the complement rule for household-level probabilities
household_probs <- all_adults_analytic_svy %>%
  group_by(hhid) %>%
  summarise(
    # Calculating the probability that at least one individual in the household has undiagnosed hypertension
    prob_at_least_one_U = 1 - prod(1 - predicted_prob, na.rm = TRUE)  # Complement rule
  )

# Debug: Check if there are unexpected duplicates or issues with `hhid`
household_counts <- household_probs %>%
  summarise(n_households = n_distinct(hhid))

print(household_counts)  # Check the number of unique households

# 3. Combine household probabilities with hypertension status and household size categories
household_probs_with_status <- all_adults_analytic_svy %>%
  select(hhid, status = htn_status, hh_size_cat, weight) %>%
  distinct() %>%  # Now this works since we're operating on a regular tibble
  left_join(household_probs, by = "hhid")

# Debug: Check for any unexpected duplications after join
print(nrow(household_probs_with_status))

# 4. Calculate the weighted count of households by status and household size category
final_table <- household_probs_with_status %>%
  group_by(status, hh_size_cat) %>%
  summarise(
    weighted_count = sum(weight, na.rm = TRUE),  # Sum of weights for households in this group
    avg_prob_at_least_one_U = mean(prob_at_least_one_U, na.rm = TRUE)  # Avg probability of having at least one U case
  )

# 5. View the final table
print(final_table)
