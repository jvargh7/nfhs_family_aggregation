rm(list=ls()); gc(); source(".Rprofile")

source("preprocessing/nfapre02_creating individual and household datasets.R")

# Creating a function to determine the unweighted percentage of each relationship type in the analytic sample:
relationship_percentage <- function(variable, category) {
  # Calculate the proportion
  n <- length(variable)
  proportion <- sum(variable == category) / n
  
  # Convert proportion to percentage
  percentage <- proportion * 100
  
  # Calculate standard error (SE) for the proportion
  standard_error <- sqrt(proportion * (1 - proportion) / n)
  
  # Calculate 95% CI (using Z = 1.96 for 95% confidence level)
  ci_lower <- (proportion - 1.96 * standard_error) * 100
  ci_upper <- (proportion + 1.96 * standard_error) * 100
  
  # Return the percentage and CI
  return(list(percentage = percentage, CI_lower = ci_lower, CI_upper = ci_upper))
}

# Using the function to determine the types of relationship in each household:
hh_head <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Head")

hh_wife_or_husband <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Wife or husband")

hh_son_or_daughter <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Son/daughter")

hh_son_or_daughter_in_law <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Son/daughter in law")

hh_grandchild <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Grandchild")

hh_parent <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Parent")

hh_parent_in_law <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Parent-In-Law")

hh_brother_or_sister <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Brother/sister")

hh_co_spouse <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Co-spouse")

hh_other_relative <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Other relative")

hh_adopted_or_foster_child <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Adopted/foster child")

hh_not_related <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Not related")

hh_niece_or_nephew_by_blood <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Niece/nephew by blood")

hh_niece_or_nephew_by_marriage <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Niece/nephew by marriage")

hh_brother_in_law_or_sister_in_law <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Brother-in-law or sister-in-law")

hh_niece_or_nephew <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Niece/nephew")

hh_domestic_servant <- relationship_percentage(all_adults_analytic_sample$relationship_hh_head, "Domestic servant")
