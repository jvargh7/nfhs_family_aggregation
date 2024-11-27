# Function to calculate mean and confidence intervals for a variable
calculate_mean_ci_distribution <- function(svy_design, variable, subset_expr = NULL) {
  if (!is.null(subset_expr)) {
    svy_design <- subset(svy_design, eval(subset_expr))
  }
  mean_result <- svymean(as.formula(paste("~", variable)), svy_design)
  ci_result <- confint(mean_result)
  estimate <- coef(mean_result)[1] * 100  # Convert to percentage
  lower_ci <- ci_result[1, 1] * 100  # Convert to percentage
  upper_ci <- ci_result[1, 2] * 100  # Convert to percentage
  
  result <- list(estimate = 100 - estimate, 
                 lower_ci = 100 - lower_ci, 
                 upper_ci = 100 - upper_ci)
  return(result)
}