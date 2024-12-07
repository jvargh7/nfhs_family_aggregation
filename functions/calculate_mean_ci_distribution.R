calculate_mean_ci_distribution <- function(svy_design, variable, subset_expr = NULL) {
  if (!is.null(subset_expr)) {
    svy_design <- subset(svy_design, eval(subset_expr))
  }
  mean_result <- svymean(as.formula(paste("~", variable)), svy_design)
  ci_result <- confint(mean_result)
  estimate <- coef(mean_result)[1] * 100  # Convert to percentage
  lower_ci <- ci_result[1, 1] * 100  # Convert to percentage
  upper_ci <- ci_result[1, 2] * 100  # Convert to percentage
  
  # Correctly compute the reversed intervals
  result <- list(
    estimate = 100 - estimate,
    lower_ci = 100 - upper_ci,  # Reverse the upper bound for lower_ci
    upper_ci = 100 - lower_ci   # Reverse the lower bound for upper_ci
  )
  return(result)
}
