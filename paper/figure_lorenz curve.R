library(ineq)

hypertension_cases <- Lc(hh_iapr$n_htn)

hypertension_cases <- unlist(hypertension_cases)

# Sort the data to simulate households from least to most cases
sorted_cases <- sort(hypertension_cases)

# Calculate the Lorenz curve
lorenz_curve <- Lc(sorted_cases)

# Plot the Lorenz curve with a title and custom colors
plot(lorenz_curve, 
     main = "",    # Add a title
     xlab = "Cumulative Proportion of Households",   # Label for the x-axis
     ylab = "Cumulative Proportion of Hypertension Cases", # Label for the y-axis
     col = "blue",         # Color of the Lorenz curve
     lwd = 2,              # Line width for the Lorenz curve
     lty = 1               # Line type for the Lorenz curve
)

# Add the line of equality (diagonal line)
abline(0, 1, col = "red", lwd = 2, lty = 2)  # Red dashed line for equality

# Add a vertical dashed line at the top 20% households (x = 0.8)
abline(v = 0.8, col = "green", lwd = 2, lty = 2)  # Green dashed vertical line at x = 0.8

# Calculate the percentage of hypertension cases in the top 20% of households
# Find the cumulative share of hypertension cases for the top 20% households
cumulative_cases <- cumsum(sorted_cases) / sum(sorted_cases)
percent_households <- seq(1, length(sorted_cases)) / length(sorted_cases)

# Extract the share of cases for the top 20% households
threshold <- 0.8  # For the top 20%, use 0.8
top_20_percent_cases <- cumulative_cases[percent_households >= threshold][1]
top_20_percent_cases_share <- 1 - top_20_percent_cases

# Print the result
cat("The top 20% of households account for approximately", 
    round(top_20_percent_cases_share * 100, 2), "% of the total hypertension cases.\n")
