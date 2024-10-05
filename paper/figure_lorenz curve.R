library(ineq)

hypertension_cases <- Lc(hh_iapr$n_htn)
diagnosed_hypertension_cases <- Lc(hh_iapr$n_diagnosedhtn)
undiagnosed_hypertension_cases <- Lc(hh_iapr$n_undiagnosedhtn)

hypertension_cases <- unlist(hypertension_cases)
diagnosed_hypertension_cases <- unlist(diagnosed_hypertension_cases)
undiagnosed_hypertension_cases <- unlist(undiagnosed_hypertension_cases)

# Sort the data to simulate households from least to most cases
sorted_htn_cases <- sort(hypertension_cases)
sorted_diaghtn_cases <- sort(diagnosed_hypertension_cases)
sorted_undiaghtn_cases <- sort(undiagnosed_hypertension_cases)

# Calculate the Lorenz curve
htn_lorenz_curve <- Lc(sorted_htn_cases)
diaghtn_lorenz_curve <- Lc(sorted_diaghtn_cases)
undiaghtn_lorenz_curve <- Lc(sorted_undiaghtn_cases)

# Set up a 1x3 grid of panels (1 row, 3 columns)
par(mfrow = c(1, 3))

# Plot the Lorenz curves on separate panels with larger text
plot(htn_lorenz_curve, main = "Total", xlab = "Cumulative Proportion of Households", 
     ylab = "Cumulative Proportion of Cases", col = "blue", lwd = 2, 
     cex.main = 3, cex.lab = 1.5, cex.axis = 1.2)  # Increase text sizes with cex
abline(v = 0.8, col = "black", lty = 2, lwd = 2)  # Add vertical line at x = 0.8

plot(diaghtn_lorenz_curve, main = "Diagnosed", xlab = "Cumulative Proportion of Households", 
     ylab = "Cumulative Proportion of Cases", col = "red", lwd = 2, 
     cex.main = 3, cex.lab = 1.5, cex.axis = 1.2)  # Increase text sizes
abline(v = 0.8, col = "black", lty = 2, lwd = 2)

plot(undiaghtn_lorenz_curve, main = "Undiagnosed", xlab = "Cumulative Proportion of Households", 
     ylab = "Cumulative Proportion of Cases", col = "green", lwd = 2, 
     cex.main = 3, cex.lab = 1.5, cex.axis = 1.2)  # Increase text sizes
abline(v = 0.8, col = "black", lty = 2, lwd = 2)

# Reset plotting parameters back to default
par(mfrow = c(1, 1))

# Now save the plot to a PNG file using dev.copy()
dev.copy(png, file = "paper/lorenz_curves.png", width = 1800, height = 600)
dev.off()  # Close the device to save the file

# Calculate the percentage of hypertension cases in the top 20% of households
# Find the cumulative share of hypertension cases for the top 20% households
cumulative_htn_cases <- cumsum(sorted_htn_cases) / sum(sorted_htn_cases)
percent_htn_households <- seq(1, length(sorted_htn_cases)) / length(sorted_htn_cases)

cumulative_diaghtn_cases <- cumsum(sorted_diaghtn_cases) / sum(sorted_diaghtn_cases)
percent_diaghtn_households <- seq(1, length(sorted_diaghtn_cases)) / length(sorted_diaghtn_cases)

cumulative_undiaghtn_cases <- cumsum(sorted_undiaghtn_cases) / sum(sorted_undiaghtn_cases)
percent_undiaghtn_households <- seq(1, length(sorted_undiaghtn_cases)) / length(sorted_undiaghtn_cases)

# Extract the share of cases for the top 20% households
threshold <- 0.8  # For the top 20%, use 0.8
top_20_percent_htn_cases <- cumulative_htn_cases[percent_htn_households >= threshold][1]
top_20_percent_htn_cases_share <- 1 - top_20_percent_htn_cases

top_20_percent_diaghtn_cases <- cumulative_diaghtn_cases[percent_diaghtn_households >= threshold][1]
top_20_percent_diaghtn_cases_share <- 1 - top_20_percent_diaghtn_cases

top_20_percent_undiaghtn_cases <- cumulative_undiaghtn_cases[percent_undiaghtn_households >= threshold][1]
top_20_percent_undiaghtn_cases_share <- 1 - top_20_percent_undiaghtn_cases

# Print the result
cat("The top 20% of households account for approximately", 
    round(top_20_percent_htn_cases_share * 100, 2), "% of the total hypertension cases.\n")

cat("The top 20% of households account for approximately", 
    round(top_20_percent_diaghtn_cases_share * 100, 2), "% of the total hypertension cases.\n")

cat("The top 20% of households account for approximately", 
    round(top_20_percent_undiaghtn_cases_share * 100, 2), "% of the total hypertension cases.\n")
