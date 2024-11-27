rm(list=ls()); gc(); source(".Rprofile")

# source("analysis/nfaan06_probability of undiagnosed in hh by individual status.R")

# Call in values from .csv file:
undiagnosed_htn_predictions <- read.csv("analysis/nfaan06_individuals counts with predicted probability.csv")

# Create the plot with rounded n values and labels above error bars
k <- ggplot(undiagnosed_htn_predictions, aes(x = factor(htn_status, levels = c("n", "d", "u")), y = response, fill = valid_size_cat)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) + 
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), 
                position = position_dodge(width = 0.8), 
                width = 0.2) +
  geom_text(aes(label = round(p*100), y = upper_CI + 0.05),  # Round n and place labels above error bars
            position = position_dodge(width = 0.8), 
            vjust = 1.5, size = 4) +  # Adjust vjust as needed to fine-tune position
  labs(x = "Individual Hypertension Status", y = "Predicted Probability", fill = "Household Size") +  # Set new legend title
  theme_minimal() +
  scale_fill_manual(
    values = c("#E76F51", "#F4A261", "#81B29A"),
    labels = c("2", "3-4", "≥5")   # Custom labels for the legend
  ) +
  scale_x_discrete(
    labels = c("n" = "None", "d" = "Diagnosed", "u" = "Undiagnosed")  # Set custom labels for x-axis
  ) +
  theme(
    axis.text.x = element_text(size = 12, hjust = 0.5),  # Adjust font size for x-axis labels
    axis.text.y = element_text(size = 12),               # Adjust font size for y-axis labels
    axis.title.x = element_text(size = 14, margin = margin(t = 15)),  # Increase font size and separation for x-axis title
    axis.title.y = element_text(size = 14),              # Increase font size for y-axis title
    legend.position = "bottom",
    legend.title = element_text(vjust = 1, size = 12)
  )

# Saving the plot:
ggsave(paste0(path_family_aggregation_folder,"/figures/undiagnosed_htn_predictions.png"), plot = k, width = 10, height = 6)




