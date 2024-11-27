rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

# Loading in the dataset:
individual_counts <- read_csv("analysis/nfaan06_individuals counts with predicted probability.csv")

# Calculating the percentage of each household size within diagnosis status:
individual_counts <- individual_counts %>%
  group_by(htn_status) %>%
  mutate(total_wt_sum = sum(wt_sum, na.rm = TRUE)) %>%
  mutate(percentage_wt_sum = (wt_sum / total_wt_sum) * 100) %>%
  ungroup()

# Adding the actual number needed to screen values:
individual_counts <- individual_counts %>%
  mutate(
    nns = 1 / response,                # 1 divided by response
    nns_lci = 1 / upper_CI,            # 1 divided by upper_ci
    nns_uci = 1 / lower_CI             # 1 divided by lower_ci
  )

# Saving the updated .csv file:
write_csv(individual_counts,"paper/family size proportions and nns.csv")

# Creating the sideways box plot
sideways_barplot <- ggplot(individual_counts, aes(
  y = factor(htn_status, levels = c("n", "u", "d")), # Manually order htn_status
  x = nns, 
  fill = factor(valid_size_cat, levels = c("5", "3-4", "2")) # Manually order valid_size_cat
)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", linewidth = 1, width = 0.6) + # Bars with adjusted dodge width
  geom_errorbar(aes(xmin = nns_lci, xmax = nns_uci), 
                position = position_dodge(width = 0.8), width = 0.2) + # Error bars with matching dodge width
  scale_fill_manual(values = c("#E76F51", "#81B29A", "#F8C291"),
                    breaks = c("2", "3-4", "5")) + # Custom colors
  theme_minimal() + # Minimal theme
  theme(
    axis.text.y = element_blank(), # Hide y-axis text labels
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    axis.text.x = element_text(size = 28, face = "bold"), # Make x-axis text bigger
    legend.title = element_text(size = 24), # Make legend title larger
    legend.text = element_text(size = 24), # Make legend text larger
    panel.grid.major = element_blank(), # Remove grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    axis.title = element_blank() # Remove axis titles
  ) +
  labs(fill = "Household Size") # Legend title


# Saving the plot:
ggsave(sideways_barplot,filename=paste0(path_family_aggregation_folder,"/figures/number needed to screen barplot.png"),width = 8, height = 10)
