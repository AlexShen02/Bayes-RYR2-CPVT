# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Read the initial CSV file
df <- read.csv("d_full_AM_REVEL_ClinVar_againstALL.csv", stringsAsFactors = FALSE)

# Remove rows where 'total_carriers' is 0 or NA
df_cleaned <- df %>%
  filter(!is.na(total_carriers) & total_carriers != 0)

# Define residue ranges
ranges <- list(
  "1-3633" = 1:3633,
  "1-639" = 1:639,
  "1-219" = 1:219,
  "220-408" = 220:408,
  "409-639" = 409:639,
  "640-1646" = 640:1646,
  "640-861,1463-1483,1595-1646" = c(640:861, 1463:1483, 1595:1646),
  "1076-1255" = 1076:1255,
  "1256-1462,1484-1594" = c(1256:1462, 1484:1594),
  "862-1076" = 862:1076,
  "1647-2108" = 1647:2108,
  "2109-3564" = 2109:3564,
  "2109-2681,2916-3042" = c(2109:2681, 2916:3042),
  "3042-3344" = 3042:3344,
  "3345-3564" = 3345:3564,
  "2682-2915" = 2682:2915,
  "3565-3633" = 3565:3633,
  "3634-4967" = 3634:4967,
  "3634-4130" = 3634:4130,
  "4016-4090" = 4016:4090,
  "4131-4209" = 4131:4209,
  "4237-4886" = 4237:4886,
  "4237-4310" = 4237:4310,
  "4480-4750" = 4480:4750,
  "4751-4886" = 4751:4886,
  "4887-4967" = 4887:4967,
  "4887-4914" = 4887:4914
)

# Function to calculate the mean and sd for each range
calculate_stats_for_range <- function(range, data) {
  df_filtered <- data %>%
    filter(resnum %in% range) %>%
    summarise(
      avg_penetrance_cpvt = mean(penetrance_cpvt, na.rm = TRUE),
      sd_penetrance_cpvt = sd(penetrance_cpvt, na.rm = TRUE),
      avg_cpvt_penetranceBayesian_initial = mean(cpvt_penetranceBayesian_initial, na.rm = TRUE),
      sd_cpvt_penetranceBayesian_initial = sd(cpvt_penetranceBayesian_initial, na.rm = TRUE),
      avg_cpvt_penetranceBayesian = mean(cpvt_penetranceBayesian, na.rm = TRUE),
      sd_cpvt_penetranceBayesian = sd(cpvt_penetranceBayesian, na.rm = TRUE),
      avg_cpvt_dist = mean(cpvt_dist, na.rm = TRUE),
      sd_cpvt_dist = sd(cpvt_dist, na.rm = TRUE)
    )
  return(df_filtered)
}

# Apply function to all ranges and store the results in a list
range_results <- lapply(names(ranges), function(range_name) {
  result <- calculate_stats_for_range(ranges[[range_name]], df_cleaned)
  result$Range <- range_name
  return(result)
})

# Combine the results into a single data frame
result_df <- bind_rows(range_results)

# Save the results to a new CSV file
write.csv(result_df, "resultsv1.csv", row.names = FALSE)

# Reshape the data for better visualization
result_long <- result_df %>%
  pivot_longer(cols = starts_with("avg_"),
               names_to = "Statistic",
               values_to = "Average") %>%
  mutate(
    SD = case_when(
      Statistic == "avg_penetrance_cpvt" ~ result_df$sd_penetrance_cpvt[rep(1:nrow(result_df), each = 4)],
      Statistic == "avg_cpvt_penetranceBayesian_initial" ~ result_df$sd_cpvt_penetranceBayesian_initial[rep(1:nrow(result_df), each = 4)],
      Statistic == "avg_cpvt_penetranceBayesian" ~ result_df$sd_cpvt_penetranceBayesian[rep(1:nrow(result_df), each = 4)],
      Statistic == "avg_cpvt_dist" ~ result_df$sd_cpvt_dist[rep(1:nrow(result_df), each = 4)]
    )
  )

# Plot for all 4 statistics with error bars
ggplot(result_long, aes(x = Range, y = Average, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Average - SD, ymax = Average + SD),
                position = position_dodge(width = 0.7), width = 0.2) +
  theme_minimal() +
  labs(
    title = "Average and Standard Deviation of CPVT Statistics by Residue Range",
    x = "Residue Range",
    y = "Average Value",
    fill = "Statistic"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c(
    "avg_penetrance_cpvt" = "steelblue", 
    "avg_cpvt_penetranceBayesian_initial" = "lightgreen", 
    "avg_cpvt_penetranceBayesian" = "lightcoral",
    "avg_cpvt_dist" = "salmon"
  ))

# Create individual plots for each statistic

# Plot for penetrance_cpvt
plot_penetrance_cpvt <- ggplot(subset(result_long, Statistic == "avg_penetrance_cpvt"), aes(x = Range, y = Average, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Average - SD, ymax = Average + SD),
                position = position_dodge(width = 0.7), width = 0.2) +
  theme_minimal() +
  labs(
    title = "Average and Standard Deviation of Penetrance CPVT by Residue Range",
    x = "Residue Range",
    y = "Average Value",
    fill = "Statistic"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("avg_penetrance_cpvt" = "steelblue"))

# Plot for cpvt_penetranceBayesian_initial
plot_cpvt_penetranceBayesian_initial <- ggplot(subset(result_long, Statistic == "avg_cpvt_penetranceBayesian_initial"), aes(x = Range, y = Average, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Average - SD, ymax = Average + SD),
                position = position_dodge(width = 0.7), width = 0.2) +
  theme_minimal() +
  labs(
    title = "Average and Standard Deviation of CPVT Penetrance Bayesian (Initial) by Residue Range",
    x = "Residue Range",
    y = "Average Value",
    fill = "Statistic"
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_fill_manual(values = c("avg_cpvt_penetranceBayesian_initial" = "lightgreen"))

# Plot for cpvt_penetranceBayesian
plot_cpvt_penetranceBayesian <- ggplot(subset(result_long, Statistic == "avg_cpvt_penetranceBayesian"), aes(x = Range, y = Average, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Average - SD, ymax = Average + SD),
                position = position_dodge(width = 0.7), width = 0.2) +
  theme_minimal() +
  labs(
    title = "Average and Standard Deviation of CPVT Penetrance Bayesian by Residue Range",
    x = "Residue Range",
    y = "Average Value",
    fill = "Statistic"
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_fill_manual(values = c("avg_cpvt_penetranceBayesian" = "lightcoral"))

# Plot for cpvt_dist
plot_cpvt_dist <- ggplot(subset(result_long, Statistic == "avg_cpvt_dist"), aes(x = Range, y = Average, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge", width
           