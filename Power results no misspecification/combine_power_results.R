# ============================================================================
# COMBINE POWER ANALYSIS RESULTS
# ============================================================================
# Run this AFTER all 4 power sessions complete
# ============================================================================
library(dplyr)
library(ggplot2)

# Set your path
path <- 'C:/Users/ahmed/Desktop/R codes for fourth project based on PhD thesis/'

# Results directory
results_dir <- paste0(path, "power_results/")

# Data sizes
data_sizes <- data.frame(
  I = c(100, 300, 500, 1000),
  J = c(25, 50, 80, 100),
  session = c("session1", "session2", "session3", "session4"),
  true_P = 2,
  true_Q = 2
)

cat("\nChecking for session results...\n")
all_complete <- TRUE

for (i in 1:nrow(data_sizes)) {
  I <- data_sizes$I[i]
  J <- data_sizes$J[i]
  session <- data_sizes$session[i]
  true_P <- data_sizes$true_P[i]
  true_Q <- data_sizes$true_Q[i]
  
  rds_file <- paste0(results_dir, "power_", session, "_I", I, "_J", J, 
                     "_P", true_P, "_Q", true_Q, "_results.rds")
  
  if (file.exists(rds_file)) {
    cat("  [OK] Session", i, "- I=", I, "J=", J, "\n")
  } else {
    cat("  [MISSING] Session", i, "- I=", I, "J=", J, "\n")
    all_complete <- FALSE
  }
}

if (!all_complete) {
  stop("\nERROR: Not all session results are available. Please wait for all sessions to complete.")
}

cat("\n")
cat(rep("=", 100), "\n", sep = "")
cat("ALL SESSION RESULTS FOUND - PROCEEDING\n")
cat(rep("=", 100), "\n", sep = "")

# Combine all results
all_summaries <- list()

for (i in 1:nrow(data_sizes)) {
  I <- data_sizes$I[i]
  J <- data_sizes$J[i]
  session <- data_sizes$session[i]
  true_P <- data_sizes$true_P[i]
  true_Q <- data_sizes$true_Q[i]
  
  cat("\nLoading results for I =", I, ", J =", J, "\n")
  
  rds_file <- paste0(results_dir, "power_", session, "_I", I, "_J", J, 
                     "_P", true_P, "_Q", true_Q, "_results.rds")
  result <- readRDS(rds_file)
  
  # Get summary
  summary_df <- result$summary
  summary_df$I <- I
  summary_df$J <- J
  summary_df$Session <- session
  summary_df$True_P <- true_P
  summary_df$True_Q <- true_Q
  summary_df$Seed <- ifelse(!is.null(result$seed_used), result$seed_used, NA)
  
  all_summaries[[i]] <- summary_df
  
  cat("  Replications:", nrow(result$detailed_results), "\n")
  cat("  Tests:", nrow(summary_df), "\n")
  if (!is.null(result$seed_used)) {
    cat("  Seed used:", result$seed_used, "(for reproducibility)\n")
  }
}

# Create final consolidated summary
final_summary <- do.call(rbind, all_summaries)

# Save
write.csv(final_summary,
          paste0(results_dir, "FINAL_power_summary.csv"),
          row.names = FALSE)

cat("\n")
cat(rep("=", 100), "\n", sep = "")
cat("FINAL POWER SUMMARY CREATED\n")
cat(rep("=", 100), "\n", sep = "")

# Print summary by test type
cat("\nPARAMETRIC TESTS:\n")
parametric <- final_summary[final_summary$Test %in% c("Andersen_LR", "Martin_Lof", "LMuo", "maxLM"), ]
print(parametric[, c("Test", "I", "J", "Statistical_Power", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)

cat("\n\nNONPARAMETRIC TESTS:\n")
nonparametric <- final_summary[final_summary$Test %in% c("T10", "T11", "M2"), ]
print(nonparametric[, c("Test", "I", "J", "Statistical_Power", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)

cat("\n\nREMAXINT TESTS:\n")
remaxint <- final_summary[grepl("^REMAXINT_", final_summary$Test), ]
print(remaxint[, c("Test", "I", "J", "Statistical_Power", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)

cat("\n\nE-REMI TESTS:\n")
eremi <- final_summary[grepl("^E_REMI_", final_summary$Test), ]
print(eremi[, c("Test", "I", "J", "Statistical_Power", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)

# Create visualizations
cat("\n")
cat(rep("=", 100), "\n", sep = "")
cat("CREATING VISUALIZATIONS\n")
cat(rep("=", 100), "\n", sep = "")

# Target power levels
target_power <- 0.80
excellent_power <- 0.90
minimum_power <- 0.70

plot_data <- final_summary %>%
  mutate(
    DataSize = paste0("I=", I, ", J=", J),
    power_adequate = ifelse(Statistical_Power >= target_power,
                            "Adequate (≥0.80)", "Inadequate (<0.80)")
  )

# Plot 1: Power by test across data sizes
p1 <- ggplot(plot_data, aes(x = DataSize, y = Statistical_Power, 
                             color = Test, group = Test)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = target_power, linetype = "solid", 
             color = "darkgreen", linewidth = 1, alpha = 0.7) +
  geom_hline(yintercept = excellent_power, linetype = "dotted", 
             color = "darkgreen", linewidth = 0.6, alpha = 0.5) +
  geom_hline(yintercept = minimum_power, linetype = "dashed", 
             color = "orange", linewidth = 0.6, alpha = 0.5) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, alpha = 0.5) +
  facet_wrap(~ Test, scales = "free_x", ncol = 3) +
  labs(
    title = "Statistical Power Analysis - TRUE P=2, Q=2 Structure (1000 Reps, N=25, Perm=250)",
    subtitle = "Target Power = 0.80 (green solid) | Excellent = 0.90 (dotted) | Minimum = 0.70 (dashed)",
    x = "Data Size",
    y = "Statistical Power (Rejection Rate)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(size = 13, face = "bold"),
    strip.text = element_text(size = 9, face = "bold")
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.2),
    limits = c(0, 1)
  )

ggsave(paste0(results_dir, "power_by_test.png"), 
       p1, width = 16, height = 10, dpi = 300)

cat("  Plot saved: power_by_test.png\n")

# Plot 2: Power by data size
p2 <- ggplot(plot_data, aes(x = Test, y = Statistical_Power, fill = DataSize)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = target_power, linetype = "solid", 
             color = "darkgreen", linewidth = 1, alpha = 0.7) +
  geom_hline(yintercept = excellent_power, linetype = "dotted", 
             color = "darkgreen", linewidth = 0.6, alpha = 0.5) +
  geom_hline(yintercept = minimum_power, linetype = "dashed", 
             color = "orange", linewidth = 0.6, alpha = 0.5) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                position = position_dodge(0.9), width = 0.2) +
  labs(
    title = "Statistical Power by Test and Data Size",
    subtitle = "TRUE Structure: P=2, Q=2",
    x = "Test",
    y = "Statistical Power",
    fill = "Data Size"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.2)
  )

ggsave(paste0(results_dir, "power_by_datasize.png"), 
       p2, width = 16, height = 10, dpi = 300)

cat("  Plot saved: power_by_datasize.png\n")

# Performance comparison
comparison <- final_summary %>%
  mutate(
    Within_Bounds = Statistical_Power >= target_power,
    Excellent = Statistical_Power >= excellent_power,
    Deviation_from_target = abs(Statistical_Power - target_power)
  ) %>%
  group_by(Test) %>%
  summarise(
    N_DataSizes = n(),
    Mean_Power = mean(Statistical_Power, na.rm = TRUE),
    Min_Power = min(Statistical_Power, na.rm = TRUE),
    Max_Power = max(Statistical_Power, na.rm = TRUE),
    SD_Power = sd(Statistical_Power, na.rm = TRUE),
    N_Adequate = sum(Within_Bounds, na.rm = TRUE),
    N_Excellent = sum(Excellent, na.rm = TRUE),
    Pct_Adequate = round(100 * mean(Within_Bounds, na.rm = TRUE), 1)
  ) %>%
  arrange(desc(Mean_Power))

cat("\n")
cat(rep("=", 100), "\n", sep = "")
cat("PERFORMANCE COMPARISON\n")
cat(rep("=", 100), "\n", sep = "")
print(comparison, n = Inf)

write.csv(comparison, 
          paste0(results_dir, "power_performance_comparison.csv"),
          row.names = FALSE)

# Identify best performers
cat("\n")
cat("BEST PERFORMERS (Highest Mean Power):\n")
cat(rep("-", 100), "\n", sep = "")
top_tests <- head(comparison, 5)
print(top_tests[, c("Test", "Mean_Power", "Min_Power", "Max_Power")], row.names = FALSE)

cat("\n")
cat("TESTS WITH ADEQUATE POWER ACROSS ALL DATA SIZES:\n")
cat(rep("-", 100), "\n", sep = "")
adequate_all <- comparison[comparison$N_Adequate == 4, ]
if (nrow(adequate_all) > 0) {
  print(adequate_all[, c("Test", "Mean_Power")], row.names = FALSE)
} else {
  cat("No tests achieved adequate power across all data sizes.\n")
}