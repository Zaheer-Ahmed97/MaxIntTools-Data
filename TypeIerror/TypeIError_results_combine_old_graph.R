# ============================================================================
# COMBINE OPTIMIZED RESULTS
# ============================================================================
# Run this AFTER all 5 sessions complete
# ============================================================================
library(dplyr)
library(ggplot2)

# Set your path
path <- '~/Documents/Type1ErrorSimulation'

# Results directory
results_dir <- paste0(path, "TypeIError_results/")

# Data sizes
data_sizes <- data.frame(
  I = c(100, 100, 200, 500, 800),
  J = c(50, 100, 100, 80, 100),
  session = c("session1", "session2", "session3", "session4", "session5")
)

all_complete <- TRUE

for (i in 1:nrow(data_sizes)) {
  I <- data_sizes$I[i]
  J <- data_sizes$J[i]
  session <- data_sizes$session[i]
  
  rds_file <- paste0(results_dir, "_", session, "_I", I, "_J", J, "_results.rds")
  
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

# Combine all results
all_summaries <- list()

for (i in 1:nrow(data_sizes)) {
  I <- data_sizes$I[i]
  J <- data_sizes$J[i]
  session <- data_sizes$session[i]
  
  rds_file <- paste0(results_dir, "_", session, "_I", I, "_J", J, "_results.rds")
  result <- readRDS(rds_file)
  
  # Get summary
  summary_df <- result$summary
  summary_df$I <- I
  summary_df$J <- J
  summary_df$Session <- session
  summary_df$Seed <- ifelse(!is.null(result$seed_used), result$seed_used, NA)
  
  all_summaries[[i]] <- summary_df
  
  if (!is.null(result$seed_used)) {
    cat("  Seed used:", result$seed_used, "(for reproducibility)\n")
  }
}

# Create final consolidated summary
final_summary <- do.call(rbind, all_summaries)

# Save
write.csv(final_summary,
          paste0(results_dir, "FINAL_optimized_summary.csv"),
          row.names = FALSE)

# Print summary
cat("\nPARAMETRIC TESTS:\n")
parametric <- final_summary[final_summary$Test %in% c("Andersen_LR", "Martin_Lof", "LMuo", "maxLM"), ]
print(parametric[, c("Test", "I", "J", "Type1_Error", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)

cat("\n\nNONPARAMETRIC TESTS:\n")
nonparametric <- final_summary[final_summary$Test %in% c("T10", "T11", "M2"), ]
print(nonparametric[, c("Test", "I", "J", "Type1_Error", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)

cat("\n\nREMAXINT TESTS:\n")
remaxint <- final_summary[grepl("^REMAXINT_", final_summary$Test), ]
print(remaxint[, c("Test", "I", "J", "Type1_Error", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)

cat("\n\nE-REMI TESTS:\n")
eremi <- final_summary[grepl("^E_REMI_", final_summary$Test), ]
print(eremi[, c("Test", "I", "J", "Type1_Error", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)

# Create visualizations
cat("\n")
cat(rep("=", 100), "\n", sep = "")
cat("CREATING VISUALIZATIONS\n")
cat(rep("=", 100), "\n", sep = "")

alpha_level <- 0.05
n_reps <- 10
se_bound <- 2 * sqrt(alpha_level * (1 - alpha_level) / n_reps)

plot_data <- final_summary %>%
  mutate(DataSize = paste0("I=", I, ", J=", J))

# Plot 1: By test
p1 <- ggplot(plot_data, aes(x = DataSize, y = Type1_Error, 
                             color = Test, group = Test)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = alpha_level, linetype = "solid", 
             color = "red", linewidth = 1) +
  geom_hline(yintercept = alpha_level + se_bound, linetype = "dashed", 
             color = "gray50") +
  geom_hline(yintercept = alpha_level - se_bound, linetype = "dashed", 
             color = "gray50") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, alpha = 0.5) +
  facet_wrap(~ Test, scales = "free_x", ncol = 3) +
  labs(
    title = "Type-I Error Rates - Optimized Parallel Approach (1000 Reps, N=25, Perm=250)",
    subtitle = paste0("Nominal α = ", alpha_level),
    x = "Data Size",
    y = "Type-I Error Rate"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

ggsave(paste0(results_dir, "optimized_type1_by_test.png"), 
       p1, width = 16, height = 10, dpi = 300)

cat("  Plot saved: optimized_type1_by_test.png\n")

# # Performance comparison
# comparison <- final_summary %>%
#   mutate(
#     Within_Bounds = Type1_Error >= (alpha_level - se_bound) & 
#                     Type1_Error <= (alpha_level + se_bound),
#     Deviation = abs(Type1_Error - alpha_level)
#   ) %>%
#   group_by(Test) %>%
#   summarise(
#     N_DataSizes = n(),
#     Mean_Type1_Error = mean(Type1_Error, na.rm = TRUE),
#     SD_Type1_Error = sd(Type1_Error, na.rm = TRUE),
#     Mean_Deviation = mean(Deviation, na.rm = TRUE),
#     Pct_Within_Bounds = round(100 * mean(Within_Bounds, na.rm = TRUE), 1)
#   ) %>%
#   arrange(Mean_Deviation)
# 
# cat("\n")
# cat(rep("=", 100), "\n", sep = "")
# cat("PERFORMANCE COMPARISON\n")
# cat(rep("=", 100), "\n", sep = "")
# print(comparison, n = Inf)
# 
# write.csv(comparison, 
#           paste0(results_dir, "optimized_performance_comparison.csv"),
#           row.names = FALSE)