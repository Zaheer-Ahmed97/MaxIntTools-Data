# ============================================================================
# COMBINE POWER ANALYSIS RESULTS
# ============================================================================
# Run this AFTER all 5 power sessions complete
# ============================================================================
library(dplyr)

# Set your path
path        <- 'C:/Users/ahmed/Desktop/Statistical power simulation/'
results_dir <- paste0(path, "Power_results/")

# ============================================================================
# SESSION CONFIGURATION
# ============================================================================
data_sizes <- data.frame(
  session_num = 1:5,
  session     = paste0("session", 1:5),
  I           = c(200, 100, 300, 300, 500),
  J           = c( 50,  25,  50,  40,  40),
  true_P = 2,
  true_Q = 2
)

make_prefix <- function(session, I, J) {
  paste0("Power_", session, "_I", I, "_J", J)
}

# ============================================================================
# STEP 1 - FIND CSV FILE FOR EACH SESSION
# ============================================================================
session_csv_map <- vector("list", nrow(data_sizes))

for (i in seq_len(nrow(data_sizes))) {
  s   <- data_sizes$session[i]
  I   <- data_sizes$I[i]
  J   <- data_sizes$J[i]
  pfx <- make_prefix(s, I, J)
  
  candidates <- c(
    paste0(results_dir, pfx, "_ALL_conditions_summary.csv"),
    paste0(results_dir, pfx, "_P2_Q2_summary.csv"),
    paste0(results_dir, pfx, "_summary.csv")
  )
  
  found <- candidates[file.exists(candidates)]
  
  if (length(found) == 0) {
    cat("  [MISSING] Session", i, "- I=", I, "J=", J, "\n")
    for (cf in candidates) cat("             Looked for:", cf, "\n")
  } else {
    session_csv_map[[i]] <- found[1]
    cat("  [OK] Session", i, "- I=", I, "J=", J,
        "| Using:", basename(found[1]), "\n")
  }
}

missing <- which(sapply(session_csv_map, is.null))
if (length(missing) > 0) {
  stop(paste("ERROR: No CSV found for session(s):", paste(missing, collapse = ", ")))
}

# ============================================================================
# STEP 2 - LOAD EACH CSV AND ADD ONLY THE SESSION IDENTIFIER
# ============================================================================
all_summaries <- vector("list", nrow(data_sizes))

for (i in seq_len(nrow(data_sizes))) {
  csv_path <- session_csv_map[[i]]
  I        <- data_sizes$I[i]
  J        <- data_sizes$J[i]
  s_label  <- paste0("Session", data_sizes$session_num[i])
  
  cat("Loading Session", i, "from:", basename(csv_path), "... ")
  
  df <- tryCatch(
    read.csv(csv_path, stringsAsFactors = FALSE),
    error = function(e) stop(paste("Failed to read", csv_path, "\n  Reason:", e$message))
  )
  
  if (nrow(df) == 0) {
    warning(paste("Session", i, "CSV is empty:", csv_path))
    next
  }
  
  # Only add I, J, Session if not already present in the CSV
  if (!"I"       %in% names(df)) df$I       <- I
  if (!"J"       %in% names(df)) df$J       <- J
  if (!"Session" %in% names(df)) df$Session <- s_label
  
  all_summaries[[i]] <- df
  cat("OK (", nrow(df), "rows x", ncol(df), "cols)\n")
}

# ============================================================================
# STEP 3 - COMBINE
# ============================================================================
final_summary <- dplyr::bind_rows(all_summaries)
cat("Combined:", nrow(final_summary), "rows x", ncol(final_summary), "cols\n")

# ============================================================================
# STEP 4 - SAVE
# ============================================================================
out_file <- paste0(results_dir, "FINAL_power_summary.csv")
write.csv(final_summary, out_file, row.names = FALSE)

# ============================================================================
# STEP 5 - SUMMARY REPORT
# ============================================================================
cat("Sessions in final file:\n")
print(table(final_summary$Session))

cat("\nColumns in final file:\n")
print(names(final_summary))

cat("\nPreview (first 3 rows):\n")
print(head(final_summary, 3))

# library(dplyr)
# library(ggplot2)
# 
# # Set your path
# path <- 'C:/Users/ahmed/Desktop/Statistical power simulation/'
# 
# # Results directory
# results_dir <- paste0(path, "Power_results/")
# 
# # Data sizes
# data_sizes <- data.frame(
#   I = c(200, 100, 300, 300, 500),
#   J = c(50, 25, 50, 40, 40),
#   session = c("Session1", "Session2", "Session3", "Session4", "Session5"),
#   true_P = 2,
#   true_Q = 2
# )
# 
# cat("\nChecking for session results...\n")
# all_complete <- TRUE
# 
# for (i in 1:nrow(data_sizes)) {
#   I <- data_sizes$I[i]
#   J <- data_sizes$J[i]
#   session <- data_sizes$session[i]
#   true_P <- data_sizes$true_P[i]
#   true_Q <- data_sizes$true_Q[i]
#   
#   rds_file <- paste0(results_dir, "power_", session, "_I", I, "_J", J, 
#                      "_P", true_P, "_Q", true_Q, "_results.rds")
#   
#   if (file.exists(rds_file)) {
#     cat("  [OK] Session", i, "- I=", I, "J=", J, "\n")
#   } else {
#     cat("  [MISSING] Session", i, "- I=", I, "J=", J, "\n")
#     all_complete <- FALSE
#   }
# }
# 
# if (!all_complete) {
#   stop("\nERROR: Not all session results are available. Please wait for all sessions to complete.")
# }
# 
# cat("\n")
# cat(rep("=", 100), "\n", sep = "")
# cat("ALL SESSION RESULTS FOUND - PROCEEDING\n")
# cat(rep("=", 100), "\n", sep = "")
# 
# # Combine all results
# all_summaries <- list()
# 
# for (i in 1:nrow(data_sizes)) {
#   I <- data_sizes$I[i]
#   J <- data_sizes$J[i]
#   session <- data_sizes$session[i]
#   true_P <- data_sizes$true_P[i]
#   true_Q <- data_sizes$true_Q[i]
#   
#   cat("\nLoading results for I =", I, ", J =", J, "\n")
#   
#   rds_file <- paste0(results_dir, "power_", session, "_I", I, "_J", J, 
#                      "_P", true_P, "_Q", true_Q, "_results.rds")
#   result <- readRDS(rds_file)
#   
#   # Get summary
#   summary_df <- result$summary
#   summary_df$I <- I
#   summary_df$J <- J
#   summary_df$Session <- session
#   summary_df$True_P <- true_P
#   summary_df$True_Q <- true_Q
#   summary_df$Seed <- ifelse(!is.null(result$seed_used), result$seed_used, NA)
#   
#   all_summaries[[i]] <- summary_df
#   
#   cat("  Replications:", nrow(result$detailed_results), "\n")
#   cat("  Tests:", nrow(summary_df), "\n")
#   if (!is.null(result$seed_used)) {
#     cat("  Seed used:", result$seed_used, "(for reproducibility)\n")
#   }
# }
# 
# # Create final consolidated summary
# final_summary <- do.call(rbind, all_summaries)
# 
# # Save
# write.csv(final_summary,
#           paste0(results_dir, "FINAL_power_summary.csv"),
#           row.names = FALSE)
# 
# cat("\n")
# cat(rep("=", 100), "\n", sep = "")
# cat("FINAL POWER SUMMARY CREATED\n")
# cat(rep("=", 100), "\n", sep = "")
# 
# # Print summary by test type
# cat("\nPARAMETRIC TESTS:\n")
# parametric <- final_summary[final_summary$Test %in% c("Andersen_LR", "Martin_Lof", "LMuo", "maxLM"), ]
# print(parametric[, c("Test", "I", "J", "Statistical_Power", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)
# 
# cat("\n\nNONPARAMETRIC TESTS:\n")
# nonparametric <- final_summary[final_summary$Test %in% c("T10", "T11", "M2"), ]
# print(nonparametric[, c("Test", "I", "J", "Statistical_Power", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)
# 
# cat("\n\nREMAXINT TESTS:\n")
# remaxint <- final_summary[grepl("^REMAXINT_", final_summary$Test), ]
# print(remaxint[, c("Test", "I", "J", "Statistical_Power", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)
# 
# cat("\n\nE-REMI TESTS:\n")
# eremi <- final_summary[grepl("^E_REMI_", final_summary$Test), ]
# print(eremi[, c("Test", "I", "J", "Statistical_Power", "SE", "Lower_CI", "Upper_CI")], row.names = FALSE)
# 
# Create visualizations
cat("\n")
cat(rep("=", 100), "\n", sep = "")
cat("CREATING VISUALIZATIONS\n")
cat(rep("=", 100), "\n", sep = "")

# Target power levels
target_power <- 0.80
excellent_power <- 0.90
minimum_power <- 0.50

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
 