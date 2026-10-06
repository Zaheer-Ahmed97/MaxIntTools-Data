# ============================================================
# STEP 9: ARDL BOUNDS TESTING -- ALL THREE TRANSFORMATIONS
# ============================================================
cat("\n\n=== STEP 9: ARDL BOUNDS TESTING (ALL TRANSFORMATIONS) ===\n")

EXOG_VARS <- c("WHEAT","FUEL","ENERGY","GAS")

run_ardl_bounds <- function(df, label) {
  cat("\n", rep("=", 65), "\n", "ARDL BOUNDS TEST --", toupper(label), "\n", rep("=", 65), "\n", sep = "")
  
  ardl_data <- as.data.frame(df[, c("date","QIM","WHEAT","FUEL","ENERGY","GAS")])
  ardl_data <- ardl_data[complete.cases(ardl_data), ]
  
  ardl_search <- auto_ardl(QIM ~ WHEAT + FUEL + ENERGY + GAS,
                           data = ardl_data, max_order = 6)
  best_order <- ardl_search$best_order
  cat("Selected ARDL order (QIM, WHEAT, FUEL, ENERGY, GAS):", best_order, "\n")
  
  ardl_model <- ardl_search$best_model
  cat("\nARDL model summary:\n"); print(summary(ardl_model))
  
  bounds_test <- bounds_f_test(ardl_model, case = 3)
  cat("\nBounds test:\n"); print(bounds_test)
  
  k_regressors <- length(attr(terms(QIM ~ WHEAT + FUEL + ENERGY + GAS), "term.labels"))
  
  uecm_model <- uecm(ardl_model)
  lr_coeffs  <- multipliers(ardl_model, type = "lr")
  cat("\nLong-run coefficients:\n"); print(lr_coeffs)
  
  recm_model <- recm(uecm_model, case = 3)
  recm_coefs <- summary(recm_model)$coefficients
  ect_row <- recm_coefs[grepl("ect", rownames(recm_coefs), ignore.case = TRUE), , drop = FALSE]
  rho <- if (nrow(ect_row) == 1) ect_row[1, "Estimate"] else NA
  half_life <- if (!is.na(rho) && rho > -1) log(0.5) / log(1 + rho) else NA
  cat("\nError correction term:\n"); print(ect_row)
  cat("Half-life:", round(half_life, 2), "months\n")
  
  # Save everything needed for later steps
  saveRDS(list(ardl_model = ardl_model, uecm_model = uecm_model,
               recm_model = recm_model, best_order = best_order,
               ardl_data = ardl_data, lr_coeffs = lr_coeffs,
               ect_row = ect_row, half_life = half_life,
               bounds_test = bounds_test, k = k_regressors),
          file.path(outputs_dir, paste0("ardl_fit_", label, ".rds")))
  
  write.csv(lr_coeffs, file.path(outputs_dir, paste0("longrun_coeffs_", label, ".csv")), row.names = FALSE)
  
  list(F_stat = as.numeric(bounds_test$statistic),
       p_value = as.numeric(bounds_test$p.value),
       k = k_regressors, best_order = best_order,
       rho = rho, half_life = half_life)
}

res_level <- run_ardl_bounds(merged,     "level")
res_log   <- run_ardl_bounds(merged_log, "log")
res_bc    <- run_ardl_bounds(merged_bc,  "boxcox")

bounds_comparison <- data.frame(
  Transformation = c("level","log","boxcox"),
  F_statistic = c(res_level$F_stat, res_log$F_stat, res_bc$F_stat),
  p_value     = c(res_level$p_value, res_log$p_value, res_bc$p_value),
  k = c(res_level$k, res_log$k, res_bc$k),
  ECT = round(c(res_level$rho, res_log$rho, res_bc$rho), 4),
  Half_life_months = round(c(res_level$half_life, res_log$half_life, res_bc$half_life), 2)
)
cat("\n\n=== BOUNDS TEST COMPARISON (ALL TRANSFORMATIONS) ===\n")
print(bounds_comparison)
write.csv(bounds_comparison, file.path(outputs_dir, "bounds_test_comparison.csv"), row.names = FALSE)

cat("\nReference critical values (Pesaran/Shin/Smith 2001, Case III, k=4):\n")
cat("  10%: I(0)=2.45, I(1)=3.52\n")
cat("   5%: I(0)=2.86, I(1)=4.01\n")
cat("   1%: I(0)=3.74, I(1)=5.06\n")
cat("Compare each F_statistic above against these bounds to confirm the\n",
    "significance level at which cointegration is confirmed.\n")

cat("\n>>> PART 2 COMPLETE. Proceed to Part 3 (diagnostics, BDS, NARDL, ARCH-LM).\n")
