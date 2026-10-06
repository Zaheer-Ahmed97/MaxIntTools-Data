# =============================================================================
# PART 3: DIAGNOSTIC CHECK  (corrected to align with Part 1 & Part 2)
# =============================================================================
path        <- "C:/Users/ahmed/Desktop/IfADo Projects/"
outputs_dir <- file.path(path, "outputs")
plots_dir   <- file.path(path, "plots")

# Verify Part 1 & 2 outputs exist before proceeding
required_files <- c(
  file.path(outputs_dir, "clean_merged_data.csv"),
  file.path(outputs_dir, "merged_log.csv"),
  file.path(outputs_dir, "merged_boxcox.csv"),
  file.path(outputs_dir, "boxcox_lambdas.rds"),
  file.path(outputs_dir, "ardl_fit_level.rds"),
  file.path(outputs_dir, "ardl_fit_log.rds"),
  file.path(outputs_dir, "ardl_fit_boxcox.rds")
)
missing <- required_files[!file.exists(required_files)]
if (length(missing) > 0) {
  stop("The following files from Part 1 / Part 2 are missing:\n",
       paste(" ", missing, collapse = "\n"),
       "\nPlease run Part_1.R and Part2_ARDL.R first.")
}
cat("All Part 1 / Part 2 files found.\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# 1.  PACKAGES  (extends Part 1 package list; does not replace it)
# ─────────────────────────────────────────────────────────────────────────────
suppressMessages({
  library(readxl);    library(dplyr);       library(tidyr)
  library(lubridate); library(zoo);         library(tseries)
  library(urca);      library(car);         library(lmtest)
  library(strucchange);library(ARDL);       library(forecast)
  library(FinTS);     library(ggplot2);     library(patchwork)
  library(nortest)
})
cat("All packages loaded.\n\n")

theme_pub <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(panel.background  = element_rect(fill = "white"),
          panel.grid.major  = element_line(color = "grey85", linewidth = 0.3),
          panel.grid.minor  = element_blank(),
          axis.text         = element_text(size = 10, color = "black"),
          axis.title        = element_text(size = 11, face = "bold"),
          plot.title        = element_text(size = 13, face = "bold", hjust = 0),
          plot.subtitle     = element_text(size = 10, color = "grey40"),
          legend.background = element_rect(fill = "white", color = "grey70"),
          legend.key        = element_rect(fill = "white"),
          strip.background  = element_rect(fill = "grey90"),
          strip.text        = element_text(face = "bold", size = 10))
}

save_tiff <- function(p, fn, width = 12, height = 8, dpi = 600) {
  fp <- file.path(plots_dir, paste0(fn, ".tiff"))
  tiff(fp, width = width, height = height,
       units = "in", res = dpi, compression = "lzw")
  if (inherits(p, "ggplot")) print(p) else p
  dev.off()
  cat("Saved:", fp, "\n")
}


# ─────────────────────────────────────────────────────────────────────────────
# 2.  LOAD DATA FROM PART 1 OUTPUTS
#     Column names: date, QIM, WHEAT, FUEL, ENERGY, GAS  (Part 1 convention)
# ─────────────────────────────────────────────────────────────────────────────
cat("=== LOADING DATA FROM PART 1 OUTPUTS ===\n")

merged     <- read.csv(file.path(outputs_dir, "clean_merged_data.csv"),
                       stringsAsFactors = FALSE)
merged_log <- read.csv(file.path(outputs_dir, "merged_log.csv"),
                       stringsAsFactors = FALSE)
merged_bc  <- read.csv(file.path(outputs_dir, "merged_boxcox.csv"),
                       stringsAsFactors = FALSE)
boxcox_lambdas <- readRDS(file.path(outputs_dir, "boxcox_lambdas.rds"))

# Parse date column
for (df_name in c("merged", "merged_log", "merged_bc")) {
  obj <- get(df_name)
  obj$date <- as.Date(obj$date)
  assign(df_name, obj)
}

VARS      <- c("QIM", "WHEAT", "FUEL", "ENERGY", "GAS")
LABS      <- c("Industrial Production (QIM)", "Wheat (PKR/10kg)",
               "Fuel (PKR/L)", "Electricity (PKR/kWh)", "Gas (PKR/MMBTU)")
TRANS_LIST <- list(level  = merged,
                   log    = merged_log,
                   boxcox = merged_bc)

cat(sprintf("Loaded: %d obs | %s to %s\n\n",
            nrow(merged), min(merged$date), max(merged$date)))


# ─────────────────────────────────────────────────────────────────────────────
# 3.  LOAD ARDL FIT OBJECTS FROM PART 2
# ─────────────────────────────────────────────────────────────────────────────
cat("=== LOADING ARDL FITS FROM PART 2 ===\n")

ardl_fits <- list(
  level  = readRDS(file.path(outputs_dir, "ardl_fit_level.rds")),
  log    = readRDS(file.path(outputs_dir, "ardl_fit_log.rds")),
  boxcox = readRDS(file.path(outputs_dir, "ardl_fit_boxcox.rds"))
)

for (tr in names(ardl_fits)) {
  cat(sprintf("  %s ARDL order: %s\n", tr,
              paste(ardl_fits[[tr]]$best_order, collapse = ",")))
}
cat("\n")

# Print Zivot-Andrews summary from Part 2 for reference
za_file <- file.path(outputs_dir, "zivot_andrews_all_transforms.csv")
if (file.exists(za_file)) {
  cat("=== ZIVOT-ANDREWS RESULTS (from Part 2 — for reference) ===\n")
  print(read.csv(za_file))
  cat("\n")
}


# ─────────────────────────────────────────────────────────────────────────────
# STEP 9: ARDL RESIDUAL DIAGNOSTICS  (all three transformations)
# Tests:
#   a) Breusch-Godfrey serial correlation  [lmtest::bgtest]
#   b) Jarque-Bera normality               [tseries::jarque.bera.test]
#   c) ARCH-LM heteroskedasticity          [FinTS::ArchTest]
#   d) Ramsey RESET misspecification       [lmtest::resettest]
#   e) BDS nonlinearity                    [tseries::bds.test]
#   f) Four-panel residual plot
# ─────────────────────────────────────────────────────────────────────────────
cat("\n", rep("=", 70), "\n", sep = "")
cat("STEP 7: ARDL RESIDUAL DIAGNOSTICS\n")
cat(rep("=", 70), "\n\n", sep = "")

all_resid_diag <- list()

for (tr in c("level", "log", "boxcox")) {
  cat(rep("-", 65), "\n", sep = "")
  cat("Transformation:", toupper(tr), "\n")
  cat(rep("-", 65), "\n", sep = "")
  
  fit_obj    <- ardl_fits[[tr]]
  ardl_model <- fit_obj$ardl_model
  resid_vec  <- residuals(ardl_model)
  n_res      <- length(resid_vec)
  
  # ── a) Breusch-Godfrey Serial Correlation ──────────────────────────────
  bg_test <- bgtest(ardl_model, order = 12)
  cat(sprintf("  Breusch-Godfrey (lag 12): chi2=%.4f  p=%.4f  => %s\n",
              bg_test$statistic, bg_test$p.value,
              ifelse(bg_test$p.value > 0.05,
                     "No serial correlation (PASS)",
                     "Serial correlation present (FAIL)")))
  
  # ── b) Jarque-Bera Normality ────────────────────────────────────────────
  jb_test <- jarque.bera.test(resid_vec)
  cat(sprintf("  Jarque-Bera:              JB=%.4f    p=%.6f  => %s\n",
              jb_test$statistic, jb_test$p.value,
              ifelse(jb_test$p.value < 0.05,
                     "NON-NORMAL residuals (justifies ML)",
                     "Normal residuals")))
  
  # ── c) ARCH-LM Heteroskedasticity ───────────────────────────────────────
  arch_test <- ArchTest(resid_vec, lags = 12)
  cat(sprintf("  ARCH-LM (lag 12):         chi2=%.4f  p=%.4f  => %s\n",
              arch_test$statistic, arch_test$p.value,
              ifelse(arch_test$p.value < 0.05,
                     "ARCH effects present (heteroskedastic)",
                     "No significant ARCH effects")))
  
  # ── d) Ramsey RESET ─────────────────────────────────────────────────────
  reset_test <- tryCatch(
    resettest(ardl_model, power = 2:3, type = "fitted"),
    error = function(e) list(statistic = NA, p.value = NA)
  )
  cat(sprintf("  Ramsey RESET:             F=%-8s  p=%-8s  => %s\n",
              ifelse(is.na(reset_test$statistic), "N/A",
                     sprintf("%.4f", reset_test$statistic)),
              ifelse(is.na(reset_test$p.value),   "N/A",
                     sprintf("%.4f", reset_test$p.value)),
              ifelse(is.na(reset_test$p.value),    "Could not compute",
                     ifelse(reset_test$p.value > 0.05,
                            "No misspecification (PASS)",
                            "Functional-form misspecification (FAIL)"))))
  
  # ── e) BDS Nonlinearity ─────────────────────────────────────────────────
  cat("  BDS Nonlinearity test on residuals:\n")
  bds_tab <- data.frame(m = integer(), BDS = numeric(),
                        p = numeric(), Conclusion = character(),
                        stringsAsFactors = FALSE)
  for (m_val in 2:5) {
    bds <- tryCatch(bds.test(resid_vec, m = m_val),
                    error = function(e) NULL)
    if (!is.null(bds)) {
      stat <- bds$statistic[1]; pval <- bds$p.value[1]
      concl <- ifelse(pval < 0.05,
                      "Reject i.i.d. => NONLINEARITY",
                      "Fail to reject i.i.d.")
      bds_tab <- rbind(bds_tab, data.frame(m = m_val, BDS = round(stat, 4),
                                           p = round(pval, 4),
                                           Conclusion = concl,
                                           stringsAsFactors = FALSE))
      cat(sprintf("    m=%d  BDS=% .4f  p=%.4f  => %s\n",
                  m_val, stat, pval, concl))
    }
  }
  
  # ── Save residual diagnostics CSV ──────────────────────────────────────
  diag_row <- data.frame(
    Transformation = tr,
    BG_chi2        = round(bg_test$statistic,    4),
    BG_p           = round(bg_test$p.value,      4),
    BG_result      = ifelse(bg_test$p.value > 0.05, "PASS", "FAIL"),
    JB_stat        = round(jb_test$statistic,    4),
    JB_p           = round(jb_test$p.value,      6),
    JB_result      = ifelse(jb_test$p.value < 0.05, "Non-normal", "Normal"),
    ARCH_chi2      = round(arch_test$statistic,  4),
    ARCH_p         = round(arch_test$p.value,    4),
    ARCH_result    = ifelse(arch_test$p.value < 0.05, "ARCH present", "No ARCH"),
    RESET_F        = ifelse(is.na(reset_test$statistic), NA,
                            round(reset_test$statistic, 4)),
    RESET_p        = ifelse(is.na(reset_test$p.value),   NA,
                            round(reset_test$p.value,    4)),
    RESET_result   = ifelse(is.na(reset_test$p.value),   "N/A",
                            ifelse(reset_test$p.value > 0.05, "PASS", "FAIL")),
    BDS_m2_stat    = if (nrow(bds_tab) >= 1) bds_tab$BDS[1] else NA,
    BDS_m2_p       = if (nrow(bds_tab) >= 1) bds_tab$p[1]   else NA,
    BDS_m3_stat    = if (nrow(bds_tab) >= 2) bds_tab$BDS[2] else NA,
    BDS_m3_p       = if (nrow(bds_tab) >= 2) bds_tab$p[2]   else NA,
    BDS_m4_stat    = if (nrow(bds_tab) >= 3) bds_tab$BDS[3] else NA,
    BDS_m4_p       = if (nrow(bds_tab) >= 3) bds_tab$p[3]   else NA,
    BDS_m5_stat    = if (nrow(bds_tab) >= 4) bds_tab$BDS[4] else NA,
    BDS_m5_p       = if (nrow(bds_tab) >= 4) bds_tab$p[4]   else NA,
    stringsAsFactors = FALSE
  )
  all_resid_diag[[tr]] <- diag_row
  write.csv(diag_row,
            file.path(outputs_dir,
                      paste0("Diag_Step7_ARDL_Residuals_", tr, ".csv")),
            row.names = FALSE)
  
  # ── f) Four-panel residual plot ────────────────────────────────────────
  df_res <- data.frame(
    idx   = seq_along(resid_vec),
    resid = as.numeric(resid_vec),
    date  = tail(TRANS_LIST[[tr]]$date, n_res)
  )
  ci_val <- qnorm(0.975) / sqrt(n_res)
  acf_obj  <- acf(resid_vec,  lag.max = 24, plot = FALSE)
  pacf_obj <- pacf(resid_vec, lag.max = 24, plot = FALSE)
  df_acf   <- data.frame(lag = as.numeric(acf_obj$lag[-1]),
                         acf = as.numeric(acf_obj$acf[-1]))
  df_pacf  <- data.frame(lag = as.numeric(pacf_obj$lag),
                         pacf = as.numeric(pacf_obj$acf))
  qq_dat   <- qqnorm(resid_vec, plot.it = FALSE)
  qq_df    <- data.frame(th = qq_dat$x, sm = qq_dat$y)
  
  p_res <- ggplot(df_res, aes(x = date, y = resid)) +
    geom_line(colour = "#264653", linewidth = 0.55) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    labs(title = "(a) Residual series", x = "Date", y = "Residual") +
    theme_pub(11)
  
  p_acf <- ggplot(df_acf, aes(x = lag, y = acf)) +
    geom_segment(aes(xend = lag, yend = 0),
                 colour = "#E76F51", linewidth = 0.7) +
    geom_hline(yintercept = c(-ci_val, ci_val),
               colour = "steelblue", linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    labs(title = "(b) ACF of residuals", x = "Lag", y = "ACF") +
    theme_pub(11)
  
  p_pacf <- ggplot(df_pacf, aes(x = lag, y = pacf)) +
    geom_segment(aes(xend = lag, yend = 0),
                 colour = "#E76F51", linewidth = 0.7) +
    geom_hline(yintercept = c(-ci_val, ci_val),
               colour = "steelblue", linetype = "dashed") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = seq(0, 24, 4)) +
    labs(title = "(c) PACF of residuals", x = "Lag", y = "PACF") +
    theme_pub(11)
  
  p_qq <- ggplot(qq_df, aes(x = th, y = sm)) +
    geom_point(colour = "#264653", size = 1.2, alpha = 0.75) +
    geom_abline(slope = sd(resid_vec), intercept = mean(resid_vec),
                colour = "#E76F51", linewidth = 0.9) +
    labs(title = sprintf("(d) Q-Q plot  [JB: stat=%.2f, p=%.4f]",
                         jb_test$statistic, jb_test$p.value),
         x = "Theoretical quantiles", y = "Sample quantiles") +
    theme_pub(11)
  
  panel <- (p_res | p_acf) / (p_pacf | p_qq) +
    plot_annotation(
      title    = sprintf("ARDL Residual Diagnostics — %s transformation",
                         toupper(tr)),
      subtitle = sprintf(
        "BG(12): chi2=%.2f p=%.3f  |  JB: stat=%.2f p=%.4f  |  ARCH(12): p=%.3f  |  BDS(m=2): p=%.3f",
        bg_test$statistic, bg_test$p.value,
        jb_test$statistic, jb_test$p.value,
        arch_test$p.value,
        if (nrow(bds_tab) >= 1) bds_tab$p[1] else NA),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, colour = "grey30")
      )
    )
  
  save_tiff(panel,
            paste0("Diag_Step7_ResidPanel_", tr),
            width = 14, height = 10)
  cat("\n")
}

# Combine all three residual diagnostic rows into one master table
combined_diag <- do.call(rbind, all_resid_diag)
write.csv(combined_diag,
          file.path(outputs_dir, "Diag_Step7_AllTransformations.csv"),
          row.names = FALSE)
cat("Combined residual diagnostics saved: Diag_Step7_AllTransformations.csv\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# STEP 10: BAI-PERRON STRUCTURAL BREAK TEST ON QIM
#          (runs on all three transformations of the QIM series)
# ─────────────────────────────────────────────────────────────────────────────
cat(rep("=", 70), "\n", sep = "")
cat("STEP 8: BAI-PERRON STRUCTURAL BREAK TEST\n")
cat(rep("=", 70), "\n\n", sep = "")

bp_summary_all <- data.frame(
  Transformation = character(),
  N_breaks       = integer(),
  Break_dates    = character(),
  RSS_BIC        = numeric(),
  stringsAsFactors = FALSE
)

for (tr in c("level", "log", "boxcox")) {
  df_tr  <- TRANS_LIST[[tr]]
  y_qim  <- df_tr$QIM
  n_obs  <- length(y_qim)
  
  bp     <- breakpoints(y_qim ~ 1, h = 0.15)
  bp_sum <- summary(bp)
  
  # BIC-optimal number of breaks (column names are "0","1","2",...)
  bic_row  <- bp_sum$RSS["BIC", ]
  opt_n    <- as.integer(names(which.min(bic_row)))
  
  # Re-extract breakpoint indices for the optimal number
  if (opt_n == 0) {
    bp_idx <- integer(0)
  } else {
    bp_opt <- breakpoints(bp, breaks = opt_n)
    bp_idx <- bp_opt$breakpoints
    # Guard: remove any NA indices that strucchange sometimes returns
    bp_idx <- bp_idx[!is.na(bp_idx)]
  }
  
  bp_dates <- if (length(bp_idx) > 0) {
    paste(format(df_tr$date[bp_idx], "%Y-%m"), collapse = ", ")
  } else {
    "No significant breaks"
  }
  
  cat(sprintf("  %s: %d break(s) at [%s]\n",
              toupper(tr), length(bp_idx), bp_dates))
  
  bp_summary_all <- rbind(bp_summary_all, data.frame(
    Transformation = tr,
    N_breaks       = length(bp_idx),
    Break_dates    = bp_dates,
    RSS_BIC        = round(min(bic_row, na.rm = TRUE), 4),
    stringsAsFactors = FALSE
  ))
  
  # ── Compute segmented-mean trend MANUALLY (avoids fitted() returning
  #    zero rows when strucchange re-calls breakpoints() internally) ──────
  seg_trend <- numeric(n_obs)
  seg_bounds <- c(0L, bp_idx, n_obs)          # segment boundary indices
  for (s in seq_len(length(seg_bounds) - 1)) {
    idx_from <- seg_bounds[s] + 1L
    idx_to   <- seg_bounds[s + 1L]
    seg_trend[idx_from:idx_to] <- mean(y_qim[idx_from:idx_to], na.rm = TRUE)
  }
  
  df_plot <- data.frame(
    date      = df_tr$date,
    value     = y_qim,
    seg_trend = seg_trend
  )
  
  p_bp <- ggplot(df_plot, aes(x = date)) +
    geom_line(aes(y = value),     colour = "#264653", linewidth = 0.6) +
    geom_line(aes(y = seg_trend), colour = "#E63946", linewidth = 1.1) +
    {if (length(bp_idx) > 0)
      geom_vline(xintercept = as.numeric(df_tr$date[bp_idx]),
                 colour = "#E9C46A", linetype = "dashed", linewidth = 0.9)
      else NULL} +
    labs(title    = paste("Bai-Perron Structural Breaks — QIM",
                          toupper(tr), "transformation"),
         subtitle = sprintf("%d break(s): %s | Red = segmented mean trend",
                            length(bp_idx), bp_dates),
         x = "Year",
         y = paste("QIM (", tr, ")", sep = "")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_pub(12)
  
  save_tiff(p_bp,
            paste0("Diag_Step8_StructuralBreaks_", tr),
            width = 14, height = 6)
}

write.csv(bp_summary_all,
          file.path(outputs_dir, "Diag_Step8_StructuralBreaks.csv"),
          row.names = FALSE)
cat("\nBai-Perron summary saved: Diag_Step8_StructuralBreaks.csv\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# STEP 11: 24-MONTH ROLLING PEARSON CORRELATIONS
#          (QIM vs each commodity, using LOG transformation)
# ─────────────────────────────────────────────────────────────────────────────
cat(rep("=", 70), "\n", sep = "")
cat("STEP 9: 24-MONTH ROLLING PEARSON CORRELATIONS\n")
cat(rep("=", 70), "\n\n", sep = "")

window_size <- 24
roll_cor <- function(x, y, w) {
  rollapply(data.frame(x, y), width = w,
            FUN = function(d) cor(d[, 1], d[, 2], use = "complete.obs"),
            by.column = FALSE, align = "right", fill = NA)
}

# Use log transformation for rolling correlations (most interpretable)
df_log <- merged_log

rc_df <- data.frame(
  date  = df_log$date,
  Fuel  = as.numeric(roll_cor(df_log$QIM, df_log$FUEL,   window_size)),
  Elec  = as.numeric(roll_cor(df_log$QIM, df_log$ENERGY, window_size)),
  Wheat = as.numeric(roll_cor(df_log$QIM, df_log$WHEAT,  window_size)),
  Gas   = as.numeric(roll_cor(df_log$QIM, df_log$GAS,    window_size))
)

# Print range summary
cat("Rolling correlation ranges [min, max] — log transformation:\n")
for (v in c("Fuel", "Elec", "Wheat", "Gas")) {
  vals <- rc_df[[v]]
  cat(sprintf("  %-6s  min=% .3f  max=% .3f\n",
              v, min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)))
}

# Long format
rc_long <- tidyr::pivot_longer(rc_df, cols = -date,
                               names_to = "Commodity", values_to = "r")
rc_long$Commodity <- factor(rc_long$Commodity,
                            levels = c("Fuel","Elec","Wheat","Gas"),
                            labels = c("Fuel","Electricity","Wheat","Gas"))
rc_long <- rc_long[!is.na(rc_long$r), ]

pal_roll <- c("Fuel"="#E63946", "Electricity"="#264653",
              "Wheat"="#2A9D8F", "Gas"="#A8DADC")

p_roll <- ggplot(rc_long, aes(x = date, y = r, colour = Commodity)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, colour = "black",
             linetype = "dashed", linewidth = 0.5) +
  scale_colour_manual(values = pal_roll) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title    = "24-Month Rolling Pearson Correlations: ln(QIM) vs Commodity Prices",
       subtitle = "Time-varying sign and magnitude confirm parameter instability in linear models",
       x = "Year", y = "Pearson r", colour = "Commodity") +
  theme_pub(12) +
  theme(legend.position = "bottom")

save_tiff(p_roll, "Diag_Step9_RollingCorrelations", width = 14, height = 6)
write.csv(rc_df,
          file.path(outputs_dir, "Diag_Step9_RollingCorrelations.csv"),
          row.names = FALSE)
cat("Rolling correlation plot & CSV saved.\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# STEP 12: CUSUM / CUSUM-OF-SQUARES PARAMETER STABILITY
#           (on the ARDL model residuals, all three transformations)
# ─────────────────────────────────────────────────────────────────────────────
cat(rep("=", 70), "\n", sep = "")
cat("STEP 10: CUSUM / CUSUM-OF-SQUARES PARAMETER STABILITY\n")
cat(rep("=", 70), "\n\n", sep = "")

for (tr in c("level", "log", "boxcox")) {
  ardl_model <- ardl_fits[[tr]]$ardl_model
  resid_vec  <- residuals(ardl_model)
  
  # efp() requires the model formula and data — use OLS-CUSUM on residuals
  tryCatch({
    cusum_obj  <- efp(resid_vec ~ 1, type = "OLS-CUSUM")
    cusumSq_obj<- efp(resid_vec ~ 1, type = "OLS-MOSUM")
    
    fp_cusum   <- file.path(plots_dir,
                            paste0("Diag_Step10_CUSUM_", tr, ".tiff"))
    tiff(fp_cusum, width = 12, height = 5, units = "in",
         res = 600, compression = "lzw")
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
    plot(cusum_obj,
         main  = paste("CUSUM —", toupper(tr)),
         ylab  = "Empirical fluctuation process",
         col   = "#264653", lwd = 2)
    plot(cusumSq_obj,
         main  = paste("MOSUM —", toupper(tr)),
         ylab  = "MOSUM process",
         col   = "#E63946", lwd = 2)
    dev.off()
    cat(sprintf("  %s: CUSUM figure saved: %s\n", toupper(tr), fp_cusum))
    
    # Formal sctest
    sc_cusum <- sctest(cusum_obj)
    cat(sprintf("         CUSUM sctest: stat=%.4f  p=%.4f  => %s\n",
                sc_cusum$statistic, sc_cusum$p.value,
                ifelse(sc_cusum$p.value < 0.05,
                       "Parameter instability detected",
                       "Parameters stable")))
  }, error = function(e) {
    cat(sprintf("  %s: CUSUM error — %s\n", toupper(tr), e$message))
  })
}
cat("\n")


# ─────────────────────────────────────────────────────────────────────────────
# STEP 13: MASTER DIAGNOSTIC SUMMARY TABLE
# ─────────────────────────────────────────────────────────────────────────────
cat(rep("=", 70), "\n", sep = "")
cat("STEP 11: MASTER DIAGNOSTIC SUMMARY\n")
cat(rep("=", 70), "\n\n", sep = "")

master <- combined_diag[, c("Transformation",
                            "BG_chi2","BG_p","BG_result",
                            "JB_stat","JB_p","JB_result",
                            "ARCH_chi2","ARCH_p","ARCH_result",
                            "RESET_F","RESET_p","RESET_result",
                            "BDS_m2_stat","BDS_m2_p",
                            "BDS_m3_stat","BDS_m3_p")]

cat("ARDL Residual Diagnostics — All Transformations:\n")
print(master, digits = 4)

write.csv(master,
          file.path(outputs_dir, "Diag_Step11_MasterSummary.csv"),
          row.names = FALSE)
cat("\nMaster summary saved: Diag_Step11_MasterSummary.csv\n")

# Also append Bai-Perron and ZA info from earlier steps
cat("\nStructural Break Summary:\n")
print(bp_summary_all)

if (file.exists(za_file)) {
  cat("\nZivot-Andrews Summary (from Part 2):\n")
  print(read.csv(za_file))
}


# ─────────────────────────────────────────────────────────────────────────────
# FINAL CONSOLE REPORT
# ─────────────────────────────────────────────────────────────────────────────
cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("PART 3 DIAGNOSTIC CHECK — COMPLETE\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("Output files written to:", outputs_dir, "\n")
cat("  Diag_Step7_ARDL_Residuals_level.csv\n")
cat("  Diag_Step7_ARDL_Residuals_log.csv\n")
cat("  Diag_Step7_ARDL_Residuals_boxcox.csv\n")
cat("  Diag_Step7_AllTransformations.csv\n")
cat("  Diag_Step8_StructuralBreaks.csv\n")
cat("  Diag_Step9_RollingCorrelations.csv\n")
cat("  Diag_Step11_MasterSummary.csv\n")
cat("\nFigures written to:", plots_dir, "\n")
cat("  Diag_Step7_ResidPanel_{level|log|boxcox}.tiff\n")
cat("  Diag_Step8_StructuralBreaks_{level|log|boxcox}.tiff\n")
cat("  Diag_Step9_RollingCorrelations.tiff\n")
cat("  Diag_Step10_CUSUM_{level|log|boxcox}.tiff\n")

cat("\nKey findings checklist:\n")
cat("  [ ] BG test: serial correlation in ARDL residuals?\n")
cat("  [ ] JB test: non-normal residuals => justifies ML approach?\n")
cat("  [ ] ARCH test: heteroskedasticity in ARDL residuals?\n")
cat("  [ ] RESET test: functional-form adequate?\n")
cat("  [ ] BDS test: nonlinearity in ARDL residuals => GBM/RF justified?\n")
cat("  [ ] Bai-Perron: structural break confirmed (April 2020 expected)?\n")
cat("  [ ] Rolling r: sign-switching confirms parameter instability?\n")
cat("  [ ] CUSUM: parameter stability over time?\n")
cat("\n>>> Proceed to Part 4: Forecasting (ARIMA, ETS, RF, GBM, hybrids).\n\n")

# =============================================================================
# END OF PART 3
# =============================================================================

