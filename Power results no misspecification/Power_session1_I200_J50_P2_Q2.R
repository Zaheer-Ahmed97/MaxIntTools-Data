# ============================================================================
# POWER SESSION 1: I=200, J=50  |  true P=2,3  /  Q=2,3  |  1000 replications
# ============================================================================

library(eRm);        library(mirt);       library(psychotools)
library(strucchange);library(parallel);   library(doParallel)
library(foreach);    library(pracma);     library(reshape)
library(gdata);      library(Rfast);      library(R.utils)
library(fossil);     library(lintools);   library(picante)
library(sna);        library(psych)

# ---------------------------------------------------------------------------
# 1.  PATHS
# ---------------------------------------------------------------------------
path <- '~/Documents/Zaheer/'

# Output folder — created here so it definitely exists before anything runs
output_dir <- file.path(path, "power_results")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = TRUE)
}
cat("Output directory:", output_dir, "\n")

# ---------------------------------------------------------------------------
# 2.  SOURCE MAIN FUNCTIONS AND PERMUTATION FILES
# ---------------------------------------------------------------------------
source(file.path(path, "Power_analysis_main_functions.R"))

required_files <- c(
  'Randompartition_function.R',
  'Unequal_Randompartition_Function.R',
  'Log_Likelihood_function_REMAXINT.R',
  'Log_Likelihood_function_E_ReMI.R',
  'Log_LR_Test_Statistic.R',
  'Update_column_clusters.R',
  'Update_row_clusters_REMAXINT.R',
  'Update_row_clusters_E_ReMI.R',
  'Update_G_Omega.R',
  'REMAXINT.R',
  'E_ReMI.R',
  'Permutation_Function.R'
)

cat("\nSourcing required files...\n")
for (f in required_files) {
  source(file.path(path, f))
  cat("  -", f, "loaded\n")
}

# ---------------------------------------------------------------------------
# 3.  SIMULATION PARAMETERS
# ---------------------------------------------------------------------------
I             <- 200
J             <- 50
true_P_values <- c(2, 3)   # one entry per condition
true_Q_values <- c(2, 3)   # must be same length as true_P_values
n_replications<- 1000
Nruns         <- 20         # increase for full run
permutations  <- 250         # increase for full run
session_seed  <- 5001

# output_file = directory + base filename (NO extension, NO cluster suffix)
# The function appends  _P2_Q2_summary.csv  etc. automatically
output_base <- file.path(output_dir,
                         paste0("Power_session1_I", I, "_J", J))

# ---------------------------------------------------------------------------
# 4.  RUN SIMULATION
# ---------------------------------------------------------------------------
set.seed(session_seed)

result <- stat_power_simulation_parallel(
  n_replications = n_replications,           
  I              = I,
  J              = J,
  true_P_values  = true_P_values,
  true_Q_values  = true_Q_values,
  Nruns          = Nruns,            # increase for full run
  permutations   = permutations,            # increase for full run
  alpha_level    = 0.05,
  n_cores        = 120,
  source_path    = path,
  output_file    = output_base   # per-condition CSVs saved automatically
)

# ---------------------------------------------------------------------------
# 5.  ATTACH SEED AND SAVE RESULTS
# ---------------------------------------------------------------------------
result$seed_used <- session_seed

# Full result object (can be reloaded with readRDS)
rds_file <- paste0(output_base, "_results.rds")
saveRDS(result, file = rds_file)
cat("RDS saved:", rds_file, "\n")

# Combined flat summary CSV (all conditions in one table)
if (!is.null(result$combined_summary) && nrow(result$combined_summary) > 0) {
  csv_file <- paste0(output_base, "_summary.csv")
  write.csv(result$combined_summary, file = csv_file, row.names = FALSE)
  cat("Combined summary CSV saved:", csv_file, "\n")
} else {
  cat("WARNING: combined_summary is empty — no CSV written.\n")
}

cat("\n=== Session 1 complete. Files saved to:", output_dir, "===\n")