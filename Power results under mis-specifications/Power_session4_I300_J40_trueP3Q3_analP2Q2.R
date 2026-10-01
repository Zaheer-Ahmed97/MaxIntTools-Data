# ========================================================================================================
# POWER SESSION 1: I=300, J=40 |  Data generated: P=3,Q=3  |  Analysed as: P=2,Q=2  |  1000 replications
# ========================================================================================================

library(eRm);        library(mirt);       library(psychotools)
library(strucchange);library(parallel);   library(doParallel)
library(foreach);    library(pracma);     library(reshape)
library(gdata);      library(Rfast);      library(R.utils)
library(fossil);     library(lintools);   library(picante)
library(sna);        library(psych)

# ---------------------------------------------------------------------------
# 1.  PATHS
# ---------------------------------------------------------------------------
path <- '~/Documents/powerUnderSpecification/'

# Output folder — created here so it definitely exists before anything runs
output_dir <- file.path(path, "power_results_trueP3Q3_analP4Q4")
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
I             <- 300
J             <- 40
true_P_values <- c(3)    # data generated with true P=3
true_Q_values <- c(3)    # data generated with true Q=3
analysis_P <- 4
analysis_Q <- 4
n_replications<- 1000
Nruns         <- 20         # increase for full run
permutations  <- 250         # increase for full run
session_seed  <- 5004

# output_file = directory + base filename (NO extension, NO cluster suffix)
# The function appends  _P2_Q2_summary.csv  etc. automatically
output_base <- file.path(output_dir,
                         paste0("Power_session1_I", I, "_J", J,
                                "_trueP3Q3_analP4Q4"))

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
  analysis_P     = analysis_P,     # <<< NEW: analyst assumes P=2
  analysis_Q     = analysis_Q,     # <<< NEW: analyst assumes Q=2
  Nruns          = Nruns,
  permutations   = permutations,
  alpha_level    = 0.05,
  n_cores        = 100,
  source_path    = path,
  output_file    = output_base
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

cat("\n=== Session 4 complete [DataGen=P3Q3 | Analysis=P2Q2]. Files saved to:", output_dir, "===\n")