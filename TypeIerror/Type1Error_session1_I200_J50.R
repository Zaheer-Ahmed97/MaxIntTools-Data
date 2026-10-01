# ============================================================================
# SESSION 1 DATA SIZE: I=200, J=50 - 1000 Replications
# ============================================================================
# Load required libraries
library(eRm)
library(mirt)
library(psychotools)
library(strucchange)
library(parallel)
library(doParallel)
library(foreach)
library(pracma)
library(reshape)
library(gdata)
library(Rfast)
library(R.utils)
library(fossil)
library(lintools)
library(picante)
library(sna)
library(psych)

# Set your path
path <- '~/Documents/Type1ErrorSimulation/'

# Source main functions
source(paste0(path, "Type1_error_main_functions.R"))

# Source required permutation files
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
for (file in required_files) {
  source(file.path(path, file))
  cat("  -", file, "loaded\n")
}

# Define cluster configurations
cluster_configs <- matrix(c(2,2), nrow=1, ncol=2, byrow=TRUE)

# Create output directory
output_dir <- paste0(path, "TypeIError_results/")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Data size for THIS session
I <- 200
J <- 50

# SET SEED FOR REPRODUCIBILITY - Session 1
session_seed <- 1001
set.seed(session_seed)

# Run simulation 
result <- type1_error_simulation_parallel(
  n_replications = 1000,
  I = I,
  J = J,
  cluster_configs = cluster_configs,
  Nruns = 20,
  permutations = 250,
  alpha_level = 0.05,
  n_cores = 120,  # Use ALL 8 cores
  source_path = path,
  output_file = paste0(output_dir, "_session1_I", I, "_J", J)
)

# Save seed info
result$seed_used <- session_seed

# Save detailed results
saveRDS(result, 
        file = paste0(output_dir, "_session1_I", I, "_J", J, "_results.rds"))
write.csv(
  stack(result),
  file = paste0(output_dir, "_session1_I", I, "_J", J, "_results.csv"),
  row.names = FALSE
)