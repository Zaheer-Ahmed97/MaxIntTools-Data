# ============================================================================
# One Data Size Per Session
# ============================================================================
# This approach is FASTER than the original Sequential Approach
# Each session focuses on ONE data size with ALL 8 cores
# ============================================================================
# Set your path
path <- '~/Documents/Type1ErrorSimulation/'

# Check if files exist
required_files <- c(
  paste0(path, "Type1_error_main_functions.R"),
  paste0(path, "Type1Error_session1_I200_J50.R"),
  paste0(path, "Type1Error_session2_I500_J40.R"),
  paste0(path, "Type1Error_session3_I800_J50.R"),
  paste0(path, "Type1Error_session4_I2000_J40.R")
)

cat("\nChecking for required files...\n")
all_exist <- TRUE
for (file in required_files) {
  if (file.exists(file)) {
    cat("  [OK]", basename(file), "\n")
  } else {
    cat("  [MISSING]", basename(file), "\n")
    all_exist <- FALSE
  }
}

if (!all_exist) {
  stop("Some required files are missing. Please ensure all files are in the correct directory.")
}

# Check if rstudioapi is available
if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  cat("\nWARNING: rstudioapi package not found.\n")
  cat("Installing rstudioapi...\n")
  install.packages("rstudioapi")
}

library(rstudioapi)

# Launch Session 1
cat("\n[1/4] Launching Session 1: I=200, J=50\n")
session1_id <- jobRunScript(
  path = paste0(path, "Type1Error_session1_I200_J50.R"),
  name = "Session1_I200_J50",
  #workingDir = path,
  importEnv = FALSE
)
cat("Session 1 started with ID:", session1_id, "\n")

Sys.sleep(2)

# Launch Session 2
cat("\n[2/4] Launching Session 2: I=500, J=40\n")
session2_id <- jobRunScript(
  path = paste0(path, "Type1Error_session2_I500_J40.R"),
  name = "Session2_I500_J40",
  #workingDir = path,
  importEnv = FALSE
)
cat("Session 2 started with ID:", session2_id, "\n")

Sys.sleep(2)

# # Launch Session 3
cat("\n[3/4] Launching Session 3: I=800, J=50\n")
session3_id <- jobRunScript(
  path = paste0(path, "Type1Error_session3_I800_J50.R"),
  name = "Session3_I800_J50",
  #workingDir = path,
  importEnv = FALSE
)
cat("Session 3 started with ID:", session3_id, "\n")

Sys.sleep(2)

# # Launch Session 4
cat("\n[4/4] Launching Session 4: I=2000, J=40\n")
session4_id <- jobRunScript(
  path = paste0(path, "Type1Error_session4_I2000_J40.R"),
  name = "Session4_I2000_J40",
  #workingDir = path,
  importEnv = FALSE
)
cat("Session 4 started with ID:", session4_id, "\n")

Sys.sleep(2)

# Save session info
session_info <- data.frame(
  Session = c("Session1", "Session2", "Session3", "Session4"),
  SessionID = c(session1_id, session2_id, session3_id, session4_id),
  DataSize = c("I=200,J=50", "I=500,J=40", "I=800,J=50", "I=2000,J=40"),
  Replications = 1000,
  OutputFolder = "Type1Error_results/",
  Status = "Running",
  StartTime = as.character(Sys.time())
)

saveRDS(session_info, paste0(path, "Type1Error_session_tracking.rds"))
write.csv(session_info, paste0(path, "Type1Error_session_tracking.csv"), row.names = FALSE)