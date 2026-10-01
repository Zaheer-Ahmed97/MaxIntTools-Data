# ============================================================================
# POWER ANALYSIS LAUNCHER — launches all sessions as RStudio background jobs
# Each session covers one (I, J) size; each session runs BOTH true P=2,Q=2
# and true P=3,Q=3 conditions internally.
# ============================================================================
# ---------------------------------------------------------------------------
# 1.  PATH
# ---------------------------------------------------------------------------
path <- '~/Documents/Zaheer/'

# ---------------------------------------------------------------------------
# 2.  SESSION DEFINITIONS
# ---------------------------------------------------------------------------
sessions <- data.frame(
  label      = c("Session1", "Session2", "Session3", "Session4", "Session5"),
  filename   = c("Power_session1_I200_J50_P2_Q2.R",
                 "Power_session2_I100_J25_P2_Q2.R",
                 "Power_session3_I300_J50_P2_Q2.R",
                 "Power_session4_I300_J40_P2_Q2.R",
                 "Power_session5_I500_J40_P2_Q2.R"),
  job_name   = c("Power_S1_I200_J50",
                 "Power_S2_I100_J25",
                 "Power_S3_I300_J50",
                 "Power_S4_I300_J40",
                 "Power_S5_I500_J40"),
  data_size  = c("I=200,J=50", "I=100,J=25", "I=300,J=50",
                 "I=300,J=40", "I=500,J=40"),
  seed       = c(5001, 5002, 5003, 5004, 5005),
  active     = c(FALSE, FALSE, FALSE, FALSE, TRUE),  # set TRUE to enable
  stringsAsFactors = FALSE
)

# Keep only sessions flagged as active
active_sessions <- sessions[sessions$active, ]

if (nrow(active_sessions) == 0) {
  stop("No sessions are marked active. Set active = TRUE for at least one session.")
}

# ---------------------------------------------------------------------------
# 3.  ENSURE OUTPUT DIRECTORY EXISTS
# ---------------------------------------------------------------------------
output_dir <- file.path(path, "power_results")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = TRUE)
  cat("\nCreated output directory:", output_dir, "\n")
} else {
  cat("\nOutput directory exists:", output_dir, "\n")
}

# ---------------------------------------------------------------------------
# 4.  LAUNCH JOBS
# ---------------------------------------------------------------------------
cat("\nLaunching", nrow(active_sessions), "session(s)...\n")

job_ids    <- character(nrow(active_sessions))
start_times <- character(nrow(active_sessions))

for (i in seq_len(nrow(active_sessions))) {
  sess <- active_sessions[i, ]
  cat(sprintf("\n[%d/%d] Launching %s: %s\n",
              i, nrow(active_sessions), sess$label, sess$data_size))
  
  job_ids[i]     <- tryCatch(
    rstudioapi::jobRunScript(
      path       = file.path(path, sess$filename),
      name       = sess$job_name,
      #workingDir = path,
      importEnv  = FALSE
    ),
    error = function(e) {
      cat("  ERROR launching", sess$label, ":", e$message, "\n")
      NA_character_
    }
  )
  start_times[i] <- as.character(Sys.time())
  cat("  Job ID:", job_ids[i], "\n")
  Sys.sleep(2)   # brief pause between launches
}

# ---------------------------------------------------------------------------
# 5.  SAVE SESSION TRACKING INFO
# ---------------------------------------------------------------------------
tracking <- data.frame(
  Session        = active_sessions$label,
  JobID          = job_ids,
  ScriptFile     = active_sessions$filename,
  DataSize       = active_sessions$data_size,
  TrueStructure  = "P=2&3, Q=2&3",   # both conditions run inside each session
  Replications   = 1000,
  Seed           = active_sessions$seed,
  OutputFolder   = output_dir,
  Status         = ifelse(is.na(job_ids), "FAILED_TO_LAUNCH", "Running"),
  StartTime      = start_times,
  stringsAsFactors = FALSE
)

tracking_rds <- file.path(path, "Power_session_tracking.rds")
tracking_csv <- file.path(path, "Power_session_tracking.csv")

saveRDS(tracking, tracking_rds)
write.csv(tracking, tracking_csv, row.names = FALSE)

cat("\n--- Session tracking saved ---\n")
cat("  RDS:", tracking_rds, "\n")
cat("  CSV:", tracking_csv, "\n")

cat("\n--- Launch summary ---\n")
print(tracking[, c("Session","DataSize","Status","StartTime")])