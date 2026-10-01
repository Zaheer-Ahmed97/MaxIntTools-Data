# ============================================================================
# # MAIN FUNCTIONS FOR STATISTICAL POWER ANALYSIS
# # This file contains all core functions for power simulations
# ============================================================================

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

# ============================================================================
# BINARY DATA GENERATION WITH TRUE CLUSTERING STRUCTURE
# ============================================================================

dicho_data_gen <- function(I, J, P, Q, mu = 0, sigma_alpha = 0.5, sigma_epsilon = 1) {
  
  if (P < I) {
    True_R <- as.matrix(Unequal_Randompartition_Function(I, P, 0.5))
  } else {
    True_R <- diag(I)
  }
  
  if (Q < J){
    True_C <- as.matrix(Randompartition_function(J, Q))
  } else {
    True_C <- diag(J)
  }
  
  CardC <- colSums(True_C)
  int_con <- I*J
  
  # Define TRUE gamma's for (P,Q) = (2,2) and (3,3)
  if (P==2 && Q==2){
    True_G = CardC[1]/(4*J)* matrix(c(+1*int_con/(0.5*I*CardC[1]),-1*int_con/(0.5*I*CardC[2]),
                                      -1*int_con/(0.5*I*CardC[1]),+1*int_con/(0.5*I*CardC[2])),
                                    nrow = 2, ncol = 2,byrow = TRUE)}
  if (P==3 && Q==3){
    True_G = CardC[1]/(8*J)* matrix(c(-1*int_con/(0.5*I*CardC[1]),0,+1*int_con/(0.5*I*CardC[3]),
                                      0,0,0,
                                      +1*int_con/(0.25*I*CardC[1]),0,-1*int_con/(0.25*I*CardC[3])),
                                    nrow = 3, ncol = 3,byrow = TRUE)}
  
  True_T <- True_R%*%True_G%*%t(True_C)
  
  alpha_i <- matrix(rnorm(I * J, mean = 0, sd = sigma_alpha),
                    nrow = I, ncol = J)
  beta_j <- matrix(rep(seq(-0.5, 0.5, length.out = J), each = I),
                   nrow = I, ncol = J, byrow = FALSE)
  epsilon_ij <- matrix(rnorm(I * J, mean = 0, sd = sigma_epsilon),
                       nrow = I, ncol = J)
  data <- mu + alpha_i + beta_j + True_T + epsilon_ij
  
  median_data <- median(data, na.rm = TRUE)
  binary_data <- matrix(as.integer(data > median_data), nrow = I, ncol = J)
  
  row_totals   <- rowSums(binary_data)
  median_total <- median(row_totals)
  group_labels <- ifelse(row_totals > median_total, 1, 0)
  
  if (length(unique(group_labels)) < 2) {
    sorted_indices <- order(row_totals)
    half_point <- ceiling(I / 2)
    group_labels <- rep(0, I)
    group_labels[sorted_indices[(half_point + 1):I]] <- 1
  }
  
  group_labels <- as.integer(as.vector(group_labels))
  row_totals   <- as.numeric(as.vector(row_totals))
  
  return(list(
    binary_data  = binary_data,
    group_labels = group_labels,
    row_totals   = row_totals
  ))
}

# ============================================================================
# PARAMETRIC IRT TESTS
# ============================================================================
parametric_irt_tests <- function(binary_data, group_labels, row_totals,
                                 alpha_level = 0.05) {
  
  results <- list()
  
  if (length(unique(group_labels)) < 2) {
    return(list(
      Andersen_LR = list(reject = NA, error = "No group variance"),
      Martin_Lof  = list(reject = NA, error = "No group variance"),
      LMuo        = list(reject = NA, error = "No group variance"),
      maxLM       = list(reject = NA, error = "No group variance")
    ))
  }
  
  group_labels <- as.integer(as.vector(group_labels))
  
  tryCatch({
    fit_RM <- RM(binary_data)
    lr_output <- LRtest(fit_RM, splitcr = group_labels)
    results$Andersen_LR <- list(
      statistic = as.numeric(lr_output$LR),
      p_value   = as.numeric(lr_output$pvalue),
      reject    = as.logical(lr_output$pvalue <= alpha_level)
    )
  }, error = function(e) {
    results$Andersen_LR <<- list(reject = NA, error = as.character(e$message))
  })
  
  tryCatch({
    if (!exists("fit_RM")) fit_RM <- RM(binary_data)
    ml_output <- MLoef(fit_RM, splitcr = "median")
    results$Martin_Lof <- list(
      statistic = as.numeric(ml_output$LR),
      p_value   = as.numeric(ml_output$p.value),
      reject    = as.logical(ml_output$p.value <= alpha_level)
    )
  }, error = function(e) {
    results$Martin_Lof <<- list(reject = NA, error = as.character(e$message))
  })
  
  tryCatch({
    data_df <- as.data.frame(binary_data)
    fit_rasch <- raschmodel(data_df)
    lmuo_output <- strucchange::sctest(fit_rasch,
                                       order.by = row_totals,
                                       functional = "LMuo")
    results$LMuo <- list(
      statistic = as.numeric(lmuo_output$statistic),
      p_value   = as.numeric(lmuo_output$p.value),
      reject    = as.logical(lmuo_output$p.value <= alpha_level)
    )
  }, error = function(e) {
    results$LMuo <<- list(reject = NA, error = as.character(e$message))
  })
  
  tryCatch({
    if (!exists("fit_rasch")) {
      data_df <- as.data.frame(binary_data)
      fit_rasch <- raschmodel(data_df)
    }
    maxlm_output <- strucchange::sctest(fit_rasch,
                                        order.by = row_totals,
                                        functional = "maxLM")
    results$maxLM <- list(
      statistic = as.numeric(maxlm_output$statistic),
      p_value   = as.numeric(maxlm_output$p.value),
      reject    = as.logical(maxlm_output$p.value <= alpha_level)
    )
  }, error = function(e) {
    results$maxLM <<- list(reject = NA, error = as.character(e$message))
  })
  
  return(results)
}

# ============================================================================
# NON-PARAMETRIC IRT TESTS
# ============================================================================

nonparametric_irt_tests <- function(binary_data, group_labels,
                                    alpha_level = 0.05) {
  
  results <- list()
  group_labels <- as.integer(as.vector(group_labels))
  
  tryCatch({
    t10_output <- NPtest(binary_data, method = "T10", splitcr = group_labels)
    results$T10 <- list(
      p_value = as.numeric(t10_output$prop),
      reject  = as.logical(t10_output$prop <= alpha_level)
    )
  }, error = function(e) {
    results$T10 <<- list(reject = NA, error = as.character(e$message))
  })
  
  tryCatch({
    t11_output <- NPtest(binary_data, method = "T11")
    results$T11 <- list(
      p_value = as.numeric(t11_output$prop),
      reject  = as.logical(t11_output$prop <= alpha_level)
    )
  }, error = function(e) {
    results$T11 <<- list(reject = NA, error = as.character(e$message))
  })
  
  tryCatch({
    J <- ncol(binary_data)
    colnames(binary_data) <- paste0("Item.", 1:J)
    fit_m2 <- mirt(binary_data, model = 1, itemtype = "Rasch", verbose = FALSE)
    m2_output <- M2(fit_m2, suppress.warnings = TRUE)
    results$M2 <- list(
      statistic = as.numeric(m2_output$M2),
      p_value   = as.numeric(m2_output$p),
      reject    = as.logical(m2_output$p <= alpha_level)
    )
  }, error = function(e) {
    results$M2 <<- list(reject = NA, error = as.character(e$message))
  })
  
  return(results)
}

# ============================================================================
# PERMUTATION TESTS  (cluster_configs built internally from true_P / true_Q)
# ============================================================================
perm_tests <- function(binary_data, true_P, true_Q,
                       Nruns = 5, permutations = 100,
                       alpha_level = 0.05, source_path = NULL) {
  
  na_result   <- function(P, Q, msg)
    list(config = c(P = P, Q = Q), config_name = paste0("P", P, "_Q", Q),
         reject = NA_real_, error = msg)
  
  results      <- list(REMAXINT = list(), E_REMI = list())
  perm_output  <- NULL
  
  tryCatch({
    perm_output <- Permutation_Function(binary_data, true_P, true_Q,
                                        Nruns, permutations, alpha_level)
    if (is.null(perm_output)) stop("Permutation_Function returned NULL")
    
    need_r <- c("Obs_Log_LR_REMAXINT","Crit_Value_Perm_REMAXINT","P_value_Perm_REMAXINT")
    miss   <- setdiff(need_r, names(perm_output))
    if (length(miss)) stop(paste("Missing REMAXINT fields:", paste(miss, collapse = ", ")))
    
    results$REMAXINT[[1]] <- list(
      config         = c(P = true_P, Q = true_Q),
      config_name    = paste0("P", true_P, "_Q", true_Q),
      statistic      = perm_output$Obs_Log_LR_REMAXINT,
      critical_value = perm_output$Crit_Value_Perm_REMAXINT,
      p_value        = perm_output$P_value_Perm_REMAXINT,
      reject         = as.numeric(
        perm_output$Obs_Log_LR_REMAXINT > perm_output$Crit_Value_Perm_REMAXINT),
      significance   = ifelse(perm_output$P_value_Perm_REMAXINT <= alpha_level,
                              "Significant", "Not Significant"))
  }, error = function(e)
    results$REMAXINT[[1]] <<- na_result(true_P, true_Q, e$message))
  
  tryCatch({
    if (is.null(perm_output))
      perm_output <- Permutation_Function(binary_data, true_P, true_Q,
                                          Nruns, permutations, alpha_level)
    
    need_e <- c("Obs_Log_LR_EReMI","Crit_Value_Perm_EReMI","P_value_Perm_EReMI")
    miss   <- setdiff(need_e, names(perm_output))
    if (length(miss)) stop(paste("Missing E_REMI fields:", paste(miss, collapse = ", ")))
    
    results$E_REMI[[1]] <- list(
      config         = c(P = true_P, Q = true_Q),
      config_name    = paste0("P", true_P, "_Q", true_Q),
      statistic      = perm_output$Obs_Log_LR_EReMI,
      critical_value = perm_output$Crit_Value_Perm_EReMI,
      p_value        = perm_output$P_value_Perm_EReMI,
      reject         = as.numeric(
        perm_output$Obs_Log_LR_EReMI > perm_output$Crit_Value_Perm_EReMI),
      significance   = ifelse(perm_output$P_value_Perm_EReMI <= alpha_level,
                              "Significant", "Not Significant"))
  }, error = function(e)
    results$E_REMI[[1]] <<- na_result(true_P, true_Q, e$message))
  
  return(results)
}

# ============================================================================
# MASTER FUNCTION
# ============================================================================
run_all_tests <- function(binary_data, group_labels, row_totals,
                          true_P, true_Q,
                          Nruns = 5, permutations = 100,
                          alpha_level = 0.05, source_path = NULL) {
  list(
    parametric    = parametric_irt_tests(binary_data, group_labels,
                                         row_totals, alpha_level),
    nonparametric = nonparametric_irt_tests(binary_data, group_labels,
                                            alpha_level),
    permutation   = perm_tests(binary_data, true_P, true_Q,
                               Nruns, permutations, alpha_level, source_path)
  )
}

# ============================================================================
# STATISTICAL POWER SIMULATION
# true_P_values / true_Q_values: vectors, one entry per condition
# analysis_P / analysis_Q    : the (P,Q) assumed by the analyst (misspecification)
# ============================================================================
stat_power_simulation_parallel <- function(
    n_replications = 1000,
    I              = 100,
    J              = 20,
    true_P_values  = c(3),   # data generated with P=3
    true_Q_values  = c(3),   # data generated with Q=3
    analysis_P     = 2,      # model fitted assuming P=2  (misspecification)
    analysis_Q     = 2,      # model fitted assuming Q=2  (misspecification)
    Nruns          = 5,
    permutations   = 100,
    alpha_level    = 0.05,
    n_cores        = NULL,
    source_path    = NULL,
    output_file    = NULL    # e.g. "C:/path/power_results/session1_I100_J20"
) {
  
  # --------------------------------------------------------------------------
  # create output directory
  # --------------------------------------------------------------------------
  if (!is.null(output_file)) {
    out_path <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
    out_dir  <- dirname(out_path)
    if (!dir.exists(out_dir)) {
      ok <- dir.create(out_dir, recursive = TRUE, showWarnings = TRUE)
      if (!ok) stop(paste("Could not create output directory:", out_dir))
    }
    output_file <- out_path
    cat("Output directory confirmed:", out_dir, "\n")
  }
  
  pq_pairs <- data.frame(true_P = true_P_values, true_Q = true_Q_values)
  
  if (is.null(n_cores)) n_cores <- max(1L, detectCores() - 1L)
  
  all_condition_results <- list()
  global_start          <- Sys.time()
  
  # ==========================================================================
  # OUTER LOOP over conditions
  # ==========================================================================
  for (cond_idx in seq_len(nrow(pq_pairs))) {
    
    true_P <- pq_pairs$true_P[cond_idx]
    true_Q <- pq_pairs$true_Q[cond_idx]
    label  <- paste0("P", true_P, "_Q", true_Q)
    
    cat("\n", strrep("=", 60), "\n", sep = "")
    cat(sprintf("Condition %d / %d  |  true_P = %d , true_Q = %d  |  analysis_P = %d , analysis_Q = %d\n",
                cond_idx, nrow(pq_pairs), true_P, true_Q, analysis_P, analysis_Q))
    cat(strrep("=", 60), "\n")
    
    # ------------------------------------------------------------------------
    # FIX: capture analysis_P / analysis_Q into local variables FIRST,
    #      then export those locals — this guarantees clusterExport can find them
    # ------------------------------------------------------------------------
    local_analysis_P <- analysis_P
    local_analysis_Q <- analysis_Q
    
    # parallel cluster
    cat("Setting up parallel processing with", n_cores, "cores...\n")
    cl <- makeCluster(n_cores)
    registerDoParallel(cl)
    
    clusterExport(cl,
                  varlist = c(
                    "dicho_data_gen", "parametric_irt_tests",
                    "nonparametric_irt_tests", "perm_tests", "run_all_tests",
                    "true_P", "true_Q",
                    "local_analysis_P", "local_analysis_Q",   # FIX: use locals
                    "Nruns", "permutations",
                    "alpha_level", "I", "J", "source_path"
                  ),
                  envir = environment())
    
    clusterEvalQ(cl, {
      library(eRm); library(mirt); library(psychotools); library(strucchange)
      library(parallel); library(doParallel); library(foreach); library(pracma)
      library(reshape); library(gdata); library(Rfast); library(R.utils)
      library(fossil); library(lintools); library(picante); library(sna)
      library(psych)
    })
    
    if (!is.null(source_path)) {
      cat("Sourcing required files on all workers...\n")
      clusterExport(cl, "source_path", envir = environment())
      clusterEvalQ(cl, {
        for (f in c('Randompartition_function.R',
                    'Unequal_Randompartition_Function.R',
                    'Log_Likelihood_function_REMAXINT.R',
                    'Log_Likelihood_function_E_ReMI.R',
                    'Log_LR_Test_Statistic.R',
                    'Update_column_clusters.R',
                    'Update_row_clusters_REMAXINT.R',
                    'Update_row_clusters_E_ReMI.R',
                    'Update_G_Omega.R',
                    'REMAXINT.R', 'E_ReMI.R', 'Permutation_Function.R'))
          source(file.path(source_path, f))
      })
    }
    
    cat("Running", n_replications, "replications...\n")
    cond_start <- Sys.time()
    
    results_list <- foreach(
      rep            = seq_len(n_replications),
      .combine       = "c",
      .packages      = c("eRm","mirt","psychotools","strucchange","pracma",
                         "reshape","gdata","Rfast","R.utils","fossil",
                         "lintools","picante","sna","psych"),
      .errorhandling = "pass",
      .verbose       = FALSE
    ) %dopar% {
      
      set.seed(rep + 1000L * cond_idx)
      
      # -- data generation uses TRUE P and Q (P=3, Q=3) ---------------------
      sim <- tryCatch(
        dicho_data_gen(I = I, J = J, P = true_P, Q = true_Q),
        error = function(e) NULL)
      
      # -- all-NA fallback row ----------------------------------------------
      # Column names reflect BOTH true structure and analyst's assumption
      make_na_row <- function() {
        r <- data.frame(replication = rep,
                        Andersen_LR = NA_real_, Martin_Lof = NA_real_,
                        LMuo        = NA_real_, maxLM      = NA_real_,
                        T10         = NA_real_, T11        = NA_real_,
                        M2          = NA_real_,
                        stringsAsFactors = FALSE)
        r[[paste0("REMAXINT_trueP", true_P, "_Q", true_Q,
                  "_analP", local_analysis_P, "_Q", local_analysis_Q)]] <- NA_real_
        r[[paste0("E_REMI_trueP",   true_P, "_Q", true_Q,
                  "_analP", local_analysis_P, "_Q", local_analysis_Q)]] <- NA_real_
        r
      }
      
      if (is.null(sim)) return(list(make_na_row()))
      
      # -- run tests with ANALYST'S assumed P and Q (P=2, Q=2) --------------
      tr <- tryCatch(
        run_all_tests(binary_data  = sim$binary_data,
                      group_labels = sim$group_labels,
                      row_totals   = sim$row_totals,
                      true_P       = local_analysis_P,   # analyst assumes P=2
                      true_Q       = local_analysis_Q,   # analyst assumes Q=2
                      Nruns        = Nruns,
                      permutations = permutations,
                      alpha_level  = alpha_level,
                      source_path  = source_path),
        error = function(e) NULL)
      
      if (is.null(tr)) return(list(make_na_row()))
      
      sr <- function(x) {
        v <- x$reject
        if (is.null(v) || !length(v)) NA_real_ else as.numeric(v[1])
      }
      
      r <- data.frame(
        replication = rep,
        Andersen_LR = sr(tr$parametric$Andersen_LR),
        Martin_Lof  = sr(tr$parametric$Martin_Lof),
        LMuo        = sr(tr$parametric$LMuo),
        maxLM       = sr(tr$parametric$maxLM),
        T10         = sr(tr$nonparametric$T10),
        T11         = sr(tr$nonparametric$T11),
        M2          = sr(tr$nonparametric$M2),
        stringsAsFactors = FALSE)
      
      # Column names clearly encode true structure AND analyst assumption
      r[[paste0("REMAXINT_trueP", true_P, "_Q", true_Q,
                "_analP", local_analysis_P, "_Q", local_analysis_Q)]] <-
        sr(tr$permutation$REMAXINT[[1]])
      r[[paste0("E_REMI_trueP",   true_P, "_Q", true_Q,
                "_analP", local_analysis_P, "_Q", local_analysis_Q)]] <-
        sr(tr$permutation$E_REMI[[1]])
      
      list(r)
    }
    
    stopCluster(cl)
    
    cond_elapsed <- difftime(Sys.time(), cond_start, units = "mins")
    cat("Condition completed in", round(as.numeric(cond_elapsed), 2), "minutes\n")
    
    # -- aggregate ------------------------------------------------------------
    valid <- Filter(is.data.frame, results_list)
    
    if (length(valid) == 0) {
      warning("All replications failed for condition ", label)
      all_condition_results[[label]] <- list(
        condition        = list(true_P = true_P, true_Q = true_Q),
        detailed_results = NULL, summary = NULL,
        error            = "All replications failed")
      next
    }
    
    res_df <- do.call(rbind, valid)
    cat("Successfully completed", nrow(res_df), "out of", n_replications, "replications\n")
    
    rej_cols <- setdiff(names(res_df), "replication")
    power    <- colMeans(res_df[, rej_cols, drop = FALSE], na.rm = TRUE)
    n_ok     <- colSums(!is.na(res_df[, rej_cols, drop = FALSE]))
    se       <- ifelse(n_ok > 0, sqrt(power * (1 - power) / n_ok), NA_real_)
    
    summ_df <- data.frame(
      Condition         = label,
      true_P            = true_P,
      true_Q            = true_Q,
      analysis_P        = local_analysis_P,
      analysis_Q        = local_analysis_Q,
      Test              = names(power),
      Statistical_Power = round(power, 4),
      SE                = round(se,    4),
      Lower_CI          = round(power - 1.96 * se, 4),
      Upper_CI          = round(power + 1.96 * se, 4),
      N_Valid           = n_ok,
      N_Failed          = n_replications - n_ok,
      Failure_Rate      = round((n_replications - n_ok) / n_replications, 3),
      row.names         = NULL,
      stringsAsFactors  = FALSE)
    
    print(summ_df[, c("Test","Statistical_Power","SE","N_Valid","Failure_Rate")])
    
    # -- save per-condition CSV -----------------------------------------------
    if (!is.null(output_file)) {
      tryCatch({
        cond_csv <- paste0(output_file, "_", label, "_summary.csv")
        cond_csv <- normalizePath(cond_csv, winslash = "/", mustWork = FALSE)
        cond_dir <- dirname(cond_csv)
        if (!dir.exists(cond_dir))
          dir.create(cond_dir, recursive = TRUE, showWarnings = FALSE)
        write.csv(summ_df, file = cond_csv, row.names = FALSE)
        cat("Saved:", cond_csv, "\n")
      }, error = function(e)
        warning("Could not save per-condition CSV: ", e$message))
    }
    
    all_condition_results[[label]] <- list(
      condition        = list(true_P = true_P, true_Q = true_Q,
                              analysis_P = local_analysis_P,
                              analysis_Q = local_analysis_Q),
      detailed_results = res_df,
      summary          = summ_df,
      elapsed_minutes  = as.numeric(cond_elapsed))
    
  }  # end condition loop
  
  # ==========================================================================
  # COMBINED SUMMARY
  # ==========================================================================
  combined_summary <- tryCatch(
    do.call(rbind, lapply(all_condition_results, `[[`, "summary")),
    error = function(e) NULL)
  
  if (!is.null(output_file) && !is.null(combined_summary)) {
    tryCatch({
      all_csv <- paste0(output_file, "_ALL_conditions_summary.csv")
      all_csv <- normalizePath(all_csv, winslash = "/", mustWork = FALSE)
      all_dir <- dirname(all_csv)
      if (!dir.exists(all_dir))
        dir.create(all_dir, recursive = TRUE, showWarnings = FALSE)
      write.csv(combined_summary, file = all_csv, row.names = FALSE)
      cat("\nCombined summary saved:", all_csv, "\n")
    }, error = function(e)
      warning("Could not save combined CSV: ", e$message))
  }
  
  total_elapsed <- difftime(Sys.time(), global_start, units = "mins")
  cat("\nTotal simulation time:", round(as.numeric(total_elapsed), 2), "minutes\n")
  
  return(list(
    condition_results = all_condition_results,
    combined_summary  = combined_summary,
    parameters        = list(
      n_replications = n_replications,
      I              = I,
      J              = J,
      true_P_values  = true_P_values,
      true_Q_values  = true_Q_values,
      analysis_P     = analysis_P,
      analysis_Q     = analysis_Q,
      alpha_level    = alpha_level,
      total_elapsed  = total_elapsed)))
}

cat("\n=== Power analysis main functions loaded successfully ===\n")