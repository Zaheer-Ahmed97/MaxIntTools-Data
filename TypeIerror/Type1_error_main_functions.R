# ============================================================================
# MAIN FUNCTIONS FOR TYPE-I ERROR ANALYSIS. IT CONTAINS ALL CORE FUNCTIONS 
# NEEDED FOR THE ANALYSIS
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
# BINARY DATA GENERATION
# ============================================================================

dicho_data_gen <- function(I, J, mu = 0, sigma_alpha = 0.5, sigma_epsilon = 1) {
  
  alpha_i <- matrix(rnorm(I * J, mean = 0, sd = sigma_alpha), 
                    nrow = I, ncol = J)
  beta_j <- matrix(rep(seq(-0.5, 0.5, length.out = J), each = I), 
                   nrow = I, ncol = J, byrow = FALSE)
  epsilon_ij <- matrix(rnorm(I * J, mean = 0, sd = sigma_epsilon), 
                       nrow = I, ncol = J)
  data <- mu + alpha_i + beta_j + epsilon_ij
  
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
# PARAMETRIC TESTS
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
  
  # Andersen LR test
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
  
  # Martin-Löf Test
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
  
  # Score-based test (LMuo)
  tryCatch({
    data_df <- as.data.frame(binary_data)
    fit_rasch <- raschmodel(data_df)
    lmuo_output <- strucchange::sctest(fit_rasch, 
                                       order.by = group_labels, 
                                       functional = "LMuo")
    results$LMuo <- list(
      statistic = as.numeric(lmuo_output$statistic),
      p_value   = as.numeric(lmuo_output$p.value),
      reject    = as.logical(lmuo_output$p.value <= alpha_level)
    )
  }, error = function(e) {
    results$LMuo <<- list(reject = NA, error = as.character(e$message))
  })
  
  # Score-based test (maxLM)
  tryCatch({
    if (!exists("fit_rasch")) {
      data_df <- as.data.frame(binary_data)
      fit_rasch <- raschmodel(data_df)
    }
    maxlm_output <- strucchange::sctest(fit_rasch, 
                                        order.by = group_labels, 
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
# NON-PARAMETRIC TESTS
# ============================================================================
nonparametric_irt_tests <- function(binary_data, group_labels, 
                                    alpha_level = 0.05) {
  
  results <- list()
  group_labels <- as.integer(as.vector(group_labels))
  
  # T10 Test
  tryCatch({
    t10_output <- NPtest(binary_data, method = "T10", splitcr = group_labels)
    results$T10 <- list(
      p_value = as.numeric(t10_output$prop),
      reject  = as.logical(t10_output$prop <= alpha_level)
    )
  }, error = function(e) {
    results$T10 <<- list(reject = NA, error = as.character(e$message))
  })
  
  # T11 Test
  tryCatch({
    t11_output <- NPtest(binary_data, method = "T11")
    results$T11 <- list(
      p_value = as.numeric(t11_output$prop),
      reject  = as.logical(t11_output$prop <= alpha_level)
    )
  }, error = function(e) {
    results$T11 <<- list(reject = NA, error = as.character(e$message))
  })
  
  # M2 Test
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
# PERMUTATION TESTS
# ============================================================================
perm_tests <- function(binary_data, 
                       cluster_configs = matrix(c(2,2),nrow=1, ncol=2, byrow=TRUE),
                       Nruns = 5,
                       permutations = 100,
                       alpha_level = 0.05,
                       source_path = NULL) {
  
  results <- list()
  n_configs <- nrow(cluster_configs)
  
  results$REMAXINT <- vector("list", n_configs)
  results$E_REMI   <- vector("list", n_configs)
  
  for (clus in 1:n_configs) {
    P <- cluster_configs[clus, 1]
    Q <- cluster_configs[clus, 2]
    
    config_name <- paste0("P", P, "_Q", Q)
    perm_output <- NULL
    
    # REMAXINT Test
    tryCatch({
      perm_output <- Permutation_Function(binary_data, P, Q, Nruns, permutations, alpha_level)
      
      if (is.null(perm_output)) {
        stop(paste("Permutation_Function returned NULL for P =", P, ", Q =", Q))
      }
      
      required_fields_remaxint <- c("Obs_Log_LR_REMAXINT", "Crit_Value_Perm_REMAXINT", "P_value_Perm_REMAXINT")
      missing_fields <- setdiff(required_fields_remaxint, names(perm_output))
      
      if (length(missing_fields) > 0) {
        stop(paste("Missing fields:", paste(missing_fields, collapse=", ")))
      }
      
      results$REMAXINT[[clus]] <- list(
        config         = c(P = P, Q = Q),
        config_name    = config_name,
        statistic      = perm_output$Obs_Log_LR_REMAXINT,
        critical_value = perm_output$Crit_Value_Perm_REMAXINT,
        p_value        = perm_output$P_value_Perm_REMAXINT,
        reject         = perm_output$Obs_Log_LR_REMAXINT > perm_output$Crit_Value_Perm_REMAXINT,
        significance   = ifelse(
          perm_output$P_value_Perm_REMAXINT <= alpha_level, 
          "Significant", 
          "Not Significant"
        )
      )
    }, error = function(e) {
      error_msg <- paste("REMAXINT P", P, "Q", Q, ":", as.character(e$message), sep=" ")
      results$REMAXINT[[clus]] <<- list(
        config = c(P = P, Q = Q),
        config_name = config_name,
        reject = NA,
        error = error_msg
      )
    })
    
    # E_REMI Test
    tryCatch({
      if (is.null(perm_output)) {
        perm_output <- Permutation_Function(binary_data, P, Q, Nruns, permutations, alpha_level)
      }
      
      required_fields_eremi <- c("Obs_Log_LR_EReMI", "Crit_Value_Perm_EReMI", "P_value_Perm_EReMI")
      missing_fields <- setdiff(required_fields_eremi, names(perm_output))
      
      if (length(missing_fields) > 0) {
        stop(paste("Missing E_REMI fields:", paste(missing_fields, collapse=", ")))
      }
      
      results$E_REMI[[clus]] <- list(
        config         = c(P = P, Q = Q),
        config_name    = config_name,
        statistic      = perm_output$Obs_Log_LR_EReMI,
        critical_value = perm_output$Crit_Value_Perm_EReMI,
        p_value        = perm_output$P_value_Perm_EReMI,
        reject         = perm_output$Obs_Log_LR_EReMI > perm_output$Crit_Value_Perm_EReMI,
        significance   = ifelse(
          perm_output$P_value_Perm_EReMI <= alpha_level, 
          "Significant", 
          "Not Significant"
        )
      )
    }, error = function(e) {
      error_msg <- paste("E_REMI P", P, "Q", Q, ":", as.character(e$message), sep=" ")
      results$E_REMI[[clus]] <<- list(
        config = c(P = P, Q = Q),
        config_name = config_name,
        reject = NA,
        error = error_msg
      )
    })
  }
  
  return(results)
}

# ============================================================================
# MASTER FUNCTION
# ============================================================================
run_all_tests <- function(binary_data, group_labels, row_totals,
                          cluster_configs = matrix(c(2,2), nrow=1, ncol=2, byrow=TRUE),
                          Nruns = 5, permutations = 100, alpha_level = 0.05, 
                          source_path = NULL) {
  
  parametric    <- parametric_irt_tests(binary_data, group_labels, 
                                        row_totals, alpha_level)
  nonparametric <- nonparametric_irt_tests(binary_data, group_labels, 
                                           alpha_level)
  
  permutationtests <- perm_tests(binary_data = binary_data, 
                                 cluster_configs = cluster_configs,
                                 Nruns = Nruns, 
                                 permutations = permutations, 
                                 alpha_level = alpha_level, 
                                 source_path = source_path)
  
  return(list(
    parametric = parametric,
    nonparametric = nonparametric,
    permutation = permutationtests
  ))
}

# ============================================================================
# PARALLEL SIMULATION FUNCTION
# ============================================================================
type1_error_simulation_parallel <- function(n_replications = 1000,
                                            I = 100,
                                            J = 20,
                                            cluster_configs = matrix(c(2,2), nrow=1, ncol=2, byrow=TRUE),
                                            Nruns = 5,
                                            permutations = 100,
                                            alpha_level = 0.05,
                                            n_cores = NULL,
                                            source_path = NULL,
                                            output_file = NULL) {
  
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 1)
  }
  
  cat("Setting up parallel processing with", n_cores, "cores...\n")
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  clusterExport(cl, c("dicho_data_gen",
                      "parametric_irt_tests",
                      "nonparametric_irt_tests",
                      "perm_tests",
                      "run_all_tests",
                      "cluster_configs",
                      "Nruns",
                      "permutations",
                      "alpha_level",
                      "I",
                      "J",
                      "source_path"),
                envir = environment())
  
  clusterEvalQ(cl, {
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
  })
  
  if (!is.null(source_path)) {
    cat("Sourcing required files on all workers...\n")
    clusterExport(cl, "source_path", envir = environment())
    
    clusterEvalQ(cl, {
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
      
      for (file in required_files) {
        source(file.path(source_path, file))
      }
    })
  }
  
  cat("Running", n_replications, "replications...\n")
  start_time <- Sys.time()
  
  results_list <- foreach(
    rep = 1:n_replications,
    .combine = 'c',
    .packages = c('eRm', 'mirt', 'psychotools', 'strucchange', 'pracma', 
                  'reshape', 'gdata', 'Rfast', 'R.utils', 'fossil', 
                  'lintools', 'picante', 'sna', 'psych'),
    .errorhandling = 'pass',
    .verbose = FALSE
  ) %dopar% {
    
    set.seed(rep + 1000)
    
    sim_data <- dicho_data_gen(I = I, J = J)
    
    test_results <- tryCatch({
      run_all_tests(
        binary_data = sim_data$binary_data,
        group_labels = sim_data$group_labels,
        row_totals = sim_data$row_totals,
        cluster_configs = cluster_configs,
        Nruns = Nruns,
        permutations = permutations,
        alpha_level = alpha_level,
        source_path = source_path
      )
    }, error = function(e) {
      list(
        error_occurred = TRUE,
        parametric = list(
          Andersen_LR = list(reject = NA),
          Martin_Lof = list(reject = NA),
          LMuo = list(reject = NA),
          maxLM = list(reject = NA)
        ),
        nonparametric = list(
          T10 = list(reject = NA),
          T11 = list(reject = NA),
          M2 = list(reject = NA)
        ),
        permutation = list(
          REMAXINT = lapply(1:nrow(cluster_configs), function(x) list(reject = NA)),
          E_REMI = lapply(1:nrow(cluster_configs), function(x) list(reject = NA))
        )
      )
    })
    
    result_row <- data.frame(
      replication = rep,
      Andersen_LR = ifelse(!is.null(test_results$parametric$Andersen_LR$reject), 
                           as.numeric(test_results$parametric$Andersen_LR$reject), NA_real_),
      Martin_Lof = ifelse(!is.null(test_results$parametric$Martin_Lof$reject), 
                          as.numeric(test_results$parametric$Martin_Lof$reject), NA_real_),
      LMuo = ifelse(!is.null(test_results$parametric$LMuo$reject), 
                    as.numeric(test_results$parametric$LMuo$reject), NA_real_),
      maxLM = ifelse(!is.null(test_results$parametric$maxLM$reject), 
                     as.numeric(test_results$parametric$maxLM$reject), NA_real_),
      T10 = ifelse(!is.null(test_results$nonparametric$T10$reject), 
                   as.numeric(test_results$nonparametric$T10$reject), NA_real_),
      T11 = ifelse(!is.null(test_results$nonparametric$T11$reject), 
                   as.numeric(test_results$nonparametric$T11$reject), NA_real_),
      M2 = ifelse(!is.null(test_results$nonparametric$M2$reject), 
                  as.numeric(test_results$nonparametric$M2$reject), NA_real_),
      stringsAsFactors = FALSE
    )
    
    for (i in 1:nrow(cluster_configs)) {
      P <- cluster_configs[i, 1]
      Q <- cluster_configs[i, 2]
      col_name <- paste0("REMAXINT_P", P, "_Q", Q)
      
      result_row[[col_name]] <- ifelse(
        !is.null(test_results$permutation$REMAXINT[[i]]$reject),
        as.numeric(test_results$permutation$REMAXINT[[i]]$reject),
        NA_real_
      )
    }
    
    for (i in 1:nrow(cluster_configs)) {
      P <- cluster_configs[i, 1]
      Q <- cluster_configs[i, 2]
      col_name <- paste0("E_REMI_P", P, "_Q", Q)
      
      result_row[[col_name]] <- ifelse(
        !is.null(test_results$permutation$E_REMI[[i]]$reject),
        as.numeric(test_results$permutation$E_REMI[[i]]$reject),
        NA_real_
      )
    }
    
    list(result_row)
  }
  
  stopCluster(cl)
  
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  
  cat("\nSimulation completed in", round(elapsed_time, 2), 
      attr(elapsed_time, "units"), "\n")
  
  valid_results <- Filter(function(x) is.data.frame(x), results_list)
  
  if (length(valid_results) == 0) {
    stop("All replications failed. No valid results to analyze.")
  }
  
  results_list <- do.call(rbind, valid_results)
  
  cat("Successfully completed", nrow(results_list), "out of", n_replications, "replications\n")
  
  type1_errors <- colMeans(results_list[, -1], na.rm = TRUE)
  n_valid <- colSums(!is.na(results_list[, -1]))
  se_type1_errors <- ifelse(n_valid > 0,
                            sqrt(type1_errors * (1 - type1_errors) / n_valid),
                            NA_real_)
  
  summary_results <- data.frame(
    Test = names(type1_errors),
    Type1_Error = round(type1_errors, 4),
    SE = round(se_type1_errors, 4),
    Lower_CI = round(type1_errors - 1.96 * se_type1_errors, 4),
    Upper_CI = round(type1_errors + 1.96 * se_type1_errors, 4),
    N_Valid = n_valid,
    N_Failed = n_replications - n_valid,
    Failure_Rate = round((n_replications - n_valid) / n_replications, 3)
  )
  
  if (!is.null(output_file)) {
    write.csv(summary_results, paste0(output_file, "_summary.csv"), row.names = FALSE)
    cat("Results saved to:", paste0(output_file, "_summary.csv\n"))
  }
  
  return(list(
    detailed_results = results_list,
    summary = summary_results,
    parameters = list(
      n_replications = n_replications,
      I = I,
      J = J,
      elapsed_time = elapsed_time
    )
  ))
}

cat("\n=== Main functions loaded successfully ===\n")
