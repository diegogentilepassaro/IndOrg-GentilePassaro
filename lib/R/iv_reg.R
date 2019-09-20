#### IV regression with constant, up to two-way fixed effects, and option for robust standard errors

iv_reg <- function (y, X = NULL, endo, instr, factor_var1 = NULL, 
                    factor_var2 = NULL, robust = FALSE) {
  outcome.assertion <- is.vector(y)
  controls.assertion <- (is.matrix(X) | is.vector(X) | is.null(X))
  endo.assertion <- is.vector(endo)
  instruments.assertion <- (is.vector(instr) | is.matrix(instr))
  
  if (outcome.assertion == FALSE | controls.assertion == FALSE | 
      endo.assertion == FALSE | instruments.assertion == FALSE) {
    print("y needs to be a vector, X a vector or a matrix, endo needs to be a vector, and Z a vector or a matrix")
    break  
  }
  
  nbr_obs_y <- length(y)
  
  cons <- rep(1,nbr_obs_y)
  
  if (is.null(X) == TRUE) {
    X = cons
  }
  else{
    X <- cbind(X, cons)
  }
  
  nbr_controls <- dim(X)[2]
  
  if (is.null(factor_var1) == FALSE){
    factor1.vector.assertion <- is.vector(factor_var1)
    
    if (factor1.vector.assertion == FALSE){
      print("factor_var1 should be a vector")
      break
    }
    if (all(factor_var1 != floor(factor_var1))){
      print("every element of vector factor_var1 should be an integer")
      break
    }
    
    factor1_matrix <- matrix(NA, nrow = nbr_obs_y, ncol = length(unique(factor_var1)))
    iteration <- as.numeric(0)
    names <- list()
    for(i in sort(unique(factor_var1))){
      iteration <- as.numeric(iteration + 1)
      name <- paste(deparse(substitute(factor_var1)), "_", i, sep = "")
      condition <- as.numeric(factor_var1 == i) 
      assign(name, condition)
      factor1_matrix[,iteration] <- get(name)
      names[iteration] <- name 
    }
    colnames(factor1_matrix) <- names
    X <- cbind(X, factor1_matrix[,2:length(unique(factor_var1))])
  }
  
  if (is.null(factor_var2) == FALSE){
    factor2.vector.assertion <- is.vector(factor_var2)
    
    if (factor2.vector.assertion == FALSE){
      print("factor_var2 should be a vector")
      break
    }
    if (all(factor_var2 != floor(factor_var2))){
      print("every element of vector factor_var2 should be an integer")
      break
    }
    
    factor2_matrix <- matrix(NA, nrow = nbr_obs_y, ncol = length(unique(factor_var2)))
    iteration <- as.numeric(0)
    names <- list()
    for(i in sort(unique(factor_var2))){
      iteration <- as.numeric(iteration + 1)
      name <- paste(deparse(substitute(factor_var2)), "_", i, sep = "")
      condition <- as.numeric(factor_var2 == i) 
      assign(name, condition)
      factor2_matrix[,iteration] <- get(name)
      names[iteration] <- name
    }
    colnames(factor2_matrix) <- names
    X <- cbind(X, factor2_matrix[,2:length(unique(factor_var2))])
  }
  
  Z <- cbind(instr, X)
  nbr_regressors = dim(Z)[2]
  szz <- solve(t(Z)%*%Z)
  betas_fs <- szz %*% (t(Z)%*%endo)
  endo_hat = Z %*% betas_fs
  residuals_fs <- endo - endo_hat
  colnames(endo_hat) <- deparse(substitute(endo))
  X_hat <- cbind(endo_hat, X)
  sxx_hat <- solve(t(X_hat)%*%X_hat)
  betas =  sxx_hat %*% (t(X_hat) %*% y)

  y_hat <- cbind(endo,X) %*% betas
  residuals = y - y_hat
  df <- (nbr_obs_y - nbr_regressors)^(-1)

  if (robust == TRUE){
    residuals_sq_fs <- residuals_fs^2
    diag_residuals_sq_fs <- diag(as.numeric(residuals_sq_fs))
    robust_var_betas_fs <- szz %*% (t(Z) %*% diag_residuals_sq_fs %*% Z) %*% szz
    robust_se_betas_fs <- sqrt(diag(robust_var_betas_fs))
    robust_t_stats_fs <- t(betas_fs)/robust_se_betas_fs
    summary_fs <- t(rbind(t(betas_fs), robust_se_betas_fs, robust_t_stats_fs))
    colnames(summary_fs) <- c("beta_fs", "robust_se_beta_fs", "robust_t_stat_fs")
    
    residuals_sq <- residuals^2
    diag_residuals_sq <- diag(as.numeric(residuals_sq))
    robust_var_betas <- sxx_hat %*% (t(X_hat) %*% diag_residuals_sq %*% X_hat) %*% sxx_hat
    robust_se_betas <- sqrt(diag(robust_var_betas))
    robust_t_stats <- t(betas)/robust_se_betas
    summary <- t(rbind(t(betas), robust_se_betas, robust_t_stats))
    colnames(summary) <- c("beta", "robust_se_beta", "robust_t_stat")
    return_list <- list("betas" = betas, "ses" = robust_se_betas, "t_stats" = robust_t_stats,
                        "obs" = as.numeric(nbr_obs_y), "df" = as.numeric(nbr_obs_y - nbr_regressors),
                        "summary" = summary, "summary_fs" = summary_fs,
                        "predicted" = y_hat, "residuals" = residuals)
  }
  if (robust == FALSE) {
    sigma_sq_fs <- as.numeric(t(residuals_fs) %*% residuals_fs)
    var_betas_fs <- (df * sigma_sq_fs) * szz
    se_betas_fs <- sqrt(diag(var_betas_fs))
    t_stats_fs <- t(betas_fs)/se_betas_fs
    summary_fs <- t(rbind(t(betas_fs), se_betas_fs, t_stats_fs))
    colnames(summary_fs) <- c("beta_fs", "se_beta_fs", "t_stat_fs")
    
    sigma_sq <- as.numeric(t(residuals) %*% residuals)
    var_betas <- (df * sigma_sq) * sxx_hat
    se_betas <- sqrt(diag(var_betas))
    t_stats <- t(betas)/se_betas
    summary <- t(rbind(t(betas), se_betas, t_stats))
    colnames(summary) <- c("beta", "se_beta", "t_stat")
    return_list <- list("betas" = betas, "ses" = se_betas, "t_stats" = t_stats,
                        "obs" = as.numeric(nbr_obs_y), "df" = as.numeric(nbr_obs_y - nbr_regressors),
                        "summary" = summary, "summary_fs" = summary_fs,
                        "predicted" = y_hat, "residuals" = residuals)
  }

  return(return_list)
}
