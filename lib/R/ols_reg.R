#### OLS regression with constant, up to two-way fixed effects, and option for robust standard errors

ols_reg <- function (y, X, factor_var1 = NULL, factor_var2 = NULL, robust = FALSE) {
  outcome.assertion <- is.vector(y)
  controls.assertion <- (is.matrix(X) | is.vector(X))
  
  if (outcome.assertion == FALSE | controls.assertion == FALSE) {
    print("y needs to be a vector and X a vector or a matrix")
    break  
    }

  nbr_obs_y <- length(y)

  cons <- rep(1,nbr_obs_y)
  X <- cbind(X, cons)
  
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
  
  nbr_regressors = dim(X)[2]
  sxx <- solve(t(X)%*%X)
  betas = sxx %*% (t(X) %*% y)
  y_hat <- X%*%betas
  residuals = y - y_hat
  df <- (nbr_obs_y - nbr_regressors)^(-1)


  if (robust == TRUE){
    residuals_sq <- residuals^2
    diag_residuals_sq <- diag(as.numeric(residuals_sq))
    robust_var_betas <- sxx %*% (t(X) %*% diag_residuals_sq %*% X) %*% sxx
    robust_se_betas <- sqrt(diag(robust_var_betas))
    robust_t_stats <- t(betas)/robust_se_betas
    summary <- t(rbind(t(betas), robust_se_betas, robust_t_stats))
    colnames(summary) <- c("beta", "robust_se_beta", "robust_t_stat")
    return_list <- list("betas" = betas, "ses" = robust_se_betas, "t_stats" = robust_t_stats,
                        "summary" = summary, "predicted" = y_hat, "residuals" = residuals)
  }
  if (robust == FALSE) {
    sigma_sq <- as.numeric(t(residuals) %*% residuals)
    var_betas <- (df * sigma_sq) * sxx
    se_betas <- sqrt(diag(var_betas))
    t_stats <- t(betas)/se_betas
    summary <- t(rbind(t(betas), se_betas, t_stats))
    colnames(summary) <- c("beta", "se_beta", "t_stat")
    return_list <- list("betas" = betas, "ses" = se_betas, "t_stats" = t_stats,
                        "summary" = summary, "predicted" = y_hat, "residuals" = residuals)
  }

  return(return_list)
}
