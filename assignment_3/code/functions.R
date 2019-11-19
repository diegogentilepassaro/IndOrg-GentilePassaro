compute_xi <- function(table, alpha_DM, alpha_HE, alpha_HU, beta) {
  table <- table %>%
    mutate(xi = log_share - log_share_norm - alpha_DM*brand_DM - alpha_HE*brand_HE - 
             alpha_HU*brand_HU + beta*(price_var))
  xi <- as.matrix(table %>%
                    select(xi))
  return(xi)
}

compute_GMM_objective <- function(Z, xi, W) {
  nbr_cols <- dim(Z)[2]
  
  ones <- t(as.matrix(rep(1, nbr_cols)))
  expanded_xi <- xi%*%ones
  
  g <- t(as.matrix(colMeans(Z*expanded_xi)))
  GMM_objective <- g %*% W %*% t(g)
  
  return(GMM_objective)
}

function_to_minimize_m1_and_m2 <- function(params, table, Z, W){
  alpha_DM <- params[1]
  alpha_HE <- params[2]
  alpha_HU <-params[3]
  beta <-params[4]
  
  compute_GMM_objective(Z, compute_xi(table, alpha_DM, alpha_HE, alpha_HU, beta), W)
}

compute_delta_BLP <- function(table, initial_delta,  mu, sigma, tol, normalized_brand) {
  N <- dim(table)[1]
  delta_new <- rep(initial_delta, N)
  diff <- rep(1, N)
  share <- as.matrix(table %>%
                   select(share))
  price <- as.matrix(table %>%
                   select(price))
  week <- as.matrix(table %>%
                      select(week))
  
  nodes <- c(-sqrt(3), 0 , sqrt(3))
  weights <- c(1/6,2/3,1/6)

  while(max(abs(diff)) >= tol) {
    
    delta <- delta_new
    sim_share <- rep(0, N)
    for (i in 1:length(weights)) {
      beta <- exp(mu + abs(sigma)*nodes[i])
      num <- exp(delta - beta*price)
      week_num <- cbind(week, num)
      colnames(week_num) <- c("week", "num")
 
      denom <- as.matrix(as_tibble(week_num) %>%
                           group_by(week) %>%
                           mutate(denom := sum(num)) %>%
                           ungroup %>%
                           select(denom))
      sim_share_node <- num/denom
      
      sim_share <- sim_share + sim_share_node*weights[i]
    }
    diff <- share - sim_share
    delta_new <- delta + diff
  }
  
  delta <- delta_new
  table <- table %>%
    mutate(delta = delta)
  normalized_brand_table <- table %>%
    filter(brand == normalized_brand)
  table <- left_join(table, normalized_brand_table, suffix = c("", "_norm"), by = "week") %>%
    mutate(diff_deltas = delta - delta_norm)
  deltas <- as.matrix(table$diff_deltas)
  return(deltas)
}

function_to_minimize_m3 <- function(params, table, Z, W, initial_delta, tol, normalized_brand){
  mu <-params[1]
  sigma <- params[2]
  
  deltas <- compute_delta_BLP(table_m3, initial_delta, mu, sigma, tol, normalized_brand)
  table_for_reg <- table %>%
    mutate(delta = deltas)
  reg_for_xi <- lm(deltas ~ brand_DM + brand_HE + brand_HU - 1, data = table_for_reg)
  xi <- as.matrix(reg_for_xi$residuals)
  compute_GMM_objective(Z, xi, W)
}

BLP <- function (initial_mu, initial_sigma, table, Z, W, initial_delta, tol, normalized_brand){
  model <- optim(c(mu = initial_mu, sigma = initial_sigma), 
                 function_to_minimize_m3, table = table, Z = Z, W = W,
                 initial_delta = initial_delta, tol = tol, normalized_brand = normalized_brand,
                 method = "L-BFGS-B")
  mu <- model$par[1]
  sigma <- model$par[2]
  
  deltas <- compute_delta_BLP(table, initial_delta, mu, sigma, tol, normalized_brand)
  table <- table %>%
    mutate(delta = deltas)
  reg_for_xi <- lm(delta ~ brand_DM + brand_HE + brand_HU - 1, data = table)
  alphas<- reg_for_xi$coefficients
  alpha_DM <-alphas[1]
  alpha_HE <-alphas[2]
  alpha_HU <-alphas[3]
  
  return_list <- list("alpha_DM" = alpha_DM,
                      "alpha_HE" = alpha_HE, 
                      "alpha_HU" = alpha_HU,
                      "mu" = mu,
                      "sigma" = sigma,
                      "deltas" = deltas)
}

function_to_minimize_m3_sigma0 <- function(params, table, Z, W, initial_delta, tol, normalized_brand){
  mu <-params[1]
  
  deltas <- compute_delta_BLP(table_m3, initial_delta, mu, 0, tol, normalized_brand)
  table_for_reg <- table %>%
    mutate(delta = deltas)
  reg_for_xi <- lm(deltas ~ brand_DM + brand_HE + brand_HU - 1, data = table_for_reg)
  xi <- as.matrix(reg_for_xi$residuals)
  compute_GMM_objective(Z, xi, W)
}

BLP_sigma0 <- function (initial_mu, table, Z, W, initial_delta, tol, normalized_brand){
  model <- optim(c(mu = initial_mu), 
                 function_to_minimize_m3_sigma0, table = table, Z = Z, W = W,
                 initial_delta = initial_delta, tol = tol, normalized_brand = normalized_brand, 
                 method = "Brent", lower = -initial_mu*10, upper = initial_mu*10)
  mu_sigma0 <- model$par[1]
  
  deltas_sigma0 <- compute_delta_BLP(table, initial_delta, mu_sigma0, 0, tol, normalized_brand)
  table <- table %>%
    mutate(delta_sigma0 = deltas_sigma0)
  reg_for_xi_sigma0 <- lm(delta_sigma0 ~ brand_DM + brand_HE + brand_HU - 1, data = table)
  alphas_sigma0 <- reg_for_xi_sigma0$coefficients
  alpha_DM_sigma0 <-alphas_sigma0[1]
  alpha_HE_sigma0 <-alphas_sigma0[2]
  alpha_HU_sigma0 <-alphas_sigma0[3]
  
  return_list <- list("alpha_DM_sigma0" = alpha_DM_sigma0,
                      "alpha_HE_sigma0" = alpha_HE_sigma0, 
                      "alpha_HU_sigma0" = alpha_HU_sigma0,
                      "mu_sigma0" = mu_sigma0,
                      "deltas_sigma0" = deltas_sigma0)
}

constraint_MPEC <- function(table, delta, mu, sigma){
    N <- dim(table)[1]
    share <- as.matrix(table %>%
                         select(share))
    price <- as.matrix(table %>%
                         select(price))
    week <- as.matrix(table %>%
                        select(week))
    
    nodes <- c(-sqrt(3), 0 , sqrt(3))
    weights <- c(1/6,2/3,1/6)
    sim_share <- rep(0, N)
    for (i in 1:length(weights)) {
      beta <- exp(mu + abs(sigma)*nodes[i])
      num <- exp(delta - beta*price)
      week_num <- cbind(week, num)
      colnames(week_num) <- c("week", "num")
      
      denom <- as.matrix(as_tibble(week_num) %>%
                           group_by(week) %>%
                           mutate(denom := sum(num)) %>%
                           ungroup %>%
                           select(denom))
      sim_share_node <- num/denom
      
      sim_share <- sim_share + sim_share_node*weights[i]
    }
    diff <- share - sim_share
    return(diff)
}