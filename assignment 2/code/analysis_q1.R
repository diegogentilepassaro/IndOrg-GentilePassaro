#### Assignment 2 - Question 1 ####
library(stats4)
library(nleqslv)

# (a)

neg_LL_model_a <- function(beta) {
  table <- market1_data %>%
    mutate(A = -beta*price) %>%
    ungroup
  table <- table %>%
    group_by(id, occasion) %>%
    mutate(B = log(sum(exp(-beta*price)))) %>%
    ungroup
  table <- table %>%
    mutate(obs_LL = chosen_brand *(A-B))
  obs_LL <- as.vector(table$obs_LL)
  neg_LL <- -sum(obs_LL)

  return(neg_LL)
}

model_a_fit <- mle(neg_LL_model_a, start = list(beta = 1))
model_a_beta <- round(model_a_fit@coef, digits = 4)
model_a_beta_se <- round(as.numeric(model_a_fit@vcov), digits = 4)

# (b)

neg_LL_model_b <- function(alpha_DEL_MONTE, alpha_HEINZ, alpha_HUNTS, beta) {
  table <- market1_data %>%
    mutate(alpha = case_when(brand == 1 ~ alpha_DEL_MONTE,
                             brand == 2 ~ alpha_HEINZ,
                             brand == 3 ~ alpha_HUNTS,
                             brand == 4 ~ 0))
  table <- table %>%
    mutate(A = alpha-beta*price) %>%
    ungroup
  table <- table %>%
    group_by(id, occasion) %>%
    mutate(B = log(sum(exp(alpha-beta*price)))) %>%
    ungroup
  table <- table %>%
    mutate(obs_LL = chosen_brand *(A-B))
  obs_LL <- as.vector(table$obs_LL)
  neg_LL <- -sum(obs_LL)
  
  return(neg_LL)
}

model_b_fit <- mle(neg_LL_model_b, start = list(alpha_DEL_MONTE = 1, alpha_HEINZ = 1, 
                                                alpha_HUNTS = 1, beta = 1))
alpha_DEL_MONTE <- round(model_b_fit@coef[1], digits = 4)
alpha_DEL_MONTE_se <- round(model_b_fit@vcov[1,1], digits = 4)

alpha_HEINZ <- round(model_b_fit@coef[2], digits = 4)
alpha_HEINZ_se <- round(model_b_fit@vcov[2,2], digits = 4)

alpha_HUNTS <- round(model_b_fit@coef[3], digits = 4)
alpha_HUNTS_se <- round(model_b_fit@vcov[3,3], digits = 4)

beta <- round(model_b_fit@coef[4], digits = 4)
beta_se <- round(model_b_fit@vcov[4,4], digits = 4)

# (c)

omega <- round(alpha_HEINZ/beta, digits = 4)

# (d)

E_change_CS_per_occasion <- function(alpha_DEL_MONTE, alpha_HEINZ, alpha_HUNTS, beta) {
  table <- market1_data %>%
    mutate(alpha = case_when(brand == 1 ~ alpha_DEL_MONTE,
                             brand == 2 ~ alpha_HEINZ,
                             brand == 3 ~ alpha_HUNTS,
                             brand == 4 ~ 0))
  table <- table %>%
    mutate(Before = exp(alpha-beta*price),
           After = case_when(brand == 1 ~ 0,
                         TRUE ~ Before))
  
  table <- table %>%
    group_by(id, occasion) %>%
    mutate(E_change_CS_it = (1/beta)*(log(sum(After)) - log(sum(Before))))
  
  table <- table %>%
    distinct(id, occasion, E_change_CS_it)
  
 E_change_CS_per_occasion <- mean(table$E_change_CS_it)
  
  return(E_change_CS_per_occasion)
}

E_change_CS_per_occasion <- round(E_change_CS_per_occasion(alpha_DEL_MONTE, alpha_HEINZ,
                  alpha_HUNTS, beta), digits = 2)

# (e)

average_etas <- function(alpha_DEL_MONTE, alpha_HEINZ, alpha_HUNTS, beta) {
  table <- market1_data %>%
    mutate(alpha = case_when(brand == 1 ~ alpha_DEL_MONTE,
                             brand == 2 ~ alpha_HEINZ,
                             brand == 3 ~ alpha_HUNTS,
                             brand == 4 ~ 0))
  table <- table %>%
    mutate(pi_numerator = exp(alpha-beta*price))
  
  table <- table %>%
    group_by(id, occasion) %>%
    mutate(pi_denominator = sum(exp(alpha-beta*price))) %>%
    ungroup
  
  table <- table %>%
    mutate(pi = pi_numerator/pi_denominator,
           eta = -beta*(price*(1-pi)))
  
  eta_DEL_MONTE_t <- table %>%
    filter(brand == 1) %>%
    select(eta)
  eta_DEL_MONTE <- mean(eta_DEL_MONTE_t$eta)
  
  eta_HEINZ_t <- table %>%
    filter(brand == 2) %>%
    select(eta)
  eta_HEINZ <- mean(eta_HEINZ_t$eta)
  
  eta_HUNTS_t <- table %>%
    filter(brand == 3) %>%
    select(eta)
  eta_HUNTS <- mean(eta_HUNTS_t$eta)
  
  eta_STORE_t <- table %>%
    filter(brand == 4) %>%
    select(eta)
  eta_STORE <- mean(eta_STORE_t$eta)
  
  return_list <- list("eta_DEL_MONTE" = eta_DEL_MONTE, "eta_HEINZ" = eta_HEINZ,
                      "eta_HUNTS" = eta_HUNTS, "eta_STORE" = eta_STORE)
  
  return(return_list)
}

average_etas <- average_etas(alpha_DEL_MONTE, alpha_HEINZ,
                    alpha_HUNTS, beta)
eta_DEL_MONTE <- average_etas$eta_DEL_MONTE
eta_HEINZ <- average_etas$eta_HEINZ
eta_HUNTS <- average_etas$eta_HUNTS
eta_STORE <- average_etas$eta_STORE

# (f)

average_price_per_brand <- function() {
  table <- market1_data %>%
    group_by(brand) %>%
    mutate(average_price = mean(price)) %>%
    distinct(brand, average_price) %>%
    ungroup
  
  P_DEL_MONTE <- as.numeric(table %>%
    filter(brand == 1) %>%
    select(average_price))

  P_HEINZ <- as.numeric(table %>%
    filter(brand == 2) %>%
    select(average_price))

  P_HUNTS <- as.numeric(table %>%
    filter(brand == 3) %>%
    select(average_price))
  
  P_STORE <- as.numeric(table %>%
    filter(brand == 4) %>%
    select(average_price))
  
  return_list <- list("P_DEL_MONTE" = P_DEL_MONTE, "P_HEINZ" = P_HEINZ,
                      "P_HUNTS" = P_HUNTS, "P_STORE" = P_STORE)
  return(return_list)
}

average_price_per_brand <- average_price_per_brand()
avg_P_DEL_MONTE <- average_price_per_brand$P_DEL_MONTE
avg_P_HEINZ <- average_price_per_brand$P_HEINZ
avg_P_HUNTS <- average_price_per_brand$P_HUNTS
avg_P_STORE <- average_price_per_brand$P_STORE

c_DEL_MONTE <- avg_P_DEL_MONTE*(1 + (1/eta_DEL_MONTE))
c_HEINZ <- avg_P_HEINZ*(1 + (1/eta_HEINZ))
c_HUNTS <- avg_P_HUNTS*(1 + (1/eta_HUNTS))
c_STORE <- avg_P_STORE*(1 + (1/eta_STORE))

# (g) Nothing to do

# (h) 

FOC_system <- function(P) {
  P_HEINZ <- P[1]
  P_HUNTS <- P[2]
  P_STORE <- P[3]
  
  pi_tilde_HEINZ = exp(alpha_HEINZ - beta*P_HEINZ)/
    (exp(alpha_HEINZ - beta*P_HEINZ) + exp(alpha_HUNTS - beta*P_HUNTS) + exp(- beta*P_STORE))
  pi_tilde_HUNTS = exp(alpha_HUNTS - beta*P_HUNTS)/
    (exp(alpha_HEINZ - beta*P_HEINZ) + exp(alpha_HUNTS - beta*P_HUNTS) + exp(- beta*P_STORE))
  pi_tilde_STORE = exp(- beta*P_STORE)/
    (exp(alpha_HEINZ - beta*P_HEINZ) + exp(alpha_HUNTS - beta*P_HUNTS) + exp(- beta*P_STORE))
  
  FOC_HEINZ <- (1/(beta*(1-pi_tilde_HEINZ))) + c_HEINZ - P_HEINZ
  FOC_HUNTS <- (1/(beta*(1-pi_tilde_HUNTS))) + c_HUNTS - P_HUNTS
  FOC_STORE <- (1/(beta*(1-pi_tilde_STORE))) + c_STORE - P_STORE
  
  return(c(FOC_HEINZ, FOC_HUNTS, FOC_STORE))
}

nash_prices <- nleqslv(c(1,1,1), FOC_system)

nash_P_HEINZ <- nash_prices$x[1]
nash_P_HUNTS <- nash_prices$x[2]
nash_P_STORE <- nash_prices$x[3]

E_CS_avg_price <- round((1/beta)*(log(exp(alpha_HEINZ - beta*avg_P_HEINZ) +
                                      exp(alpha_HUNTS - beta*avg_P_HUNTS) +
                                      exp(-beta*avg_P_STORE)) - 
                                  log(exp(alpha_DEL_MONTE - beta*avg_P_DEL_MONTE) + 
                                      exp(alpha_HEINZ - beta*avg_P_HEINZ) +
                                      exp(alpha_HUNTS - beta*avg_P_HUNTS) +
                                      exp(-beta*avg_P_STORE))), digits = 2)

E_CS_nash <- round((1/beta)*(log(exp(alpha_HEINZ - beta*nash_P_HEINZ) +
                                 exp(alpha_HUNTS - beta*nash_P_HUNTS) +
                                 exp(-beta*nash_P_STORE)) - 
                             log(exp(alpha_DEL_MONTE - beta*avg_P_DEL_MONTE) + 
                                 exp(alpha_HEINZ - beta*avg_P_HEINZ) +
                                 exp(alpha_HUNTS - beta*avg_P_HUNTS) +
                                 exp(-beta*avg_P_STORE))), digits = 2)