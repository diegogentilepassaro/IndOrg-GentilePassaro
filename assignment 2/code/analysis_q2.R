#### Assignment 2 - Question 2 ####
library(dplyr)
library(bbmle)

# (a)

households_identified <- as.numeric(market1_data %>%
  filter(chosen_brand == 1) %>%
  distinct(id, brand) %>%
  group_by(id) %>%
  tally() %>%
  filter(n == 4) %>%
  tally())

households_total <- as.numeric(market1_data %>%
  filter(chosen_brand == 1) %>%
  distinct(id) %>%
  tally())

# (b)

mv_std_normal_gamma_draws <- function(nbr_draws) {
  mu = c(0,0,0) 
  Sigma = matrix(c(1,0,0,
                   0,1,0,
                   0,0,1), nrow = 3, ncol = 3)
  random_gammas <-  MASS::mvrnorm(n = nbr_draws, mu = mu, Sigma = Sigma , tol = 1e-3)  
  random_gammas <- t(random_gammas)
  random_gammas <- rbind(random_gammas, rep(0, nbr_draws))
  brand <- c(1,2,3,4)
  random_gammas <- cbind(brand, random_gammas)
  for (draw in 1:nbr_draws) {
    colnames(random_gammas)[draw+1] <- paste0("gamma_draw_",draw)
  }
  return(random_gammas)
}

simulate_gammas <- function(nbr_draws) {
  ids <- market1_data_sample %>%
    distinct(id)
  nbr_brands <- as.numeric(dim(market1_data_sample %>%
                                 distinct(brand))[1])
 simulated_gammas <- matrix(, nrow = 0, ncol = nbr_draws+2)
  for(i in 1:nrow(ids)) {
    i <- as.numeric(ids[i,1])
    gamma_draws <- mv_std_normal_gamma_draws(nbr_draws)
    id <- rep(i, nbr_brands)
    gamma_draws <- cbind(id, gamma_draws)
    simulated_gammas <- rbind(simulated_gammas, gamma_draws)
  }
  simulated_gammas <- as_tibble(simulated_gammas)
  return(simulated_gammas)
}

simulated_neg_LL <- function(mu_DEL_MONTE, mu_HEINZ, mu_HUNTS, 
                             sigma_DEL_MONTE, sigma_HEINZ, sigma_HUNTS,
                             beta, nbr_draws) {
  simulated_gammas <- simulate_gammas(nbr_draws)
  table <- left_join(market1_data_sample, simulated_gammas, by = c("id", "brand"))
  for (draw in 1:nbr_draws) {
    table <- table %>%
      mutate(!!paste0("alpha_draw_", draw) := case_when(brand == 1 ~ mu_DEL_MONTE + sigma_DEL_MONTE*!!as.name(paste0("gamma_draw_", draw)),
                                                       brand == 2 ~ mu_HEINZ + sigma_HEINZ*!!as.name(paste0("gamma_draw_", draw)),
                                                       brand == 3 ~ mu_HUNTS + sigma_HUNTS*!!as.name(paste0("gamma_draw_", draw)),
                                                       brand == 4 ~ 0))
      
    table <- table %>%
      mutate(!!paste0("cond_pi_numerator_draw_", draw) := exp(!!as.name(paste0("alpha_draw_", draw)) - beta*price))
    table <-table %>%
      group_by(id, occasion) %>%
      mutate(!!paste0("cond_pi_denominator_draw_", draw) := sum(exp(!!as.name(paste0("alpha_draw_", draw)) - beta*price))) %>%
      ungroup
    table <- table %>%
      mutate(!!paste0("cond_pi_draw_", draw) := !!as.name(paste0("cond_pi_numerator_draw_", draw))/
               !!as.name(paste0("cond_pi_denominator_draw_", draw)))
    table <- table %>%
      group_by(id) %>%
      mutate(!!paste0("obs_L_draw_", draw) := prod((!!as.name(paste0("cond_pi_draw_", draw)))^chosen_brand))
  }
  
  table <- table %>%
    select(id, starts_with("obs_L_draw_")) %>%
    distinct() %>%
    ungroup
  table <- table %>%
    mutate(simul_obs_L = rowMeans(select(table, starts_with("obs_L_draw_"))))
  
  simul_obs_L <- as.vector(table$simul_obs_L)
  neg_SLL <- -sum(log(simul_obs_L))
  
  return(neg_SLL)
}

market1_data_sample <- market1_data %>%
  group_by(id) %>%
  mutate(random_val = runif(n = 1)) %>%
  ungroup %>%
  filter(random_val <= 0.05) %>%
  select(occasion, id, date, sid, price, brand,
         chosen_brand)

set.seed(8)
nbr_draws <- 50
system.time(model_het_fit <- mle2(simulated_neg_LL,
                start = list(mu_DEL_MONTE = alpha_DEL_MONTE, mu_HEINZ = alpha_HEINZ, mu_HUNTS = alpha_HUNTS,
                             sigma_DEL_MONTE = 1, sigma_HEINZ = 1, sigma_HUNTS = 1,
                             beta = beta),
                fixed = list(nbr_draws = nbr_draws), skip.hessian = TRUE,
                method = "BFGS"))

mu_DEL_MONTE <- round(model_het_fit@coef[1], digits = 4)
mu_HEINZ <- round(model_het_fit@coef[2], digits = 4)
mu_HUNTS <- round(model_het_fit@coef[3], digits = 4)
sigma_DEL_MONTE <- round(model_het_fit@coef[4], digits = 4)
sigma_HEINZ <- round(model_het_fit@coef[5], digits = 4)
sigma_HUNTS <- round(model_het_fit@coef[6], digits = 4)
beta_het <- round(model_het_fit@coef[7], digits = 4)

# (c)

E_omega_het <- mu_HEINZ/beta_het
V_omega_het <- sigma_HEINZ/beta_het

# (d)

data_for_elasticity <- as_tibble(mv_std_normal_gamma_draws(nbr_draws)) %>%
  mutate(avg_price = case_when(brand == 1 ~ avg_P_DEL_MONTE,
                               brand == 2 ~ avg_P_HEINZ,
                               brand == 3 ~ avg_P_HUNTS,
                               brand == 4 ~ avg_P_STORE))
for (draw in 1:nbr_draws) {
  data_for_elasticity <- data_for_elasticity %>%
    mutate(!!paste0("alpha_draw_", draw) := case_when(brand == 1 ~ mu_DEL_MONTE + sigma_DEL_MONTE*!!as.name(paste0("gamma_draw_", draw)),
                                                      brand == 2 ~ mu_HEINZ + sigma_HEINZ*!!as.name(paste0("gamma_draw_", draw)),
                                                      brand == 3 ~ mu_HUNTS + sigma_HUNTS*!!as.name(paste0("gamma_draw_", draw)),
                                                      brand == 4 ~ 0))
  
  data_for_elasticity <- data_for_elasticity %>%
    mutate(!!paste0("cond_pi_numerator_draw_", draw) := exp(!!as.name(paste0("alpha_draw_", draw)) - beta_het*avg_price))
  data_for_elasticity <-data_for_elasticity %>%
    mutate(!!paste0("cond_pi_denominator_draw_", draw) := sum(exp(!!as.name(paste0("alpha_draw_", draw)) - beta_het*avg_price))) %>%
    ungroup
  data_for_elasticity <- data_for_elasticity %>%
    mutate(!!paste0("cond_pi_draw_", draw) := !!as.name(paste0("cond_pi_numerator_draw_", draw))/
             !!as.name(paste0("cond_pi_denominator_draw_", draw)))
  data_for_elasticity <- data_for_elasticity %>%
    mutate(!!paste0("one_minus_cond_pi_draw_", draw) := 1 - !!as.name(paste0("cond_pi_draw_", draw))) 
}
data_for_elasticity <- data_for_elasticity %>%
  mutate(simul_one_minus_pi = rowMeans(select(data_for_elasticity, starts_with("one_minus_cond_pi_draw_")))) %>%
  select(brand, avg_price, simul_one_minus_pi) %>%
  mutate(elasticity = -beta_het*avg_price*simul_one_minus_pi)

het_eta_DEL_MONTE <- as.numeric(data_for_elasticity %>%
                                  filter(brand == 1) %>%
                                  select(elasticity))
het_eta_HEINZ <- as.numeric(data_for_elasticity %>%
                                  filter(brand == 2) %>%
                                  select(elasticity))
het_eta_HUNTS <- as.numeric(data_for_elasticity %>%
                                  filter(brand == 3) %>%
                                  select(elasticity))
het_eta_STORE <- as.numeric(data_for_elasticity %>%
                                  filter(brand == 4) %>%
                                  select(elasticity))

