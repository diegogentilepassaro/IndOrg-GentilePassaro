source("functions.R")

# 1. Ketchup

# Use the store brand as the normalized option
store_brand_data <- ketchup_data %>%
  filter(brand_ST == 1)
table <- left_join(ketchup_data, store_brand_data, suffix = c("", "_norm"), by = "week")

table <- table %>%
  mutate(log_share = log(share),
         log_share_norm = log(share_norm),
         diff_log_shares = log_share - log_share_norm,
         price_var = price - price_norm,
         othmkt_price_var = price_othmkt - price_othmkt_norm)

# Model 1 - Just a fancy OLS

Z <- as.matrix(table %>%
                 select(price_var, brand_DM, brand_HE, brand_HU))
W <- as.matrix(solve(t(Z)%*%Z))

params_model_1 <- optim(c(alpha_DM = 1, alpha_HE = 1, alpha_HU = 1, beta = 1), 
                        function_to_minimize_m1_and_m2, table = table, Z = Z, W = W)

alpha_DM_m1 <- params_model_1$par["alpha_DM"]
alpha_HE_m1 <- params_model_1$par["alpha_HE"]
alpha_HU_m1 <- params_model_1$par["alpha_HU"]
beta_m1 <- params_model_1$par["beta"]

# Note that we get the same as if we compute the following OLS model where the 
# coefficient for price_var has has the opposite sign of result that the command below displays
lm(diff_log_shares ~ brand_DM + brand_HE + 
     brand_HU + price_var - 1, data = table)

# Model 2 - Just a fancy 2SLS

Z <- as.matrix(table %>%
                 select(othmkt_price_var, brand_DM, brand_HE, brand_HU))
W <- as.matrix(solve(t(Z)%*%Z))

params_model_2 <- optim(c(alpha_DM = 1, alpha_HE = 1, alpha_HU = 1, beta = 1), 
                        function_to_minimize_m1_and_m2, table = table, Z = Z, W = W)

alpha_DM_m2 <- params_model_2$par["alpha_DM"]
alpha_HE_m2 <- params_model_2$par["alpha_HE"]
alpha_HU_m2 <- params_model_2$par["alpha_HU"]
beta_m2 <- params_model_2$par["beta"]

# Note that we get the same as if we compute the following 2SLS model
felm(diff_log_shares ~ brand_DM + brand_HE + 
     brand_HU - 1 | 0 | (price_var ~ othmkt_price_var), data = table)

# Model 3
tol <- 0.00001
tol_assertions <- 0.01

table_m3 <- ketchup_data

# Builds prices for each brand as a column for using all prices moments
distinct_brands <- as.matrix(table_m3 %>%
  distinct(brand))
for (b in distinct_brands) {
  brand_table <- table_m3 %>%
    filter(brand == b) %>%
    mutate(!!paste0("price_", b) := price) %>%
    select(week, !!paste0("price_", b))
    table_m3 <- left_join(table_m3, brand_table, by = c("week"))
}

Z <- as.matrix(table_m3 %>%
                 select(price_DM, price_HE, price_HU, price_ST, brand_DM, brand_HE, brand_HU))
W <- as.matrix(solve(t(Z)%*%Z))

## Sanity check 1
# Run BLP with sigma 0
BLP_sigma0 <- BLP_sigma0(initial_mu = 1, table = table_m3, 
                         Z = Z, W = W, initial_delta = 1, tol = tol, normalized_brand = "ST")
alpha_DM_sigma0 <- BLP_sigma0$alpha_DM_sigma0
alpha_HE_sigma0 <- BLP_sigma0$alpha_HE_sigma0
alpha_HU_sigma0 <- BLP_sigma0$alpha_HU_sigma0
mu_sigma0 <- BLP_sigma0$mu_sigma0
beta_sigma0 <- exp(mu_sigma0)

# compare with GMM similar to OLS but with the moments of model 3
OLS_with_moments_m3 <- optim(c(alpha_DM = 1, alpha_HE = 1, alpha_HU = 1, beta = 1), 
                        function_to_minimize_m1_and_m2, table = table, Z = Z, W = W)

assert(abs(alpha_DM_sigma0 - OLS_with_moments_m3$par[1]) <= tol_assertions)
assert(abs(alpha_HE_sigma0 - OLS_with_moments_m3$par[2]) <= tol_assertions)
assert(abs(alpha_HU_sigma0 - OLS_with_moments_m3$par[3]) <= tol_assertions)
assert(abs(beta_sigma0 - OLS_with_moments_m3$par[4]) <= tol_assertions)
# They match!

# Now the BLP
BLP_model <- BLP(initial_mu = 1, initial_sigma = 1, table = table_m3, 
                         Z = Z, W = W, initial_delta = 1, tol = tol, normalized_brand = "ST")
alpha_DM_m3 <- BLP_model$alpha_DM
alpha_HE_m3 <- BLP_model$alpha_HE
alpha_HU_m3 <- BLP_model$alpha_HU
mu <- BLP_model$mu
sigma <- BLP_model$sigma
deltas <-BLP_model$deltas

# Sanity check 2
diff_between_share_and_sim_share <- constraint_MPEC(table_m3, deltas, mu, sigma)
assert(max(abs(diff_between_share_and_sim_share)) <= tol_assertions)

# Sanity check 3
BLP_model2 <- BLP(initial_mu = log(beta_m2), initial_sigma = 0.1, table = table_m3, 
            Z = Z, W = W, initial_delta = 1, tol = tol, normalized_brand = "ST")

alpha_DM_m3_2 <- BLP_model2$alpha_DM
alpha_HE_m3_2 <- BLP_model2$alpha_HE
alpha_HU_m3_2 <- BLP_model2$alpha_HU
mu_2 <- BLP_model2$mu
sigma_2 <- BLP_model2$sigma

assert(abs(alpha_DM_m3_2 - alpha_DM_m3) <= tol_assertions)
assert(abs(alpha_HE_m3_2 - alpha_HE_m3) <= tol_assertions)
assert(abs(alpha_HU_m3_2 - alpha_HU_m3) <= tol_assertions)
assert(abs(mu_2 - mu) <= tol_assertions)
assert(abs(sigma_2 - sigma) <= tol_assertions)

# Elasticity for model 1 and 2

brand_share_avg_price <- table %>%
  group_by(brand) %>%
  mutate(avg_price = mean(price)) %>%
  distinct(brand, avg_price) %>%
  ungroup %>%
  mutate(alpha_m1 = case_when(brand == "DM" ~ alpha_DM_m1,
                           brand == "HE" ~ alpha_HE_m1,
                           brand == "HU" ~ alpha_HU_m1,
                           TRUE ~ 0),
         num_share_m1 = exp(alpha_m1 - beta_m1*avg_price),
         share_m1 = num_share_m1 / sum(num_share_m1),
         alpha_m2 = case_when(brand == "DM" ~ alpha_DM_m2,
                              brand == "HE" ~ alpha_HE_m2,
                              brand == "HU" ~ alpha_HU_m2,
                              TRUE ~ 0),
         num_share_m2 = exp(alpha_m2 - beta_m2*avg_price),
         share_m2 = num_share_m2 / sum(num_share_m2))

brand_share_avg_price <- brand_share_avg_price %>%
  mutate(brand2 = brand) %>%
  select(brand, avg_price, share_m1, share_m2)

brand_combinations <- brand_share_avg_price %>%
  mutate(brand2 = brand) %>%
  select(brand, brand2)
brand_combinations <- tidyr::expand(brand_combinations, brand, brand2)

elasticities <- left_join(brand_combinations, brand_share_avg_price, by = "brand")
elasticities <- left_join(elasticities, brand_share_avg_price, by = c("brand2" = "brand"))

elasticities <- elasticities %>%
  mutate(elasticity_m1 = round(case_when(brand == brand2 ~ -beta_m1*(1-share_m1.x)*avg_price.x,
                                TRUE ~ beta_m1*share_m1.y*avg_price.y), 2),
         elasticity_m2 = round(case_when(brand == brand2 ~ -beta_m2*(1-share_m2.x)*avg_price.x,
                                   TRUE ~ beta_m2*share_m2.y*avg_price.y), 2))

# Elasticity for model 3 (by simulation instead of quadrature because it is quite fast anyways)
nbr_draws <- 200

normal_draws <- ketchup_data %>%
  distinct(brand)
for (draw in 1:nbr_draws) {
  normal_draws <- normal_draws %>%
    mutate(!!paste0("normal_draw_", draw) := rnorm(1))     
}

inputs_elasticities_m3 <- brand_share_avg_price %>%
  select(-share_m1, -share_m2)
inputs_elasticities_m3  <- left_join(inputs_elasticities_m3, normal_draws, by = c("brand")) %>%
  mutate(alpha_m3 = case_when(brand == "DM" ~ alpha_DM_m3,
                              brand == "HE" ~ alpha_HE_m3,
                              brand == "HU" ~ alpha_HU_m3,
                              TRUE ~ 0))

for (draw in 1:nbr_draws) {
  inputs_elasticities_m3 <- inputs_elasticities_m3 %>%
    mutate(!!paste0("beta_draw_", draw) := exp(mu + sigma*!!as.name(paste0("normal_draw_", draw))),
           !!paste0("numerator_draw_", draw) := exp(alpha_m3 - !!as.name(paste0("beta_draw_", draw))*avg_price))
  inputs_elasticities_m3 <- inputs_elasticities_m3 %>%
    mutate(!!paste0("denominator_draw_", draw) := sum(exp(alpha_m3 - !!as.name(paste0("beta_draw_", draw))*avg_price)))
  inputs_elasticities_m3 <- inputs_elasticities_m3 %>%
    mutate(!!paste0("sim_share_draw_", draw) := !!as.name(paste0("numerator_draw_", draw))/
             !!as.name(paste0("denominator_draw_", draw)))
}

inputs_elasticities_m3 <- inputs_elasticities_m3 %>%
  select(brand, contains("beta_draw_"), contains("sim_share_draw_"), contains("avg_price"))
elasticities_m3 <- left_join(brand_combinations, inputs_elasticities_m3, by = "brand") %>%
  select(-contains("beta_draw_"))
elasticities_m3 <- left_join(elasticities_m3, inputs_elasticities_m3, by = c("brand2" = "brand"))

for (draw in 1:nbr_draws) {
  elasticities_m3 <- elasticities_m3 %>%
    mutate(!!paste0("sim_derivative_draw_", draw) := case_when(brand == brand2 ~ !!as.name(paste0("beta_draw_", draw))*
                                                                                 !!as.name(paste0("sim_share_draw_", draw, ".x"))*
                                                                                 (1-!!as.name(paste0("sim_share_draw_", draw, ".x"))),
                                                               TRUE ~ !!as.name(paste0("beta_draw_", draw))*
                                                                      !!as.name(paste0("sim_share_draw_", draw, ".x"))*
                                                                      !!as.name(paste0("sim_share_draw_", draw, ".y"))))
}

elasticities_m3 <- elasticities_m3 %>%
  mutate(sim_share_m3.x = rowMeans(select(select(elasticities_m3, ends_with(".x")), starts_with("sim_share_draw_"))),
         sim_share_m3.y = rowMeans(select(select(elasticities_m3, ends_with(".y")), starts_with("sim_share_draw_"))),
         sim_derivative_m3 = rowMeans(select(elasticities_m3, starts_with("sim_derivative_draw_"))))

elasticities_m3 <- elasticities_m3 %>%
  mutate(elasticity_m3 = round(case_when(brand == brand2 ~ -(avg_price.x/sim_share_m3.x)*sim_derivative_m3,
                                TRUE ~ (avg_price.y/sim_share_m3.x)*sim_derivative_m3), 2)) %>%
  select(brand, brand2, elasticity_m3)
