#### Preliminaries ####
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
rm(list = ls())
options(scipen=999)

unlink("../temp", recursive = TRUE)
unlink("../output", recursive = TRUE)
dir.create("../temp")
dir.create("../output")

#### Assignment 2 - analysis ####
source("preclean.R")
library(stats4)

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
model_b_alpha_DEL_MONTE <- round(model_b_fit@coef[1], digits = 4)
model_b_alpha_DEL_MONTE_se <- round(model_b_fit@vcov[1,1], digits = 4)

model_b_alpha_HEINZ <- round(model_b_fit@coef[2], digits = 4)
model_b_alpha_HEINZ_se <- round(model_b_fit@vcov[2,2], digits = 4)

model_b_alpha_HUNTS <- round(model_b_fit@coef[3], digits = 4)
model_b_alpha_HUNTS_se <- round(model_b_fit@vcov[3,3], digits = 4)

model_b_beta <- round(model_b_fit@coef[4], digits = 4)
model_b_beta_se <- round(model_b_fit@vcov[4,4], digits = 4)

# (c)

omega <- round(model_b_alpha_HEINZ/model_b_beta, digits = 4)

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

E_change_CS_per_occasion <- round(E_change_CS_per_occasion(model_b_alpha_DEL_MONTE, model_b_alpha_HEINZ,
                  model_b_alpha_HUNTS, model_b_beta), digits = 4)

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

average_etas <- average_etas(model_b_alpha_DEL_MONTE, model_b_alpha_HEINZ,
                    model_b_alpha_HUNTS, model_b_beta)
  
# (f)

# (g) Nothing to do

# (h) 
