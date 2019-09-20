#### Preliminaries ####
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
rm(list = ls())

unlink("../temp", recursive = TRUE)
unlink("../output", recursive = TRUE)
dir.create("../temp")
dir.create("../output")

#### Assignment 1 ####
library(dplyr)
library(stringr)
library(ggplot2)
library(haven)
library(moderndive)
library(broom)
library(lfe)

source("preclean.R")
source("../../lib/R/ols_reg.R")
source("../../lib/R/iv_reg.R")

#### a) Estimate supply elasticity and b) demand elasticity

## Supply elasticity
naive_supply_model <- ols_reg(y = fd_ln_q_supply, X = cbind(endo_supply, controls_supply), 
                                    factor_var1 =  year_supply, factor_var2 = state_supply)

iv_estimate_supply_model <- iv_reg(y = fd_ln_q_supply, X = controls_supply, endo = endo_supply, 
                                   instr = instruments_supply, factor_var1 =  year_supply, factor_var2 = state_supply)

supply_model_data <- data.frame(var = row.names(iv_estimate_supply_model$summary), 
                                iv_estimate_supply_model$summary)
df_supply_model <- iv_estimate_supply_model$df 
supply_endo <- supply_model_data %>%
  filter(stringr::str_detect(var, "endo_supply")) %>%
  mutate(t_test_elastic = (beta - 1)/se_beta)
t_test_supply_elastic <- supply_endo$t_test_elastic
critical_value_supply <- qt(p = 0.05, df = df_supply_model)

if (abs(supply_endo$t_test_elastic) >= abs(critical_value_supply)) {
  print("Reject hypothesis that supply is elastic")
} else {
  print("Can't reject hypothesis that supply is elastic")
}

supply_elasticity <- as.numeric(supply_endo$beta)

## Demand elasticity
naive_demand_model <- ols_reg(y = fd_ln_q_demand, X = cbind(endo_demand, controls_demand), 
                              factor_var1 =  year_demand, factor_var2 = state_demand)

iv_estimate_demand_model <- iv_reg(y = fd_ln_q_demand, X = controls_demand, endo = endo_demand, 
                                   instr = instruments_demand, factor_var1 =  year_demand, factor_var2 = state_demand)

demand_model_data <- data.frame(var = row.names(iv_estimate_demand_model$summary), 
                                iv_estimate_demand_model$summary)
df_demand_model <- iv_estimate_supply_model$df 
demand_endo <- demand_model_data %>%
  filter(stringr::str_detect(var, "endo_demand")) %>%
  mutate(t_test_elastic = (beta - (-1))/se_beta)
t_test_demand_elastic <- demand_endo$t_test_elastic
critical_value_demand <- qt(p = 0.05, df = df_demand_model)

if (abs(demand_endo$t_test_elastic) >= abs(critical_value_demand)) {
  print("Reject hypothesis that supply is elastic")
} else {
  print("Can't reject hypothesis that supply is elastic")
}

demand_elasticity <- as.numeric(demand_endo$beta)

### c) and d)  Supply shocks
supply_shock_data <- data.frame(var = row.names(iv_estimate_supply_model$summary), 
                                iv_estimate_supply_model$summary)
supply_shock_data <- supply_shock_data %>%
  filter(stringr::str_detect(var, "year")) %>%
  mutate(year = substring(var, -4)) %>%
  select(year, beta)

ggplot(data = supply_shock_data, aes(y = beta, x = year)) + geom_line(group = 1) +
  xlab("Year") + ylab("Year FE") + theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))

# ### e) Demand shocks
demand_shock_data <- data.frame(var = row.names(iv_estimate_demand_model$summary), 
                                iv_estimate_demand_model$summary)
demand_shock_data <- demand_shock_data %>%
  filter(stringr::str_detect(var, "year")) %>%
  mutate(year = substring(var, -4)) %>%
  select(year, beta)

ggplot(data = demand_shock_data, aes(y = beta, x = year)) + geom_line(group = 1) +
  xlab("Year") + ylab("Year FE") + theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))

### f)
ri_2008_data <- gas_data %>%
  filter(state == "Rhode Island", year == 2008)

quantity <- as.numeric(ri_2008_data %>%
                         mutate(quantity = exp(ln_q_percapita)) %>%
                         select(quantity))
demand_constant <- as.numeric(ri_2008_data %>%
  mutate(demand_constant = (exp(ln_q_percapita))/(exp(ln_real_gas_p)^demand_elasticity)) %>%
  select(demand_constant))
supply_constant <- as.numeric(ri_2008_data %>%
                                mutate(supply_constant = (exp(ln_q_percapita))/(exp(ln_real_gas_producer_p)^supply_elasticity)) %>%
                                select(supply_constant))

price_producer <- as.numeric(ri_2008_data %>%
                                mutate(price_producer = exp(ln_real_gas_producer_p)) %>%
                                select(price_producer))
price_consumer <- as.numeric(ri_2008_data %>%
                               mutate(price_consumer = exp(ln_real_gas_p)) %>%
                               select(price_consumer))
total_tax <- price_consumer - price_producer
new_price <- (demand_constant/supply_constant)^(1/(supply_elasticity - demand_elasticity))
new_quantity <- demand_constant*new_price^demand_elasticity

if (round(new_quantity, digits = 3) == round(supply_constant*new_price^supply_elasticity,digits = 3)){
  print("Celebrate: Demand equals supply")
}else{
  print("Oops, demand does not equal supply")
}
  
### g)
demand <- function(x) {demand_constant*(x^(demand_elasticity))}
supply <- function(x) {supply_constant*(x^(supply_elasticity))}
change_cs <- integrate(demand, lower = new_price, upper = price_consumer)$value
change_ps <- integrate(supply, lower = price_producer, upper = new_price)$value
gov_loss <- quantity*(price_consumer- price_producer)
change_ts <- change_cs + change_ps - gov_loss

### i)
ri_unemp <- gas_data %>%
  filter(state == "Rhode Island") %>%
  select(year, state_num, unemployment)
ggplot(data = ri_unemp, aes(y = unemployment, x = year)) + geom_line()

ri_unemp_fix <- ri_unemp %>%
  mutate(unemployment = case_when(year>= 1985 & year <= 2000 ~ unemployment*100,
                           TRUE ~ unemployment))
ggplot(data = ri_unemp_fix, aes(y = unemployment, x = year)) + geom_line()

