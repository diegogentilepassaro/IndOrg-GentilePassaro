#### Assignment 1 - preclean ####
library(haven)
library(dplyr)

unzip("../raw/2012-0324_data.zip", exdir = "../temp")

gas_data <- read_dta("../temp/Web_materials/annual_regression_data.dta")
gas_data <- as_tibble(gas_data)
gas_data <- gas_data %>%
  filter(is.na(id) == FALSE)
gas_data <- mutate(gas_data,
                   observation_id         = row_number(),
                   real_gas_p             = ((taxin_gas_price)/cpi87),
                   real_gas_producer_p    = (taxin_gas_price - gas_tax_all)/cpi87,
                   ln_real_gas_producer_p = log(real_gas_producer_p),
                   ln_real_gas_p          = log(real_gas_p),
                   q_percapita            = (hug/pop_state_adj)*1000,
                   ln_q_percapita         = log(q_percapita),
                   real_gas_tax_all       = ((gas_tax_all)/cpi87),
                   ln_real_gas_tax_all    = log(real_gas_tax_all),                   
                   realinc_percapita      = (realincome_state*1000/pop_state_adj),
                   ln_realinc_percapita   = log(realinc_percapita),
                   ln_real_oilprice       = log(real_oilprice))

gas_data <- gas_data %>%
  arrange(state_num, year) %>%
  mutate(fd_ln_real_gas_producer_p = ln_real_gas_producer_p - lag(ln_real_gas_producer_p),
         fd_ln_real_gas_p = ln_real_gas_p - lag(ln_real_gas_p),
         fd_ln_q_percapita = ln_q_percapita - lag(ln_q_percapita),
         fd_ln_realinc_percapita = ln_realinc_percapita - lag(ln_realinc_percapita),
         fd_ln_real_gas_tax_all = ln_real_gas_tax_all - lag(ln_real_gas_tax_all),
         fd_urbanization = urbanization - lag(urbanization),
         fd_budget_surplus = budget_surplus - lag(budget_surplus),
         fd_real_oilprice = real_oilprice - lag(real_oilprice))

gas_data <- gas_data %>%
  select(observation_id, year, state_num, state, fd_ln_real_gas_p, fd_ln_real_gas_producer_p, fd_ln_q_percapita,
         fd_ln_real_gas_tax_all, fd_ln_realinc_percapita, fd_budget_surplus, fd_urbanization, fd_real_oilprice,
         unemployment, ln_real_gas_p, ln_real_gas_producer_p, ln_q_percapita)

# write_dta(gas_data, "../temp/gas_data.dta", version = 14)

gas_data_supply <- gas_data %>%
  select(fd_ln_q_percapita, fd_ln_real_gas_producer_p, fd_ln_real_gas_tax_all, fd_real_oilprice,
         fd_ln_realinc_percapita, fd_budget_surplus, fd_urbanization, year)
gas_data_supply <- gas_data_supply[complete.cases(gas_data_supply),]  
fd_ln_q_supply <- as.vector(gas_data_supply$fd_ln_q_percapita)
year_supply <- as.vector(gas_data_supply$year)
endo_supply <- as.vector(gas_data_supply$fd_ln_real_gas_producer_p)
controls_supply <- cbind(gas_data_supply$fd_ln_realinc_percapita, gas_data_supply$fd_urbanization)
colnames(controls_supply) <- c("fd_ln_realinc_percapita","fd_urbanization")
instruments_supply <- as.vector(gas_data_supply$fd_ln_real_gas_tax_all)

gas_data_demand <- gas_data %>%
  select(fd_ln_q_percapita, fd_ln_real_gas_p, fd_ln_real_gas_tax_all, fd_real_oilprice, 
         fd_ln_realinc_percapita, fd_budget_surplus, fd_urbanization, year)
gas_data_demand <- gas_data_demand[complete.cases(gas_data_demand),]
fd_ln_q_demand <- as.vector(gas_data_demand$fd_ln_q_percapita)
year_demand <- as.vector(gas_data_demand$year)
endo_demand <- as.vector(gas_data_demand$fd_ln_real_gas_p)
controls_demand <- cbind(gas_data_demand$fd_ln_realinc_percapita, gas_data_demand$fd_urbanization)
colnames(controls_demand) <- c("fd_ln_realinc_percapita","fd_urbanization")
instruments_demand <- as.vector(gas_data_demand$fd_ln_real_gas_tax_all)
