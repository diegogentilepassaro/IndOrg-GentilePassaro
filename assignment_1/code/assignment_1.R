#### Preliminaries ####
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
rm(list = ls())

unlink("../temp", recursive = TRUE)
unlink("../output", recursive = TRUE)
dir.create("../temp")
dir.create("../output")

#### Assignment 1 ####
library(tibble)
library(ggplot2)
library(dplyr)
library(haven)
library(moderndive)
library(broom)
library(lfe)

source("../../lib/R/ols_reg.R")
source("../../lib/R/iv_reg.R")

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
                   real_gas_tax_state     = ((sgastax)/cpi87),
                   ln_real_gas_tax_state  = log(real_gas_tax_state),
                   realinc_percapita      = (realincome_state*1000/pop_state_adj),
                   ln_realinc_percapita   = log(realinc_percapita),
                   ln_real_oilprice       = log(real_oilprice))
real_state_tax_66 <- gas_data %>%
  filter(year == 1966) %>%
  select(state_num, real_gas_tax_state_66 = real_gas_tax_state)

gas_data <- left_join(gas_data, real_state_tax_66, "state_num") %>%
  mutate(instrument_demand_price = real_gas_tax_state_66 * real_oilprice)
gas_data <- gas_data %>%
  select(observation_id, year, state_num, state, ln_real_gas_p, ln_real_gas_producer_p, ln_q_percapita,
         real_gas_tax_state, ln_real_gas_tax_state, real_gas_tax_state_66, instrument_demand_price, 
         ln_real_oilprice, real_oilprice, realinc_percapita, ln_realinc_percapita, pop_state_adj, urbanization, 
         mining_gsp, road_mileage, railpop, unemployment) %>%
  arrange(state_num, year)

ln_q_percapita <- as.vector(gas_data$ln_q_percapita)
endogenous <- as.vector(gas_data$ln_real_gas_producer_p)
supply_regressors <- cbind(gas_data$ln_realinc_percapita, gas_data$real_gas_tax_state)
colnames(supply_regressors) <- c("ln_realinc_percapita", "real_gas_tax_state")
instruments_supply <- cbind(gas_data$urbanization, gas_data$railpop, gas_data$road_mileage)
colnames(instruments_supply) <- c("urbanization", "railpop", "road_mileage")
year <- as.vector(gas_data$year)
state <- as.vector(gas_data$state_num)

# write_dta(gas_data, "../temp/gas_data.dta", version = 14)

# # avg_gas_price_ts <- gas_data %>%
# #   group_by(year) %>%
# #   summarise(avg_gas_price = mean(taxin_real_gas_price_dollars, na.rm = TRUE))
# # ggplot() + geom_line(data = avg_gas_price_ts, mapping = aes(x = year, y = avg_gas_price))

#### a) Estimate supply elasticity and b) demand elasticity

## Supply elasticity
naive_supply_model <- ols_reg(ln_q_percapita, cbind(supply_regressors, endogenous), 
                                    factor_var1 =  year, factor_var2 = state)

iv_estimate_supply_model <- iv_reg(ln_q_percapita, supply_regressors, endogenous, instruments_supply, 
                                         factor_var1 =  year, factor_var2 = state)

naive_supply_model$summary
iv_estimate_supply_model$summary

## Demand elasticity
naive_demand_model <- felm(ln_q_percapita ~ 1 + realinc_percapita + urbanization + real_gas_tax_state
                           + ln_real_gas_p
                           |year + state_num, data = gas_data)

iv_estimate_demand_model <- felm(ln_q_percapita ~ 1 + realinc_percapita + urbanization + real_gas_tax_state
                                 |year + state_num|
                                 (ln_real_gas_p ~ instrument_demand_price),
                                 data = gas_data)

tidy(naive_demand_model)
tidy(iv_estimate_demand_model)

### c) and d)  Supply shocks

iv_estimate_supply_model <- felm(ln_q_percapita ~ 1 + ln_realinc_percapita + real_gas_tax_state
                                 | year + state_num | (ln_real_gas_producer_p ~ urbanization + road_mileage + railpop),
                                 data = gas_data)
year_fes <- getfe(iv_estimate_supply_model) %>%
  filter(fe == "year") %>%
  select(year = idx, effect)
ggplot(data = year_fes, aes(y = effect, x = year)) + geom_line(group = 1) + 
  xlab("Year") + ylab("Year FE") + theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))

### e) Demand shocks
iv_estimate_demand_model <- felm(ln_q_percapita ~ 1 + realinc_percapita + urbanization + real_gas_tax_state
                                |year + state_num|
                                (ln_real_gas_p ~ instrument_demand_price),
                                data = gas_data)
year_fes <- getfe(iv_estimate_demand_model) %>%
  filter(fe == "year") %>%
  select(year = idx, effect)
ggplot(data = year_fes, aes(y = effect, x = year)) + geom_line(group = 1) + 
  xlab("Year") + ylab("Year FE") + theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))

### i)
ri_unemp <- gas_data %>%
  filter(state == "Rhode Island") %>%
  select(year, state_num, unemployment)
ggplot(data = ri_unemp, aes(y = unemployment, x = year)) + geom_line()

ri_unemp_fix <- ri_unemp %>%
  mutate(unemployment = case_when(year>= 1985 & year <= 2000 ~ unemployment*100,
                           TRUE ~ unemployment))
ggplot(data = ri_unemp_fix, aes(y = unemployment, x = year)) + geom_line()

