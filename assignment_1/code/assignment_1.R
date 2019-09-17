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
library(dplyr)
library(haven)
library(moderndive)
library(broom)
library(lfe)

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

write_dta(gas_data, "../temp/gas_data.dta", version = 14)

# # avg_gas_price_ts <- gas_data %>%
# #   group_by(year) %>%
# #   summarise(avg_gas_price = mean(taxin_real_gas_price_dollars, na.rm = TRUE))
# # ggplot() + geom_line(data = avg_gas_price_ts, mapping = aes(x = year, y = avg_gas_price))

#### a) Estimate supply elasticity and b) demand elasticity

## Supply elasticity
naive_supply_model <- felm(ln_q_percapita ~ 1 + ln_realinc_percapita + real_gas_tax_state +
                                            ln_real_gas_producer_p| year + state_num, data = gas_data)

iv_estimate_supply_model <- felm(ln_q_percapita ~ 1 + ln_realinc_percapita + real_gas_tax_state
                                 | year + state_num | (ln_real_gas_producer_p ~ urbanization + road_mileage + railpop),
                                 data = gas_data)

tidy(naive_supply_model)
tidy(iv_estimate_supply_model)

## Demand elasticity
naive_demand_model <- felm(ln_q_percapita ~ 1 + realinc_percapita + 
                           ln_real_gas_p
                           |year + state_num, data = gas_data)

iv_estimate_demand_model <- felm(ln_q_percapita ~ 1 + realinc_percapita 
                                 |year + state_num|
                                 (ln_real_gas_p ~ instrument_demand_price),
                                 data = gas_data)

tidy(naive_demand_model)
tidy(iv_estimate_demand_model)

# ### c) and d)  Supply shocks
# data_frame_resid_supply <- augment(iv_estimate_supply_model)
# supply_resid_yearly <- data_frame_resid_supply %>%
#   group_by(year) %>%
#   summarize(mean_supply_resid = weighted.mean(.resid)) %>%
#   select(year, mean_supply_resid)
# ggplot(data = supply_resid_yearly, aes(y = mean_supply_resid, x = year)) + geom_line()
# 
# ### e) Demand shocks
# demand_shock_data <- gas_data %>%
#   group_by(year) %>%
#   summarise(pop_weighted_ln_real_inc_percapita = weighted.mean(ln_real_inc_percapita, pop_state_adj)) %>%
#   mutate(fd_pop_weighted_ln_real_inc_percapita   = pop_weighted_ln_real_inc_percapita - lag(pop_weighted_ln_real_inc_percapita)) %>%
#   select(year, fd_pop_weighted_ln_real_inc_percapita)
# 
# ggplot(data = demand_shock_data, aes(y = fd_pop_weighted_ln_real_inc_percapita, x = year)) + geom_line()
# 
# # data_frame_resid_demand <- augment(iv_estimate_demand_model)
# # demand_resid_yearly <- data_frame_resid_demand %>%
# #   group_by(year) %>%
# #   summarize(mean_demand_resid = weighted.mean(.resid)) %>%
# #   select(year, mean_demand_resid)
# # ggplot(data = demand_resid_yearly, aes(y = mean_demand_resid, x = year)) + geom_line()
# 
# ### i) 
# ri_unemp <- gas_data %>%
#   filter(state == "Rhode Island") %>%
#   select(year, state_num, unemployment)
# ggplot(data = ri_unemp, aes(y = unemployment, x = year)) + geom_line()
# 
# ri_unemp_fix <- ri_unemp %>%
#   mutate(unemployment = case_when(year>= 1985 & year <= 2000 ~ unemployment*100,
#                            TRUE ~ unemployment))
# ggplot(data = ri_unemp_fix, aes(y = unemployment, x = year)) + geom_line()
# 
