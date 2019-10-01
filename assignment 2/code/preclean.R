#### Assignment 2 - preclean ####
library(haven)
library(dplyr)
library(tidyr)
library(stringr)

main <- function() {
  upc_data <- clean_upcs()
  pf1_data <- clean_price_data()
  brand_price_data_upc <- store_date_brand_price(pf1_data, upc_data)
  brand_price_data <- brand_price_data_upc  %>%
    distinct(date, sid, brand, .keep_all = TRUE) %>%
    select(date, sid, brand, pre_coupon_price)
  
  hp1_data <- clean_hp_data()
  purchases_data <- left_join(hp1_data, brand_price_data_upc, by = c("date", "sid", "upc")) %>%
    filter(is.na(brand) == FALSE) %>%
    mutate(price = pre_coupon_price - standarized_coupons,
           chosen_brand = 1) %>%
    select(id, date, sid, brand, price, chosen_brand)
  
  choices_data <- purchases_data %>%
    expand(nesting(id, date, sid), brand)
  
  market1_data <- left_join(choices_data, brand_price_data, by = c("date", "sid", "brand"))
  market1_data <- left_join(market1_data, purchases_data, by = c("id", "date", "sid", "brand")) %>%
    mutate(price = case_when(is.na(price) == TRUE ~ pre_coupon_price,
                             price < 0 ~ 0,
                             TRUE ~ price),
           chosen_brand = case_when(is.na(chosen_brand) == TRUE ~ 0,
                                    TRUE ~ chosen_brand),
           brand = case_when(brand == "DEL MONTE" ~ 1,
                             brand == "HEINZ" ~ 2,
                             brand == "HUNT'S" ~ 3,
                             brand == "STORE" ~ 4))
  
  market1_data <- market1_data %>%
    group_by(id, date, sid) %>%
    mutate(occasion = group_indices()) %>%
    ungroup
  
  market1_data <- market1_data %>%
    group_by(occasion) %>%
    mutate(some_price_missing = case_when(is.na(price) == TRUE ~ 1,
                                          TRUE ~ 0),
           sum_some_price_missing = sum(some_price_missing)) %>%
    ungroup %>%
    filter(sum_some_price_missing == 0) %>%
    select(occasion, id, date, sid, price, brand, 
           chosen_brand)
  
  return(market1_data)
  }

clean_upcs <- function() {
  upc_data <- read_dta("../raw_data/kchp_upc.dta")
  upc_data$upc <- format(upc_data$upc, digits = 13)
  upc_data$eqv <- format(upc_data$eqv, scientific = FALSE)
  upc_data <- upc_data %>%
    mutate(brand = case_when(str_extract(desc, "^(HEINZ){1}") == "HEINZ" ~ "HEINZ",
                             str_extract(desc, "^(HUNT'S){1}") == "HUNT'S" ~ "HUNT'S",
                             str_extract(desc, "^(DEL MONTE){1}") == "DEL MONTE" ~ "DEL MONTE",
                             (str_extract(desc, "^(CTL BR){1}") == "CTL BR") | 
                               (str_extract(desc, "^(GENERIC){1}") == "GENERIC") ~ "STORE",
                             TRUE ~ NA_character_),
           weight = wamt/1000) %>%
    select(upc, brand, weight)
  upc_data$weight <- as.numeric(format(upc_data$weight, digits =  0))
  return(upc_data)
}

clean_price_data <- function() {
  pf1_data <- read_dta("../raw_data/kchp_pf1.dta")
  pf1_data$date <- as.Date("1960-01-01") + pf1_data$date
  pf1_data$upc <- format(pf1_data$upc, digits = 13)
  pf1_data <- pf1_data %>%
    select(upc, sid, date, dpr, units)
  return(pf1_data)
}

store_date_brand_price <- function(pf1_data, upc_data) {
  price_data <- left_join(pf1_data, upc_data, by = "upc") %>%
    mutate(price_per_standard = 32*(dpr/weight),
           standarized_units = units/32) %>%
    group_by(upc) %>%
    mutate(total_standarized_sales = sum(standarized_units)) %>%
    ungroup
  price_data <- price_data %>%
    group_by(brand, sid, date) %>%
    mutate(pre_coupon_price = weighted.mean(price_per_standard, total_standarized_sales)) %>%
    select(brand, sid, date, upc, pre_coupon_price)
  return(price_data)
}

clean_hp_data <- function() {
  hp1_data <- read_dta("../raw_data/kchp_hp1.dta")
  hp1_data$date <- as.Date("1960-01-01") + hp1_data$date
  hp1_data$upc <- format(hp1_data$upc, digits = 13)
  hp1_data <- hp1_data %>%
    select(id, date, trip, upc, sid, units, weight, scval, mcval) %>%
    mutate(standarized_coupons = (32/(weight*units))*(scval + mcval)) %>%
    arrange(id, date, upc) %>%
    select(id, date, sid, upc, standarized_coupons) %>%
    distinct(id, date, sid, .keep_all = TRUE)
  return(hp1_data)
}

# Execute
market1_data <- main()
write_dta(market1_data, "../temp/market1_data.dta", version = 14)

