#### Preliminaries ####
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
rm(list = ls())
options(scipen=999)

library(tibble)
library(dplyr)
library(testit)

clean_data <- read.csv("../temp/clean_data.csv", header = TRUE)

# (c)
clean_data <- as_tibble(clean_data)
sri_lanka <- clean_data %>%
  filter(country == "SriLanka")

beta <- as.numeric(sri_lanka %>%
  mutate(beta_i = 0.025*X/Y) %>%
  summarise(weighted.mean(beta_i, X)))

# (d)

clean_data <- clean_data %>%
  mutate(A = Y/(X^beta))
clean_data <- clean_data %>%
  group_by(country) %>%
  mutate(total_inputs = sum(X),
         country_needed_term = sum(A^(1/(1-beta)))) %>%
  ungroup
clean_data <- clean_data %>%
  mutate(firm_needed_term = A^(1/(1-beta)),
         X_efficient = (total_inputs*firm_needed_term)/country_needed_term,
         Y_efficient = A*X_efficient^{beta})

# sanity check
print(sum(clean_data$X))
print(sum(clean_data$X_efficient))
assert(round(sum(clean_data$X), 10) == round(sum(clean_data$X_efficient), 10))

by_country_data <- clean_data %>%
  group_by(country) %>%
  mutate(total_Y = sum(Y),
         total_Y_efficient = sum(Y_efficient)) %>%
  distinct(country, total_Y, total_Y_efficient)
by_country_data <- by_country_data %>%
  mutate(perc_chg_move_to_eff = (total_Y_efficient - total_Y)/total_Y*100)
