#### Preliminaries ####
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
rm(list = ls())

source("ols_reg.R")

n <- 1000
y <- rnorm(n, mean = 3, sd = 1)
x1 <- rnorm(n, mean = 1, sd = 2)
year  <- as.integer(rnorm(n, mean = 1990, sd = 10))
state <- as.integer(runif(n, min = 1, max = 50))
X <- cbind(x1)
test_ols_reg <- ols_reg(y, X)
test_ols_reg$summary

test_ols_reg_robust <- ols_reg(y, X, robust = TRUE)
test_ols_reg_robust$summary

test_ols_reg_one_factor <- ols_reg(y, X, factor_var1 = year)
test_ols_reg_one_factor$summary
test_ols_reg_two_factors <- ols_reg(y, X, factor_var1 = year, factor_var2 = state)
test_ols_reg_two_factors$summary