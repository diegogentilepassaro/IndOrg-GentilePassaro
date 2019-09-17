#### Preliminaries ####
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
rm(list = ls())

source("iv_reg.R")

n <- 1000
y <- rnorm(n, mean = 3, sd = 1)
x1 <- rnorm(n, mean = 1, sd = 2)
X <- cbind(x1)
endo <- rnorm(n, mean = 2, sd = 2)
instr <-endo + rnorm(n, mean = 1, sd = 1) 
year  <- as.integer(rnorm(n, mean = 1990, sd = 10))
state <- as.integer(runif(n, min = 1, max = 50))
test_iv_reg <- iv_reg(y, X, endo, instr)
test_iv_reg$summary

test_iv_reg_robust <- iv_reg(y, X, endo, instr, robust = TRUE)
test_iv_reg_robust$summary

test_iv_reg_one_factor <- iv_reg(y, X, endo, instr, factor_var1 = year)
test_iv_reg_one_factor$summary
test_iv_reg_two_factors <- iv_reg(y, X, endo, instr, factor_var1 = year, factor_var2 = state)
test_iv_reg_two_factors$summary