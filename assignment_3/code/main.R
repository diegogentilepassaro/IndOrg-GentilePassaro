#### Preliminaries ####
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
rm(list = ls())
options(scipen=999)

unlink("../temp", recursive = TRUE)
unlink("../output", recursive = TRUE)
dir.create("../temp")
dir.create("../output")

library(dplyr)
library(tidyr)
library(tibble)
library(lfe)
library(testit)

source("preclean.R")
system.time(source("analysis.R"))