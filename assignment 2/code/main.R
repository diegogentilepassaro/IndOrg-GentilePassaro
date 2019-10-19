#### Preliminaries ####
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
rm(list = ls())
options(scipen=999)

unlink("../temp", recursive = TRUE)
unlink("../output", recursive = TRUE)
dir.create("../temp")
dir.create("../output")

source("preclean.R")
source("analysis_q1.R")
source("analysis_q2.R")
