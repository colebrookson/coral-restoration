# load packages and the data we need ===========================================
library(scales)
library(deSolve)
library(tidyverse)
library(here)
library(qs)

data <- qs::qread(here("./data/allparam_data_ordered.qs"))

# set parameter values =========================================================
recruitvalue_vec = rep(c(0, 0.05, 0.25, 0.5), each = 3*4)
g_val_vec = rep(rep(c(0.1, 0.21, 0.5), each = 4),4)
mc_comp_vec = rep(c(0.05, 0.3, 0.5, 0.99),4*3)
