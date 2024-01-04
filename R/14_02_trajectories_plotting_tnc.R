# load packages and the data we need ===========================================
library(tidyverse)
library(here)
library(qs)

source(here::here("./R/cc/00_functions.R"))
source(here::here("./R/00_global_funs.R"))
`%notin%` <- Negate(`%in%`)

tnc_df <- qs::qread(here::here("./data/plotting-data/tnc-simulation.qs"))