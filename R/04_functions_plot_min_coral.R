# load packages and the data we need ===========================================
library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)
library(qs)

# read in the data with all the IDs ============================================
data <- qs::qread(here("./data/parameter-data/allparam_data_old_ordered.qs"))

data <- data[, names(data)[which(!is.na(names(data)))]]
# get the correct IDs ==========================================================

gt30_ids <- data %>% 
  dplyr::filter(C >= 0.3) %>% 
  dplyr::distinct(ID)

# loop through all the files ===================================================

# NOTE
#' So what we'll do here is loop through each basin of attraction file, 
#' pull in the info, see if any of the ids are in in the file, if so, find 
#' the minimum coral value that corresponds to that id 

# get the list of all the files 
