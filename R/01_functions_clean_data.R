library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)

# add in important info to the data ============================================

# read in data
data <- readr::read_delim(
  here("./data/parameter-data/full-restoration-model-output.txt"),
  col_names = c("r","d","y","a","g","z",
                "Equilibrium","C","M","eig_1","eig_2"))

# remove empty rows
data <- data[data$C <= 1, ]

#need to add a 'stability' column
data$stability <- NA
data$stability[data$eig_1 > 0 & data$eig_2 > 0] <- 'unstable_node'
data$stability[data$eig_1 < 0 & data$eig_2 < 0] <- 'stable_node'
data$stability[data$eig_1 == 0 | data$eig_2 == 0] <- 'bifurcation_point'
data$stability[is.na(data$stability)] <- 'saddle_node'

#give each of the stability designations an associated colour
data$Colour <- NA
data$Colour[data$stability == "stable_node"] <- 'black'
data$Colour[data$stability == "unstable_node"] <- 'gold'
data$Colour[data$stability == "saddle_node"] <- 'purple'
data$Colour[data$stability == "bifurcation_point"] <- 'green'

# get all parameter combinations ===============================================
data <- data %>% 
  dplyr::mutate(
    a_fac = as.factor(a),
    g_fac = as.factor(g),
    z_fac = as.factor(z)
  ) %>% 
  dplyr::group_by(g_fac, a_fac, z_fac) %>% 
  dplyr::mutate(
    paramcombo = dplyr::cur_group_id()
  )
max(data$paramcombo)

# split up data into smaller easy to work with bits ============================

# # split the paramater combos into three roughly equal groups
# param_groups <- split(unique(data$paramcombo), 
#                       unique(data$paramcombo)%%3)
# 
# data_g1 <- data[which(data$paramcombo %in% param_groups$`0`), ]
# data_g2 <- data[which(data$paramcombo %in% param_groups$`1`), ]
# data_g3 <- data[which(data$paramcombo %in% param_groups$`2`), ]
# 
# # save data after cleaning =====================================================
# 
# readr::write_csv(
#   data_g1,
#   here::here("./data/parameter-data/all-param-vals-unordered-1.csv")
# )
# readr::write_csv(
#   data_g2,
#   here::here("./data/parameter-data/all-param-vals-unordered-2.csv")
# )
# 
# readr::write_csv(
#   data_g3,
#   here::here("./data/parameter-data/all-param-vals-unordered-3.csv")
# )