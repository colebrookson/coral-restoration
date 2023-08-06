library(dplyr)
library(readr)
library(here)

# define variables =============================================================

# a = rate macroalgae overgrows turf algae
a_vec <- seq(0, 0.99, 0.05)

# g = rate of grazing on algae
g_vec <- seq(0, 0.99, 0.01)

# z = rate coral larvae recruit and overgrow turf algae
z_vec <- seq(0, 0.99, 0.05)

# get length of unique number of params 
length(a_vec) * length(g_vec) * length(z_vec)

equil_nums <- c(1:20)

# put them all in a data.frame
df <- expand.grid(Equilibrium = equil_nums,
                  a = a_vec, g  = g_vec, z = z_vec)

# now add in the other variables that stay consistent
df$r <- 0.55; df$d <- 0.24; df$y <- 0.77
df$C <- 2; df$M <- 2; df$eig_1 <- 2; df$eig_2 <- 2

# order them as we had them already 
df <- df %>% dplyr::select(r, d, y, a, g, z, Equilibrium, C, M, eig_1, eig_2)

#save as a csv
readr::write_csv(
  df,
  here("./data/parameter-data/full-params-for-matlab.csv"),
  col_names = FALSE
)

# check the old version ========================================================
df <- readr::read_csv(here("./data/parameter-data/dep/full_combo_data.csv"))
names(df) <- c("r","d","y","a","g","z",
                          "Equilibrium","C","M","eig_1","eig_2")
