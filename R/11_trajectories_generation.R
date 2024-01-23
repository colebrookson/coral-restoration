# load packages and the data we need ===========================================
library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)
library(qs)

source(here::here("./R/cc/00_functions.R"))

`%notin%` <- Negate(`%in%`)

restoration_sim <- function(t,state,parameters){
  
  with(as.list(c(state,parameters)),{
    
    dM1 <- a*M1*C1 - (g*M1)/(M1+Tu1) + y*M1*Tu1 
    dC1 <- r*Tu1*C1 - d*C1 - a*M1*C1 + z*r*Tu1
    dT1 <- (g*M1)/(M1+Tu1) - y*M1*Tu1 - r*Tu1*C1 + d*C1 - z*r*Tu1
    list(c(dM1,dC1,dT1))
    
  })
}

# NHRCP simulation =============================================================
# NHRCP: C_i = 0.25, M_i = 0.01, g = 0-0.2, gamma = 0.66-0.99, z = 0
# New Heaven Reef Conservation Program 

Cinit <- 0.25; Minit <- 0.01; Tinit <- 1 - (Cinit + Minit)
g_val_all <- seq(0, 0.2, by = 0.01)
mc_comp_all <- seq(0.66, 0.99, by = 0.01)
recruitvalue_all <- 0

param_grid <- expand.grid(Cinit = Cinit, Minit = Minit, Tinit = Tinit, 
                          g_val = g_val_all, mc_comp = mc_comp_all, 
                          recruitvalue = recruitvalue_all)


# the times for the simulation 
times <- seq(0,2000, by = 0.1)

res_df <- data.frame(
  time = as.numeric(),
  M1 = as.numeric(), 
  C1 = as.numeric(), 
  Tu1 = as.numeric(), 
  Cinit = as.numeric(), 
  Minit = as.numeric(),
  Tinit = as.numeric(),
  g_val = as.numeric(),
  mc_comp = as.numeric(),
  recruitvalue = as.numeric()
)

for(row in seq_len(nrow(param_grid))) {
 
  # get the parameters
  parameters <- c(
    a <- param_grid$mc_comp[row], 
    d <- 0.24, 
    g <- param_grid$g_val[row], 
    r <- 0.55, 
    y <- 0.77, 
    z <- param_grid$recruitvalue[row]
  )
  
  # giving M1 and M2, C1 and C2, T1 and T2 starting conditions
  state <- c(
    M1 = param_grid$Minit[row], 
    C1 = param_grid$Cinit[row], 
    Tu1 = param_grid$Tinit[row]
  )
  
  # RUN THE ODE
  out <- lsode(
    y = state, 
    times = times, 
    func = restoration_sim,
    parms = parameters)	
  
  # get dataframe of the other info 
  vals_df <- do.call("rbind", replicate(nrow(out), 
                                        param_grid[row, ], simplify = FALSE))
  res <- cbind(out, vals_df)
  
  res_df <- rbind(res_df, res)
  
}

qs::qsave(res_df, here::here("./data/plotting-data/nhrcp-simulation.qs"))

## set up nhrcp data to be plotted =============================================
nhrcp_df <- res_df %>% 
  dplyr::group_by(g_val, mc_comp, recruitvalue) %>% 
  dplyr::mutate(group_num = dplyr::cur_group_id())

mean_nhrcp <- nhrcp_df %>% 
  dplyr::group_by(time) %>% 
  dplyr::summarize(
    M = mean(M1),
    C = mean(C1),
    Tu = mean(Tu1)
  )

qs::qsave(mean_nhrcp, here::here("./data/plotting-data/mean-nhrcp-sims.qs"))

# TNC simulation ===============================================================

# TNC: C_i = 0.1, M_i = 0.15, g = 0-0.2, gamma = 0-0.33, z = 0-0.33

Cinit <- 0.1; Minit <- 0.15; Tinit <- 1 - (Cinit + Minit)
g_val_all <- seq(0, 0.2, by = 0.02)
mc_comp_all <- seq(0.0, 0.33, by = 0.05)
recruitvalue_all <- seq(0, 0.33, by = 0.05)

param_grid <- expand.grid(Cinit = Cinit, Minit = Minit, Tinit = Tinit, 
                          g_val = g_val_all, mc_comp = mc_comp_all, 
                          recruitvalue = recruitvalue_all)


# the times for the simulation 
times <- seq(0,2000, by = 0.1)

res_df <- data.frame(
  time = as.numeric(),
  M1 = as.numeric(), 
  C1 = as.numeric(), 
  Tu1 = as.numeric(), 
  Cinit = as.numeric(), 
  Minit = as.numeric(),
  Tinit = as.numeric(),
  g_val = as.numeric(),
  mc_comp = as.numeric(),
  recruitvalue = as.numeric()
)
#res_df <- do.call("rbind", replicate((nrow(param_grid)*20004), 
#                           res_df, simplify = FALSE))

for(row in seq_len(nrow(param_grid))) {
  
  # get the parameters
  parameters <- c(
    a <- param_grid$mc_comp[row], 
    d <- 0.24, 
    g <- param_grid$g_val[row], 
    r <- 0.55, 
    y <- 0.77, 
    z <- param_grid$recruitvalue[row]
  )
  
  # giving M1 and M2, C1 and C2, T1 and T2 starting conditions
  state <- c(
    M1 = param_grid$Minit[row], 
    C1 = param_grid$Cinit[row], 
    Tu1 = param_grid$Tinit[row]
  )
  
  # RUN THE ODE
  out <- lsode(
    y = state, 
    times = times, 
    func = restoration_sim,
    parms = parameters)	
  
  # get dataframe of the other info 
  vals_df <- do.call("rbind", replicate(nrow(out), 
                                        param_grid[row, ], simplify = FALSE))
  res <- cbind(out, vals_df)
  
  res_df <- rbind(res_df, res)
  
}

qs::qsave(res_df, here::here("./data/plotting-data/tnc-simulation.qs"))

## set up data to be plotted ===================================================

tnc_df <- res_df %>% 
  dplyr::group_by(g_val, mc_comp, recruitvalue) %>% 
  dplyr::mutate(group_num = dplyr::cur_group_id())

mean_tnc <- tnc_df %>% 
  dplyr::group_by(time) %>% 
  dplyr::summarize(
    M = mean(M1),
    C = mean(C1),
    Tu = mean(Tu1)
  )

qs::qsave(mean_tnc, here::here("./data/plotting-data/mean-tnc-sims.qs"))

# NHRCP recovery simulation ====================================================

#' This is simulating from the "final" restoration values instead of the 
#' initial values

Cinit <- 0.6; Minit <- 0.01; Tinit <- 1 - (Cinit + Minit)
g_val_all <- seq(0, 0.2, by = 0.01)
mc_comp_all <- seq(0.66, 0.99, by = 0.01)
recruitvalue_all <- 0

param_grid <- expand.grid(Cinit = Cinit, Minit = Minit, Tinit = Tinit, 
                          g_val = g_val_all, mc_comp = mc_comp_all, 
                          recruitvalue = recruitvalue_all)


# the times for the simulation 
times <- seq(0,500, by = 0.1)

res_df <- data.frame(
  time = as.numeric(),
  M1 = as.numeric(), 
  C1 = as.numeric(), 
  Tu1 = as.numeric(), 
  Cinit = as.numeric(), 
  Minit = as.numeric(),
  Tinit = as.numeric(),
  g_val = as.numeric(),
  mc_comp = as.numeric(),
  recruitvalue = as.numeric()
)

for(row in seq_len(nrow(param_grid))) {
  
  # get the parameters
  parameters <- c(
    a <- param_grid$mc_comp[row], 
    d <- 0.24, 
    g <- param_grid$g_val[row], 
    r <- 0.55, 
    y <- 0.77, 
    z <- param_grid$recruitvalue[row]
  )
  
  # giving M1 and M2, C1 and C2, T1 and T2 starting conditions
  state <- c(
    M1 = param_grid$Minit[row], 
    C1 = param_grid$Cinit[row], 
    Tu1 = param_grid$Tinit[row]
  )
  
  # RUN THE ODE
  out <- lsode(
    y = state, 
    times = times, 
    func = restoration_sim,
    parms = parameters)	
  
  # get dataframe of the other info 
  vals_df <- do.call("rbind", replicate(nrow(out), 
                                        param_grid[row, ], simplify = FALSE))
  res <- cbind(out, vals_df)
  
  res_df <- rbind(res_df, res)
  
}

qs::qsave(res_df, 
          here::here("./data/plotting-data/nhrcp-recovery-simulation.qs"))

## set up nhrcp data to be plotted =============================================
nhrcp_df <- res_df %>% 
  dplyr::group_by(g_val, mc_comp, recruitvalue) %>% 
  dplyr::mutate(group_num = dplyr::cur_group_id())

mean_nhrcp <- nhrcp_df %>% 
  dplyr::group_by(time) %>% 
  dplyr::summarize(
    M = mean(M1),
    C = mean(C1),
    Tu = mean(Tu1)
  )

qs::qsave(mean_nhrcp, 
          here::here("./data/plotting-data/mean-nhrcp-sims-recovery.qs"))

# TNC recovery simulation ======================================================

# TNC: C_i = 0.1, M_i = 0.15, g = 0-0.2, gamma = 0-0.33, z = 0-0.33

Cinit <- 0.15; Minit <- 0.13; Tinit <- 1 - (Cinit + Minit)
g_val_all <- seq(0, 0.2, by = 0.02)
mc_comp_all <- seq(0.0, 0.33, by = 0.05)
recruitvalue_all <- seq(0, 0.33, by = 0.05)

param_grid <- expand.grid(Cinit = Cinit, Minit = Minit, Tinit = Tinit, 
                          g_val = g_val_all, mc_comp = mc_comp_all, 
                          recruitvalue = recruitvalue_all)


# the times for the simulation 
times <- seq(0,500, by = 0.1)

res_df <- data.frame(
  time = as.numeric(),
  M1 = as.numeric(), 
  C1 = as.numeric(), 
  Tu1 = as.numeric(), 
  Cinit = as.numeric(), 
  Minit = as.numeric(),
  Tinit = as.numeric(),
  g_val = as.numeric(),
  mc_comp = as.numeric(),
  recruitvalue = as.numeric()
)
#res_df <- do.call("rbind", replicate((nrow(param_grid)*20004), 
#                           res_df, simplify = FALSE))

for(row in seq_len(nrow(param_grid))) {
  
  # get the parameters
  parameters <- c(
    a <- param_grid$mc_comp[row], 
    d <- 0.24, 
    g <- param_grid$g_val[row], 
    r <- 0.55, 
    y <- 0.77, 
    z <- param_grid$recruitvalue[row]
  )
  
  # giving M1 and M2, C1 and C2, T1 and T2 starting conditions
  state <- c(
    M1 = param_grid$Minit[row], 
    C1 = param_grid$Cinit[row], 
    Tu1 = param_grid$Tinit[row]
  )
  
  # RUN THE ODE
  out <- lsode(
    y = state, 
    times = times, 
    func = restoration_sim,
    parms = parameters)	
  
  # get dataframe of the other info 
  vals_df <- do.call("rbind", replicate(nrow(out), 
                                        param_grid[row, ], simplify = FALSE))
  res <- cbind(out, vals_df)
  
  res_df <- rbind(res_df, res)
  
}

qs::qsave(res_df, 
          here::here("./data/plotting-data/tnc-simulation-recovery.qs"))

## set up data to be plotted ====================================================

tnc_df <- res_df %>% 
  dplyr::group_by(g_val, mc_comp, recruitvalue) %>% 
  dplyr::mutate(group_num = dplyr::cur_group_id())

mean_tnc <- tnc_df %>% 
  dplyr::group_by(time) %>% 
  dplyr::summarize(
    M = mean(M1),
    C = mean(C1),
    Tu = mean(Tu1)
  )

qs::qsave(mean_tnc, 
          here::here("./data/plotting-data/mean-tnc-sims-recovery.qs"))
