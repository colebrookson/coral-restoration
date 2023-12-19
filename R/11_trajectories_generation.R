# load packages and the data we need ===========================================
library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)
library(qs)

source(here::here("./R/cc/00_functions.R"))

`%notin%` <- Negate(`%in%`)

traj_sim <- function(mc_comp, g_val, recruitvalue, M1, C1, T1) {
  
  # the times for the simulation 
  times <- seq(0,2000, by = 0.1)
  
  # get the parameters
  parameters <- c(
    a <- mc_comp, 
    d <- 0.24, 
    g <- g_val, 
    r <- 0.55, 
    y <- 0.77, 
    z <- recruitvalue
  )
  
  # giving M1 and M2, C1 and C2, T1 and T2 starting conditions
  state <- c(M1 = M1, C1 = C1, Tu1 = T1)
  
  # RUN THE ODE
  out <- lsode(
    y = state, 
    times = times, 
    func = MumbyOpen_Restoration,
    parms = parameters)	
  
  return(out)
  
}

# NHRCP: C_i = 0.25, M_i = 0.01, g = 0-0.2, gamma = 0.66-0.99, z = 0
# New Heaven Reef Conservation Program 

C1 <- 0.25; M1 <- 0.01; T1 <- 1 - (C1 + M1)
g_val <- seq(0, 0.2, by = 0.01)
mc_comp <- seq(0.66, 0.99, by = 0.01)
recruitvalue <- 0

param_grid <- expand.grid(C1, M1, T1, g_val, mc_comp, recruitvalue)

res_df <- 
  


# TNC: C_i = 0.1, M_i = 0.15, g = 0-0.2, gamma = 0-0.33, z = 0-0.33