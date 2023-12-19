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
  
}