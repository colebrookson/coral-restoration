# set up =======================================================================
library(scales)
library(deSolve)
library(geometry)
library(fields)
library(here)
library(tidyverse)

# pull in the functions that we need to do the simulations
source(here("./R/cc/00_functions.R"))

# the values needed for each simulation
args = commandArgs(TRUE)
g_val = as.numeric(args[1])
recruitvalue = as.numeric(args[2])
mc_comp = as.numeric(args[3])

# create grid of starting points ===============================================

# the grid is a bunch of starting points in the region of [0,1] x [0,1] that is 
# allowed by 1 = M + C + T
start <- Sys.time()
#x and y coords of all of the initial starting points
interval <- 0.05
x_coords <- seq(0.01,0.99, by = interval)
y_coords <- seq(0.01,0.99,by = interval)


#make dataframe 
grid <- data.frame(
  Point_Num = seq(1, length(x_coords) * length(y_coords),by = 1), 
  Minit = rep(x_coords, each = length(y_coords)), 
  Cinit = NA, 
  Tinit = NA)

#populate C column - want a lower triangle situation in an MxC graph
for(i in 1:length(x_coords)){ 
  val <- x_coords[i]
  grid$Cinit[grid$Minit == val] <- c(
    seq(0.01,1-val,by=interval),
    rep(NA, i)) # want C+M in each row to be =< 1, hence the NAs
}
plot(grid$Minit,grid$Cinit,pch=20)

#remove the NA rows
grid <- grid[-which(is.na(grid$Cinit)),]

#populate T column
for(i in 1:dim(grid)[1]){
  #the sum of each row should be 1
  grid$Tinit[i] <- 1 - grid$Minit[i] - grid$Cinit[i]
  grid$Tinit[i] <- round(grid$Tinit[i],digits = 3)
}

#can remove the Point_Num column, it was really just a placeholder column
grid$Point_Num <- NULL

# set the values for the simulation ============================================
ntrajectory <- dim(grid)[1]
times <- seq(0,2000, by = 0.1)
npoints <- length(times)
initM <- grid$Minit
initC <- grid$Cinit
initT <- grid$Tinit

#initializing limits for whether a trajectory made it to an attractor or not
#needs to be within a 0.005 radius from the equi point 
radius <- 0.005 
times <- seq(0,2000, by = 0.1) #changed from 0,100 by = 0.1

#needs to spend last tenth of the total time within 0.005 radius of stable_node
finaltime <- floor(length(times)*0.1) 

# instead of the loop this is where the simulation bit happens =================

# some initial NA values
mumbytraj <- NA
mumbytrajectories <- NA
basins<-NA
basinofattractionID <- NA	
# recruitvalue <- recruitvalue_vec[j]
# g_val <- g_val_vec[j]
# mc_comp <- mc_comp_vec[j]

#initializing the dataframes beforehand
mumbytrajectories <- data.frame(
  Run=rep(1:ntrajectory, each = npoints),
  M1 = NA, 
  C1 = NA,
  T1 = NA, 
  TimeStep = rep(1:npoints))

# set parameter values 
parameters <- c(
  a <- mc_comp, 
  d <- 0.24, 
  g <- g_val, 
  r <- 0.55, 
  y <- 0.77, 
  z <- recruitvalue)

i=1 # this is the run

# do the actual simulation
mumbytraj <- CalcTrajectories(
  i = i, # this is the run number
  parameters = parameters, # all the parameters
  recruitvalue = recruitvalue,
  g_val = g_val,
  mc_comp = mc_comp,
  ntrajectory = ntrajectory, # the number of the trajectory (there are many)
  times = times, # times to be put into the simulation
  mumbytrajectories = mumbytrajectories, # the dataframe to use
  initM = initM,
  initC = initC,
  initT = initT,
  MumbyOpen_Restoration = MumbyOpen_Restoration # the function for simulation
  ) 

# save the trajectories
save(
  mumbytraj, 
  file = here("data", "boa-outputs", "updated", "trajectory_files", 
              paste0("mumbytrajectories_recr",
                     recruitvalue,"g",g_val,"_mccomp",
                     mc_comp,"_20000.RData")))

# data frame of results put in the values 
basins <- data.frame(
  RecruitValue = c(rep(recruitvalue,440)),
  Grazing=c(rep(g_val,440)),
  mc_comp=c(rep(mc_comp,440)),
  EquilibriumID=seq(1,440,by=1), 
  Size = 0, 
  numNA = -1)

basins$Grazing <- as.numeric(as.character(basins$Grazing))
basins$RecruitValue <- as.numeric(as.character(basins$RecruitValue))
basins$mc_comp <- as.numeric(as.character(basins$mc_comp))

basinofattractionID <- data.frame(
  InitCond=rep(1:ntrajectory), 
  Equilibrium = NA, 
  initM1 = initM[1:ntrajectory], 
  initC1 = initC[1:ntrajectory], 
  initT1 = initT[1:ntrajectory])

# calculate the basins of attraction given the trajectories
output <- BOA(
  mc_comp = mc_comp, 
  recruitvalue = recruitvalue,
  g_val = g_val, 
  data_IDs = data, 
  basinofattractionID = basinofattractionID, 
  basins = basins, 
  ntrajectory = ntrajectory, 
  mumbytrajectories = mumbytraj,
  radius = radius, 
  times = times, 
  finaltime = finaltime)

basinofattractionID <- output[[1]]
basins <- output[[2]]
basins$Grazing <- as.numeric(as.character(basins$Grazing))
basins$RecruitValue <- as.numeric(as.character(basins$RecruitValue))
basins$mc_comp <- as.numeric(as.character(basins$mc_comp))

save(basinofattractionID, 
     file = here("data", "boa-outputs", "updated", "basinofattractionID_files",
                 paste0("basinofattractionID_recr",
                        recruitvalue,"g",g_val,"_mccomp"
                        ,mc_comp,"_20000.RData")))

basinsabr <- basins[basins$RecruitValue == recruitvalue & 
                      basins$Grazing == g_val & 
                      basins$mc_comp == mc_comp,] 
save(basinsabr, file = here("data", "boa-outputs", "updated", 
                            "basinsabr_files",
                            paste0("basins_recr",
                                   recruitvalue,"g",g_val,
                                   "_mccomp",mc_comp,"_20000.RData")))

end <- Sys.time()










