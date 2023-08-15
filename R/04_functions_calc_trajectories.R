# set up =======================================================================
library(scales)
library(deSolve)
library(geometry)
library(fields)
library(here)
library(tidyverse)


#LOAD IN data
data <- qs::qread(here("./data/parameter-data/allparam_data_ordered.qs"))

# create grid of starting points ===============================================

# the grid is a bunch of starting points in the region of [0,1] x [0,1] that is 
# allowed by 1 = M + C + T

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
print(paste("The first row of grid = ", grid[1,]))

#can remove the Point_Num column, it was really just a placeholder column
grid$Point_Num <- NULL

# set the equation for the simulation ==========================================

MumbyOpen_Restoration <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    dM1 <- a*M1*C1 - (g_val*M1)/(M1+Tu1) + y*M1*Tu1 
    dC1 <- r*Tu1*C1 - d*C1 - a*M1*C1 + z*r*Tu1
    dT1 <- (g_val*M1)/(M1+Tu1) - y*M1*Tu1 - r*Tu1*C1 + d*C1 - z*r*Tu1
    list(c(dM1,dC1,dT1))
  })
}

ntrajectory <- dim(grid)[1]
times <- seq(0,2000, by = 0.1)
npoints <- length(times)
initM <- grid$Minit
initC <- grid$Cinit
initT <- grid$Tinit

#initializing limits for whether a trajectory made it to an attractor or not
#needs to be within a 0.005 radius from the equi point (i.e. within the same 
# grid point? i think?) in all components?
radius <- 0.005 
times <- seq(0,2000, by = 0.1) #changed from 0,100 by = 0.1
#needs to spend last tenth of the total time within 0.005 radius of stable_node
finaltime <- floor(length(times)*0.1) 
#should all end up by some stable point, if ran the simulation for long enough

# make calculating trajectory functions ========================================
CalcTrajectories <- function(i, parameters, recruitvalue, g_val, mc_comp, 
                             ntrajectory, times, mumbytrajectories, initM,
                             initC, initT, MumbyOpen_Restoration){
  for(i in 1:ntrajectory){
    
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
    state <- c(M1 = initM[i], C1 = initC[i], Tu1 = initT[i])
    
    # RUN THE ODE
    out <- lsode(
      y = state, 
      times = times, 
      func = MumbyOpen_Restoration,
      parms = parameters)	
    
    # get the data into the dataframe 
    mumbytrajectories[mumbytrajectories$Run == i,"M1"] <- out[,2]
    mumbytrajectories[mumbytrajectories$Run == i,"C1"] <- out[,3]
    mumbytrajectories[mumbytrajectories$Run == i,"T1"] <- out[,4]
  }
  
  return(mumbytrajectories)
}

BOA <- function(mc_comp, recruitvalue, g_val, data_IDs, basinofattractionID,
                basins, ntrajectory, radius,times, finaltime){

  # colour code each grid point as determined by which basin 
  # of attraction it's in number of stable equi at that recruitment and 
  # grazing value combo
  numequi <- dim(
    data_IDs[data_IDs$z == recruitvalue & 
               data_IDs$g == g_val & 
               data_IDs$a == mc_comp & 
               data_IDs$stability == "stable_node",])[1]
  
  #coordinates of the stable equi at that recruitment and grazing value combo
  M1equi <- data_IDs$M[data_IDs$z == recruitvalue & 
                         data_IDs$g == g_val & 
                         data_IDs$a == mc_comp & 
                         data_IDs$stability == "stable_node"]

  C1equi <- data_IDs$C[data_IDs$z == recruitvalue & 
                         data_IDs$g == g_val & 
                         data_IDs$a == mc_comp & 
                         data_IDs$stability == "stable_node"]
  
  for(n in 1:ntrajectory){
    for(m in 1:numequi){

      # if stay within that radius for the final 10th of the time, 
      # initial conditions + run # assigned that colour + equi number
      if((all(((M1equi[m] - radius) <  
              mumbytrajectories$M1[mumbytrajectories$Run == n & 
                                mumbytrajectories$TimeStep > 
                                (length(times) - finaltime)])) && 
          all(((M1equi[m] + radius) >
           mumbytrajectories$M1[mumbytrajectories$Run == n &
                                mumbytrajectories$TimeStep > 
                                (length(times) - finaltime)]))) & 
        (all(((C1equi[m] - radius) < 
          mumbytrajectories$C1[mumbytrajectories$Run == n & 
                               mumbytrajectories$TimeStep > 
                               (length(times) - finaltime)])) && 
         all(((C1equi[m] + radius) > 
          mumbytrajectories$C1[mumbytrajectories$Run == n & 
                               mumbytrajectories$TimeStep > 
                               (length(times) - finaltime)])))){
        basinofattractionID$Equilibrium[basinofattractionID$InitCond ==  n] <- 
          data_IDs$ID[data_IDs$z == recruitvalue &
                        data_IDs$g == g_val & 
                        data_IDs$a == mc_comp & 
                        data_IDs$stability == "stable_node" & 
                        data_IDs$M == M1equi[m] &
                        data_IDs$C == C1equi[m]]
        
        basins$Size[basins$RecruitValue == recruitvalue & 
                      basins$mc_comp == mc_comp & 
                      basins$Grazing == g_val & 
                      basins$EquilibriumID == 
                      data_IDs$ID[data_IDs$z == recruitvalue & 
                                    data_IDs$g == g_val &
                                    data_IDs$a == mc_comp & 
                                    data_IDs$stability == "stable_node" &
                                    data_IDs$M == M1equi[m] & data_IDs$C ==
                                    C1equi[m]]] <- 
          1 + basins$Size[basins$RecruitValue == recruitvalue &
                            basins$mc_comp == mc_comp & 
                            basins$Grazing == g_val & 
                            basins$EquilibriumID == 
                            data_IDs$ID[data_IDs$z == recruitvalue & 
                                          data_IDs$g == g_val & 
                                          data_IDs$a == mc_comp & 
                                          data_IDs$stability == "stable_node" &
                                          data_IDs$M == M1equi[m] & 
                                          data_IDs$C == C1equi[m]]]}
    }}
  
  output <- list(
    "basinofattractionID" <- basinofattractionID, 
    "basins" <- basins)
  
  return(output)
}

# get the values of the parameters
recruitvalue_vec = unique(data$a) 
g_val_vec = unique(data$g) 
mc_comp_vec = unique(data$z)

param_grid <- expand.grid(unique(data$a), unique(data$g), unique(data$z))

# loop through each of the parameter values and follow the process outlined in 
# these two functions
for(j in 1:1) { #length(g_val_vec)){
  
  # some initial NA values
  mumbytraj <- NA
  mumbytrajectories <- NA
  basins<-NA
  basinofattractionID <- NA	
  recruitvalue <- recruitvalue_vec[j]
  g_val <- g_val_vec[j]
  mc_comp <- mc_comp_vec[j]
  
  #initializing the dataframes beforehand
  mumbytrajectories <- data.frame(
    Run=rep(1:ntrajectory, each = npoints),
    M1 = NA, 
    C1 = NA,
    T1 = NA, 
    TimeStep = rep(1:npoints))
  
  print(paste("starting j = ", j))
  start_time <- Sys.time()
  
  i=1
  # set parameter values 
  parameters <- c(
    a <- mc_comp, 
    d <- 0.24, 
    g <- g_val, 
    r <- 0.55, 
    y <- 0.77, 
    z <- recruitvalue)
  
  # run the trajectories function 
  mumbytraj <- CalcTrajectories(
    i, parameters, recruitvalue, g_val, mc_comp, 
    ntrajectory, times, mumbytrajectories, 
    initM, initC, initT, MumbyOpen_Restoration)
  print(paste("trajectories calculated for j = ", j))
  
  # put in the trajectories 
  mumbytrajectories <- mumbytraj
  save(
    mumbytrajectories, 
    file = here("data", "boa-outputs", "trajectory_files", 
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
  
  output <- BOA(
    mc_comp = mc_comp, 
    recruitvalue = recruitvalue,
    g_val = g_val, 
    data_IDs = data, 
    basinofattractionID = basinofattractionID, 
    basins = basins, 
    ntrajectory = ntrajectory, 
    radius = radius, 
    times = times, 
    finaltime = finaltime)
  
  basinofattractionID <- output[[1]]
  basins <- output[[2]]
  basins$Grazing <- as.numeric(as.character(basins$Grazing))
  basins$RecruitValue <- as.numeric(as.character(basins$RecruitValue))
  basins$mc_comp <- as.numeric(as.character(basins$mc_comp))
  save(basinofattractionID, 
       file = here("data", "boa-outputs", "basinofattractionID_files",
                   paste0("basinofattractionID_recr",
                          recruitvalue,"g",g_val,"_mccomp"
                          ,mc_comp,"_20000.RData")))
  
  basinsabr <- basins[basins$RecruitValue == recruitvalue & 
                        basins$Grazing == g_val & 
                        basins$mc_comp == mc_comp,] 
  save(basinsabr, file = here("data", "boa-outputs", 
                              "basinsabr_files",
                              paste0("basins_recr",
                                     recruitvalue,"g",g_val,
                                     "_mccomp",mc_comp,"_20000.RData")))
  
  print(paste("finished j = ", j))
  end_time <- Sys.time()
  print(end_time - start_time)
}

#Plotting basinofattraction data in a C vs M scatterplot
recruitvalue_vec = unique(data$a) 
g_val_vec = unique(data$g)
mc_comp_vec = unique(data$z)

#see: PrepforDarlingLabMeeting_MatchingIDvals.R 
# onlymalg <- c(7,32,52,101)
# onlycoral <- c(2,213,330)
# mostlymalg <- c(111,138,159,209,225,252,273,324,363,383,433)
# malgcoral <- 339

# make plot 
for(j in 1:length(g_val_vec)){
  basinofattractionID <- NA
  recruitvalue <- recruitvalue_vec[j]
  g_val <- g_val_vec[j]
  mc_comp <- mc_comp_vec[j]
  load(here("data", "boa-outputs", 
            "basinofattractionID_files",
            paste0("basinofattractionID_recr",
                   recruitvalue,"g",g_val,"_mccomp",mc_comp,"_20000.RData")))

    basinofattractionID$colour <- NA
  basinofattractionID <- basinofattractionID %>%
    rowwise() %>%
    mutate(colour = as.character(colour), 
           colour = case_when(
             (Equilibrium %in% c(7,32,52,101)) ~ "#0E4D92",
             (Equilibrium %in% c(2,213,330)) ~ "#A00000",
             (Equilibrium %in% c(111,138,159,209,225,252,273,324,363,383,433)) ~ "#90EE90",
             (Equilibrium == 339) ~ "#FFC0CB",
             TRUE ~ colour)) 
  pdf(here("graphs", "BOAplots", 
           paste0("PLOT_basinofattractionID_recr",recruitvalue,"g",
                  g_val,"_mccomp",mc_comp,"_20000.pdf")))
  plot(x=basinofattractionID$initM1, y=basinofattractionID$initC1, 
       col = basinofattractionID$colour, 
       xlab = "Percent Initial Macroalgal Level", 
       ylab = "Percent Initial Coral Level", 
       main = paste("Dispersal = ", recruitvalue, "Grazing Rate = ", 
                    g_val, "Macroalgal-Coral Level = ", mc_comp), 
       pch = 16, cex = 2) 
  legend("topright", c("Only Macroalgae", "Only Coral", 
                       "Mostly Macroalgae", 
                       "MAlg + Coral"), 
         col = c("#0E4D92", "#A00000", "#90EE90","#FFC0CB"), pch = c(20,20))
  dev.off()
}

