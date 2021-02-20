setwd("/home/agreiner/scratch/BoA_calculations/scenariofour") 

library(scales)
library(deSolve)
library(geometry)
library(fields)
#library(optparse)


#BOA_iterator.csv is being passed into bash and providing the arguments (args[#]), these would normally be iterated through using a for loop
#we'd have grazing level, MCcomp level, dispersal level instead of these 
args = commandArgs(TRUE)
glvl = args[1]
g_val = args[2]
recruitvalue = args[3]
lvl = args[4]
glvl <- as.numeric(glvl)
g_val <- as.numeric(g_val)
recruitvalue <- as.numeric(recruitvalue)
lvl <- as.numeric(lvl)

#recruitvalue = iterator[j,3]
#g_val =iterator[j,2]
#lvl = iterator[j,1]


#LOAD IN twopatch_ext_scfour_ID_full (here we'd load in allparam_data_abr from the CleanMatlabOutput_Make.... folder)
load('/home/agreiner/scratch/BoA_calculations/scenariofour/twopatch_ext_scfour_ID_full.RData')
print(paste("The first row of twopatch_ext_scfour_ID_full = ", twopatch_ext_scfour_ID_full[1,]))

#[PYTHON CODE (simulation_code) HANDLES FROM HERE TO]
#STEP 1: create grid of starting points for trajectories in the region of the [0,1] x [0,1] space allowed by 1 = M+C+T 

#x and y coords of all of the initial starting points
interval <- 0.05
x_coords <- seq(0.01,0.99, by = interval)
y_coords <- seq(0.01,0.99,by = interval)
#make dataframe 
grid <- data.frame(Point_Num=seq(1,length(x_coords)*length(y_coords),by=1),
                   Minit = rep(x_coords,each = length(y_coords)), 
                   Cinit = NA, Tinit = NA)
#populate C column - want a lower triangle situation in an MxC graph
for(i in 1:length(x_coords)){ 
  val <- x_coords[i]
  grid$Cinit[grid$Minit == val] <- c(seq(0.01,1-val,by=interval),rep(NA,i)) #want C+M in each row to be =< 1, hence the NAs
}
#plot(grid$Minit,grid$Cinit,pch=20) #whoo it worked

#remove the NA rows
grid <- grid[-which(is.na(grid$Cinit)),] 

#populate T column
for(i in 1:dim(grid)[1]){
  grid$Tinit[i] <- 1 - grid$Minit[i] - grid$Cinit[i] #the sum of each row should be 1
  grid$Tinit[i] <- round(grid$Tinit[i],digits = 3)
}
print(paste("The first row of grid = ", grid[1,]))

set.seed(2)
#make one for reef patch 2 [need it to be a sampled version of the first one]
reord <- sample(c(1:dim(grid)[1]))
gridtwo <- data.frame(Point_Num=seq(1,dim(grid)[1],by=1), Minit = NA, Cinit = NA, Tinit = NA)
for(i in 1:dim(gridtwo)[1]){
  gridtwo[i,] <- grid[reord[i],]
}
print(paste("The first row of grid2 = ", gridtwo[1,]))

#can remove the Point_Num column, it was really just a placeholder column
grid$Point_Num <- NULL
gridtwo$Point_Num <- NULL


MumbyOpen_Elmhirst_2PatchExt <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    dM1 <- a*M1*C1 - (glvl*M1)/(M1+Tu1) + gamma*(1-q_m)*M1*Tu1 + gamma*p_m*M2*Tu1
    dC1 <- r*(1-q_c)*Tu1*C1 - d*C1 - a*M1*C1 + p_c*r*C2*Tu1
    dT1 <- (glvl*M1)/(M1+Tu1) - gamma*(1-q_m)*M1*Tu1 + d*C1 - r*(1-q_c)*Tu1*C1 - (p_c*r*C2 + gamma*p_m*M2)*Tu1
    dM2 <- a*M2*C2 - (g*M2)/(M2+Tu2) + gamma*(1-p_m)*M2*Tu2 + gamma*q_m*M1*Tu2
    dC2 <- r*(1-p_c)*Tu2*C2 - d*C2 - a*M2*C2 + q_c*r*C1*Tu2
    dT2 <- (g*M2)/(M2+Tu2) - gamma*(1-p_m)*M2*Tu2 + d*C2 - r*(1-p_c)*Tu2*C2 - (gamma*q_m*M1 + q_c*r*C1)*Tu2
    list(c(dM1,dC1,dT1,dM2,dC2,dT2))
  })
}

#[HERE]

#this is where you store the size of the basins for each of the stable nodes for each of the parameter combinations
#850 is being used as 'max number of equilibria for a particular parameter combination'...but this is way too high, we've been seeing max 4 equilibria so you could just use '5' instead of '850' here
basins <- data.frame(RecruitValue=c(rep(seq(0,0.99,by=0.01),each=850*99),
                                    rep(seq(0,0.99,by=0.01),each=850*99),
                                    rep(seq(0,0.99,by=0.01),each=850*99),
                                    rep(seq(0,0.99,by=0.01),each=850*99),
                                    rep(seq(0,0.99,by=0.01),each=850*99),
                                    rep(seq(0,0.99,by=0.01),each=850*99),
                                    rep(seq(0,0.99,by=0.01),each=850*99),
                                    rep(seq(0,0.99,by=0.01),each=850*99),
                                    rep(seq(0,0.99,by=0.01),each=850*99),
                                    rep(seq(0,0.99,by=0.01),each=850*99),
                                    rep(seq(0,0.99,by=0.01),each=850*99),
                                    rep(seq(0,0.99,by=0.01),each=850*99)),
                     Grazing=c(rep(seq(0.01,0.99,by=0.01),each=850),
                               rep(seq(0.01,0.99,by=0.01),each=850),
                               rep(seq(0.01,0.99,by=0.01),each=850),
                               rep(seq(0.01,0.99,by=0.01),each=850),
                               rep(seq(0.01,0.99,by=0.01),each=850),
                               rep(seq(0.01,0.99,by=0.01),each=850),
                               rep(seq(0.01,0.99,by=0.01),each=850),
                               rep(seq(0.01,0.99,by=0.01),each=850),
                               rep(seq(0.01,0.99,by=0.01),each=850),
                               rep(seq(0.01,0.99,by=0.01),each=850),
                               rep(seq(0.01,0.99,by=0.01),each=850),
                               rep(seq(0.01,0.99,by=0.01),each=850)),
                     grazlvl=c(rep(0.1,4*99*100*850), #don't need to use this
                               rep(0.3,4*99*100*850),
                               rep(0.5,4*99*100*850)), 
                     level=c(rep(0.05,99*100*850), #this is our z parameter
                             rep(0.1,99*100*850),
                             rep(0.25,99*100*850),
                             rep(0.5,99*100*850)),
                     EquilibriumID=seq(1,850,by=1), Size = 0, numNA = -1)
#change to numerics as opposed to factors
basins$Grazing <- as.numeric(as.character(basins$Grazing))
basins$RecruitValue <- as.numeric(as.character(basins$RecruitValue))
basins$grazlvl <- as.numeric(as.character(basins$grazlvl))
basins$level <- as.numeric(as.character(basins$level))

print(paste("The first row of basins = ", basins[1,]))

ntrajectory <- dim(grid)[1]
times <- seq(0,2000, by = 0.1)
npoints <- length(times)
initM <- grid$Minit
initC <- grid$Cinit
initT <- grid$Tinit

#can't see the full range of trajectories without starting the values in the other patch from different starting points (can't just set M1=M2, C1=C2 re: starting points)
initM2 <- gridtwo$Minit
initC2 <- gridtwo$Cinit
initT2 <- gridtwo$Tinit

#initializing the dataframes beforehand
mumbytrajectories <- data.frame(Run=rep(1:ntrajectory, each = npoints), 
                                M1 = NA, C1 = NA, T1 = NA, M2 = NA, C2 = NA, 
                                T2 = NA, TimeStep = rep(1:npoints))
basinofattractionID <- data.frame(InitCond=rep(1:ntrajectory), 
                                  Equilibrium = NA, 
                                  initM1 = initM[1:ntrajectory], 
                                  initC1 = initC[1:ntrajectory], 
                                  initT1 = initT[1:ntrajectory], 
                                  initM2 = initM2[1:ntrajectory], 
                                  initC2 = initC2[1:ntrajectory], 
                                  initT2 = initT2[1:ntrajectory])

print(paste("The first row of mumbytrajectories = ", mumbytrajectories[1,]))
print(paste("The first row of basinofattractionID = ", basinofattractionID[1,]))

#initializing limits for whether a trajectory made it to an attractor or not
radius <- 0.005 #needs to be within a 0.005 radius from the equi point (i.e. within the same grid point? i think?) in all components?
times <- seq(0,2000, by = 0.1) #changed from 0,100 by = 0.1
finaltime <- floor(length(times)*0.1) #needs to spend last tenth of the total time within a 0.005 radius of the stable_node
#should all end up by some stable point, if ran the simulation for long enough

#[PYTHON CODE HANDLES FROM HERE TO]
CalcTrajectories <- function(i,parameters,recruitvalue, g_val, glvl, lvl, 
                             ntrajectory,times,mumbytrajectories,initM,initC,
                             initT,initM2,initC2,initT2,
                             MumbyOpen_Elmhirst_2PatchExt){
  for(i in 1:ntrajectory){
    #Elmhirst parameter model 
    parameters <- c(a <- 0.1, d <- 0.24, g <- g_val, r <- 0.55, 
                    gamma <- 0.77, p_c <- recruitvalue, q_c <- recruitvalue, 
                    p_m <- round(recruitvalue*lvl,4), 
                    q_m <- round(recruitvalue*lvl,4), glvl <- glvl)
    #giving M1 and M2, C1 and C2, T1 and T2 starting conditions
    state <- c(M1 = initM[i], C1 = initC[i], Tu1 = initT[i], M2 = initM2[i], 
               C2 = initC2[i], Tu2 = initT2[i])
    print("In trajectory calculation function")
    out <- lsode(y = state, times = times, func = MumbyOpen_Elmhirst_2PatchExt, 
                 parms = parameters)	
    print(paste("first row of out = ", out[1,]))
    mumbytrajectories[mumbytrajectories$Run == i,"M1"] <- out[,2]
    print(paste("M1 worked and first element is",
                mumbytrajectories[mumbytrajectories$Run == i,"M1"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"C1"] <- out[,3]
    print(paste("C1 worked and first element is",
                mumbytrajectories[mumbytrajectories$Run == i,"C1"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"T1"] <- out[,4]
    print(paste("T1 worked and first element is",
                mumbytrajectories[mumbytrajectories$Run == i,"T1"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"M2"] <- out[,5]
    print(paste("M2 worked and first element is",
                mumbytrajectories[mumbytrajectories$Run == i,"M2"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"C2"] <- out[,6]
    print(paste("C2 worked and first element is",
                mumbytrajectories[mumbytrajectories$Run == i,"C2"][1]))
    mumbytrajectories[mumbytrajectories$Run == i,"T2"] <- out[,7]
    print(paste("T2 worked and first element is",
                mumbytrajectories[mumbytrajectories$Run == i,"T2"][1]))
    print(paste("First row of mumbytrajectories post-indexing",
                mumbytrajectories[1,]))
  }
  return(mumbytrajectories)
}
#[HERE]

BOA <- function(glvl, lvl, recruitvalue,g_val,twopatch_ext_IDs,
                basinofattractionID,basins,ntrajectory,radius,times,finaltime){
  print("In BOA, glvl = ", glvl, "lvl =", lvl)
  #STEP 4: colour code each grid point as determined by which basin of attraction it's in
  #number of stable equi at that recruitment and grazing value combo
  numequi <- dim(twopatch_ext_IDs[twopatch_ext_IDs$p_c == recruitvalue & 
                                    twopatch_ext_IDs$p_m == round(recruitvalue*lvl,4) & 
                                    twopatch_ext_IDs$q_c == recruitvalue & 
                                    twopatch_ext_IDs$q_m == round(recruitvalue*lvl,4) & 
                                    twopatch_ext_IDs$g == g_val & twopatch_ext_IDs$g2 == glvl & 
                                    twopatch_ext_IDs$stability == "stable_node",])[1]
  #coordinates of the stable equi at that recruitment and grazing value combo
  M1equi <- twopatch_ext_IDs$M1[twopatch_ext_IDs$p_c == recruitvalue &  
                                  twopatch_ext_IDs$p_m == round(recruitvalue*lvl,4) &  
                                  twopatch_ext_IDs$q_c == recruitvalue &  
                                  twopatch_ext_IDs$q_m == round(recruitvalue*lvl,4) &  
                                  twopatch_ext_IDs$g == g_val & twopatch_ext_IDs$g2 == glvl &  
                                  twopatch_ext_IDs$stability == "stable_node"]
  print(paste("M1equi worked and is",M1equi))
  M2equi <- twopatch_ext_IDs$M2[twopatch_ext_IDs$p_c == recruitvalue &  
                                  twopatch_ext_IDs$p_m == round(recruitvalue*lvl,4) & 
                                  twopatch_ext_IDs$q_c == recruitvalue & 
                                  twopatch_ext_IDs$q_m == round(recruitvalue*lvl,4) & 
                                  twopatch_ext_IDs$g == g_val & twopatch_ext_IDs$g2 == glvl &  
                                  twopatch_ext_IDs$stability == "stable_node"]
  print(paste("M2equi worked and is",M2equi))
  C1equi <- twopatch_ext_IDs$C1[twopatch_ext_IDs$p_c == recruitvalue &  
                                  twopatch_ext_IDs$p_m == round(recruitvalue*lvl,4) &  
                                  twopatch_ext_IDs$q_c == recruitvalue &  
                                  twopatch_ext_IDs$q_m == round(recruitvalue*lvl,4) & 
                                  twopatch_ext_IDs$g == g_val & twopatch_ext_IDs$g2 == glvl &  
                                  twopatch_ext_IDs$stability == "stable_node"]
  print(paste("C1equi worked and is",C1equi))
  C2equi <- twopatch_ext_IDs$C2[twopatch_ext_IDs$p_c == recruitvalue &  
                                  twopatch_ext_IDs$p_m == round(recruitvalue*lvl,4) &  
                                  twopatch_ext_IDs$q_c == recruitvalue &  
                                  twopatch_ext_IDs$q_m == round(recruitvalue*lvl,4) & 
                                  twopatch_ext_IDs$g == g_val & twopatch_ext_IDs$g2 == glvl &  
                                  twopatch_ext_IDs$stability == "stable_node"]
  print(paste("C2equi worked and is",C2equi))
  
  for(n in 1:ntrajectory){
    for(m in 1:numequi){
      print(paste("n = ",n,"m = ", m))
      #if stay within that radius for the final 10th of the time, initial conditions + run # assigned that colour + equi number
      if((((M1equi[m] - radius) < mumbytrajectories$M1[mumbytrajectories$Run == n & 
                                                       mumbytrajectories$TimeStep > (length(times) - finaltime)]) &&
          ((M1equi[m] + radius) > mumbytrajectories$M1[mumbytrajectories$Run == n & 
                                                       mumbytrajectories$TimeStep > (length(times) - finaltime)])) &
         (((C1equi[m] - radius) < mumbytrajectories$C1[mumbytrajectories$Run == n & 
                                                       mumbytrajectories$TimeStep > (length(times) - finaltime)]) &&
          ((C1equi[m] + radius) > mumbytrajectories$C1[mumbytrajectories$Run == n & 
                                                       mumbytrajectories$TimeStep > (length(times) - finaltime)])) &
         (((M2equi[m] - radius) < mumbytrajectories$M2[mumbytrajectories$Run == n &
                                                       mumbytrajectories$TimeStep > (length(times) - finaltime)]) &&
          ((M2equi[m] + radius) > mumbytrajectories$M2[mumbytrajectories$Run == n &
                                                       mumbytrajectories$TimeStep > (length(times) - finaltime)])) &
         (((C2equi[m] - radius) < mumbytrajectories$C2[mumbytrajectories$Run == n & 
                                                       mumbytrajectories$TimeStep > (length(times) - finaltime)]) &&
          ((C2equi[m] + radius) > mumbytrajectories$C2[mumbytrajectories$Run == n &
                                                       mumbytrajectories$TimeStep > (length(times) - finaltime)]))){
        basinofattractionID$Equilibrium[basinofattractionID$InitCond ==  n] <- 	
          twopatch_ext_IDs$ID[twopatch_ext_IDs$p_c == recruitvalue & twopatch_ext_IDs$p_m == round(recruitvalue*lvl,4) &
                                twopatch_ext_IDs$q_c == recruitvalue & 
                                twopatch_ext_IDs$q_m == round(recruitvalue*lvl,4) & 
                                twopatch_ext_IDs$g == g_val & twopatch_ext_IDs$g2 == glvl &
                                twopatch_ext_IDs$stability == "stable_node" & twopatch_ext_IDs$M1 == M1equi[m] &
                                twopatch_ext_IDs$M2 == M2equi[m] & twopatch_ext_IDs$C1 == C1equi[m] &
                                twopatch_ext_IDs$C2 == C2equi[m]]
        basins$Size[basins$RecruitValue == recruitvalue & basins$grazlvl == glvl & basins$Grazing == g_val & #each of size is coded as zero before it goes into this loop - and so every trajectory from every initial condition that falls within that basin of attraction adds 1 
                      basins$EquilibriumID == twopatch_ext_IDs$ID[twopatch_ext_IDs$p_c == recruitvalue & 
                                                                    twopatch_ext_IDs$p_m == round(recruitvalue*lvl,4) & 
                                                                    twopatch_ext_IDs$q_c == recruitvalue &
                                                                    twopatch_ext_IDs$q_m == round(recruitvalue*lvl,4) &
                                                                    twopatch_ext_IDs$g == g_val & 
                                                                    twopatch_ext_IDs$g2 == glvl & 
                                                                    twopatch_ext_IDs$stability == "stable_node" & 
                                                                    twopatch_ext_IDs$M1 == M1equi[m] & 
                                                                    twopatch_ext_IDs$M2 == M2equi[m] & 
                                                                    twopatch_ext_IDs$C1 == C1equi[m] & 
                                                                    twopatch_ext_IDs$C2 == C2equi[m]]] <- 1 + basins$Size[basins$RecruitValue == recruitvalue & 
                                                                                                                            basins$grazlvl == glvl & 
                                                                                                                            basins$Grazing == g_val & 
                                                                                                                            basins$EquilibriumID == twopatch_ext_IDs$ID[twopatch_ext_IDs$p_c == recruitvalue &
                                                                                                                                                                          twopatch_ext_IDs$p_m == round(recruitvalue*lvl,4) & 
                                                                                                                                                                          twopatch_ext_IDs$q_c == recruitvalue & 
                                                                                                                                                                          twopatch_ext_IDs$q_m == round(recruitvalue*lvl,4) & 
                                                                                                                                                                          twopatch_ext_IDs$g == g_val & 
                                                                                                                                                                          twopatch_ext_IDs$g2 == glvl & 
                                                                                                                                                                          twopatch_ext_IDs$stability == "stable_node" &
                                                                                                                                                                          twopatch_ext_IDs$M1 == M1equi[m] &
                                                                                                                                                                          twopatch_ext_IDs$M2 == M2equi[m] &
                                                                                                                                                                          twopatch_ext_IDs$C1 == C1equi[m] & 
                                                                                                                                                                          twopatch_ext_IDs$C2 == C2equi[m]]]}
    }}
  output <- list("basinofattractionID" <- basinofattractionID, "basins" <- basins)
  return(output)
}


#print(paste("recruitvalue =", recruitvalue, "g_value =", g_val, lvl*100, "percent of coral"))

#zero-ing out the old dataframes
#mumbytrajectories$M1 <- NA
#mumbytrajectories$C1 <- NA
#mumbytrajectories$T1 <- NA
#mumbytrajectories$M2 <- NA
#mumbytrajectories$C2 <- NA
#mumbytrajectories$T2 <- NA
#basinofattractionID$Equilibrium <- NA

parameters <- c(a <- 0.1, d <- 0.24, g <- g_val, r <- 0.55, gamma <- 0.77, 
                p_c <- recruitvalue, q_c <- round(recruitvalue*lvl,4),
                p_m <- recruitvalue, q_m <- round(recruitvalue*lvl,4), 
                glvl <- glvl)

#determining the trajectories
i=1
print("About to start calculating trajectories")
mumbytraj <- CalcTrajectories(i,parameters,recruitvalue, g_val, glvl, 
                              lvl, ntrajectory,times,mumbytrajectories,
                              initM,initC,initT,initM2,initC2,initT2,
                              MumbyOpen_Elmhirst_2PatchExt)
mumbytrajectories <- mumbytraj
print(paste("The first row of mumbytraj = ", mumbytrajectories[1,]))
save(mumbytrajectories, file = paste0("/home/agreiner/scratch/BoA_calculations/scenariofour/mumbytraj_heteroggrazlong/mumbytrajectories_recr",
                                      recruitvalue,"g",g_val,"_glvl",glvl,"_lvl",lvl*100,"_20000.RData"))

#code each initial condition as determined by which basin of attraction it's in
output <- BOA(glvl, lvl, recruitvalue,g_val,twopatch_ext_heterograz_ID_full,basinofattractionID,basins,ntrajectory,radius,times,finaltime)
basinofattractionID <- output[[1]]
basins <- output[[2]]
basins$Grazing <- as.numeric(as.character(basins$Grazing))
basins$RecruitValue <- as.numeric(as.character(basins$RecruitValue))
basins$grazlvl <- as.numeric(as.character(basins$grazlvl))
basins$level <- as.numeric(as.character(basins$level))
save(basinofattractionID, file = paste0("/home/agreiner/scratch/BoA_calculations/scenariofour/BoAID_heteroggrazlong/basinofattractionID_recr",
                                        recruitvalue,"g",g_val,"_glvl",glvl,"percentofcoral_20000.RData"))
save(basins, file = paste0("/home/agreiner/scratch/BoA_calculations/scenariofour/bsns_heteroggrazlong/basins_recr",recruitvalue,"g",g_val,"_glvl",
                           glvl,"_lvl",lvl*100,"_20000.RData"))
basinsabr <- basins[basins$RecruitValue == recruitvalue & basins$grazlvl == glvl & basins$Grazing == g_val & basins$level = lvl,]
save(basinsabr, file = paste0("/home/agreiner/scratch/BoA_calculations/scenariofour/bsns_heteroggrazlong_abr/basins_recr",recruitvalue,"g",g_val,
                              "_glvl",glvl,"_lvl",lvl*100,"_20000.RData"))


#save whether a set of parameters has any NAs and how many it has
basins$numNA[basins$RecruitValue == recruitvalue & basins$grazlvl == glvl & 
               basins$Grazing == g_val & 
               basins$level == lvl] <-length(which(is.na(basinofattractionID$Equilibrium)))
save(basins, file = paste0("/home/agreiner/scratch/BoA_calculations/scenariofour/bsns_heteroggrazlong/basins_recr",
                           recruitvalue,"g",g_val,"_glvl",glvl,"_lvl",lvl*100,"_20000.RData"))
basinsabr <- basins[basins$RecruitValue == recruitvalue &
                      basins$grazlvl == glvl &
                      basins$Grazing == g_val &
                      basins$level == lvl,]
save(basinsabr, file = paste0("/home/agreiner/scratch/BoA_calculations/scenariofour/bsns_heteroggrazlong_abr/basins_recr",recruitvalue,"g",g_val,"_glvl",glvl,"_lvl",lvl*100,"_20000.RData"))

#iterator[j,4] <- length(which(is.na(basinofattractionID$Equilibrium)))
#save(iterator[j,4],file=paste0("/home/agreiner/scratch/BoA_calculations/scenariofour/NAchckr/iterator_recr",recruitvalue,"g",g_val,lvl*100,"percentofcoral_5850.RData")) 

#i think it's easier to just know where to look and then see how bad the problem is and decide whether to fix it and how after the fact (just run those particular initial conditions for longer? run the whole set of initial conditions for longer for that parameter combination? etc)




