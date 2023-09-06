
#' MumbyOpen_Restoration
#' 
#' Function that simulates the three state variables
MumbyOpen_Restoration <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    dM1 <- a*M1*C1 - (g_val*M1)/(M1+Tu1) + y*M1*Tu1 
    dC1 <- r*Tu1*C1 - d*C1 - a*M1*C1 + z*r*Tu1
    dT1 <- (g_val*M1)/(M1+Tu1) - y*M1*Tu1 - r*Tu1*C1 + d*C1 - z*r*Tu1
    list(c(dM1,dC1,dT1))
  })
}

#' CalcTrajectories
#' 
#' Function that goes through each of the trajectories and finds the values 
#' that we need or 
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

#' BOA
#' 
#' Function that's main purpose is to perform a long series of checks to see if 
#' the trajectory leaves or stays in the basin of attraction
BOA <- function(mc_comp, recruitvalue, g_val, data_IDs, basinofattractionID,
                basins, ntrajectory, mumbytrajectories,
                radius, times, finaltime){
  
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