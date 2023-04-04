##' File Description
##' AUTHOR: Cole B. Brookson
##' DATE OF CREATION: 2023-03-19
#'
#' This file contains manual testing for the equilibria matching process
#'
#'All functions are documented using the roxygen2 framework and the docstring
#'library
#'

unord_parms <- readr::read_csv(
  here::here(
    "./data/parameter-data/all-parameter-vals-equilibria-unordered.csv"))

# we need a column that indicates when to change to a new parameter combination 
new_param_combo <- function(df) {
  #' Find when to change to a new parameter combination 
  #' 
  #' @description Since there are multiple equilibria for each parameter combo,
  #' we need to have a rowwise measure of which equilibria belong to which param
  #' combos. This function goes through and assigns equilibria belonging to the 
  #' same parameter combo the same combo value
  #' 
  #' @param df dataframe. The datadrame at hand of unordered parameter
  #' combinatons
  #' @return The same dataframe with with a column indicating parameter combos
  
  df$param_combo <- NA # initialize column
  param_combo <- 1 # start at 1
  df$param_combo[1] <- param_combo # first row is first combo 
  
  # loop through the dataframe and assing a param combo to each unique combo
  # note that some combos can have >1 (4 at this writing is the max) equil. so 
  # ecah of those needs to be classified with the same param combo 
  
  for(i in 2:nrow(df)) {
    # if the equilibrium is the first one, add one to the param_combo value and 
    # assign to that equil 
    if(df$Equilibrium[i] == 1) {
      
      param_combo <- param_combo + 1 # get a new combo 
      df$param_combo[i] <- param_combo
    
    # if it's not the first equilibria, then it's one of the equilibria of the
    # same param combo, so keep the same param combo 
    } else { 
      df$param_combo[i] <- param_combo
    }
  }
  
  return(df)
}

unord_parms <- new_param_combo(df = unord_parms)

check_stable_nodes <-function(df) {
  #' Make sure each parameter combinatoin has a stable node 
  #' 
  #' @description With multiple equilibria at some of the parameter combinations
  #' we need to know if there is minimum one stable node at each combination
  #' 
  #' @param unord_parms dataframe. The datadrame at hand of unordered parameter
  #' combinatons
  #' @return TRUE/FALSE
  
  df <- df %>% 
    dplyr::group_by(param_combo) %>% 
    dplyr::summarize(stable = ifelse("stable_node" %in% stability, 1, 0))
  
  return(any(df$stable == 0))
}

check_stable_nodes(unord_parms)

keep_stable_nodes <- function(df) {
  #' Make sure each parameter combinatoin has a stable node 
  #' 
  #' @description With multiple equilibria at some of the parameter combinations
  #' we need to know if there is minimum one stable node at each combination
  #' 
  #' @param unord_parms dataframe. The datadrame at hand of unordered parameter
  #' combinatons
  #' @return TRUE/FALSE
  
  # we know there are more than one stable node per parameter combo in some of 
  # combos, so we need to assing them ID's
  
  # assign all the non-stable nodes, a 0 ID
  df <- df %>% 
    dplyr::mutate(
      id = ifelse(
        stability == "stable_node", as.numeric(NA), 0
      )
    )
  
  # determine the starting values 
  
  # start at 1
  df[which(df$param_combo == 1 & df$stability == "stable_node"), "id"] <- 1 
  num_equi_old <- length(df[which(df$param_combo == 1 & 
                                 df$stability == "stable_node"), "Equilibrium"])
  
  # set up dataframe to store some values in 
  equi_val_store <- data.frame(
    id_num = seq(1, param_combo, by = 1),
    C = NA,
    M = NA
  )
  
  max_id <- max(df[which(df$param_combo == 1 & 
                           df$stability == "stable_node"), "id"])
  
  equi_vals_old <- data.frame(
    equi_num = seq(1, num_equi_old, by = 1),
    C = NA,
    M = NA, 
    norep = 0
  )
  
  equi_vals_old$C <- df[which(df$param_combo == 1 & 
                                 df$stability == "stable_node"), "C"]
  equi_vals_old$M <- df[which(df$param_combo == 1 & 
                                df$stability == "stable_node"), "M"]
  
  # start with id 1
  id_id <- df[which(df$param_combo == 1 & df$stability == "stable_node"), "id"]
  # put that value in the storages df 
  
  
  data$ID <- NA
  data$ID[data$stability == "unstable_node"] <- 0
  data$ID[data$stability == "bifurcation_point"] <- 0
  data$ID[data$stability == "saddle_node"] <- 0
  
  data$ID[data$param_combo == 1 & data$stability == "stable_node"] <- 1 #things get messed up below if this doesn't start at 1
  numequi_old <- length(data$Equilibrium[data$param_combo == 1 & data$stability == "stable_node"]) 
  equival_storage <- data.frame(ID_num=seq(1,param_combo, by=1), C = NA, M = NA, ID = NA, norep = 0)
  MaxID <- max(data$ID[data$param_combo == 1 & data$stability == "stable_node"])
  equivals_old <- data.frame(EquiNum=seq(1,numequi_old, by=1), C = NA, M = NA)
  equivals_old$C <- data$C[data$param_combo == 1 & data$stability == "stable_node"] 
  equivals_old$M <- data$M[data$param_combo == 1 & data$stability == "stable_node"] 
  
  
  #only 1 to start with
  idid <- data$ID[data$param_combo == 1 & data$stability == "stable_node"]
  equival_storage$ID[idid] <-  idid
  equival_storage$C[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- data$C[data$param_combo == 1 & data$stability == "stable_node"]
  equival_storage$M[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- data$M[data$param_combo == 1 & data$stability == "stable_node"]
  
  
  prop_cutoff<- 0.05 #0.02,0.03 seemed a bit too picky
  cutoff_add <- 0.01
  
  warning <- rep(NA,paramcomb)
  na.catcher <- rep("no",paramcomb)
  bifurc <- rep(0, paramcomb)
  
   
    
    
    
    
    
    
    
  
  
  
  
  
  
  
}





#want to add IDs to the stable nodes
data$ID <- NA

#column indicating when change to a new parameter combination
data$paramcombo <- NA
paramcomb <- 1
data$paramcombo[1] <- 1
for(i in 2:dim(data)[1]){
  if(data$Equilibrium[i] == 1){
    paramcomb <- paramcomb + 1
    data$paramcombo[i] <- paramcomb	
  }else{data$paramcombo[i] <- paramcomb} 
}
#paramcomb got up to 20604 (4 dispersal values * 51 g values * 101 a values)

#does every paramcombo have a stable node?
stable <- rep(NA, paramcomb)
for(i in 1:paramcomb){
  stable[i] <- "no"
  numequi <- tail(data$Equilibrium[data$paramcombo == i], 1)
  for(j in 1:numequi){
    if(data$stability[data$paramcombo == i][j] == "stable_node"){
      stable[i] <- "yes"}
  }
}
length(stable[stable == "yes"]) #20604 - good

#note: the nice thing about just doing stable nodes is that they can't be next to each other 
#so if have to define the others later, it could be good to do each separately  

#reset ID column 
data$ID <- NA
data$ID[data$stability == "unstable_node"] <- 0
data$ID[data$stability == "bifurcation_point"] <- 0
data$ID[data$stability == "saddle_node"] <- 0
#because only want to define the stable nodes anyways (at least for now)

onlystabnodes <- data[data$stability == "stable_node",]
dim(onlystabnodes) #26699 x 15...we know that there is one stable node per param_combo at least so 6095 have more than 1

#i think that there is a much easier way to do this than the code that is being used below but im just going to do it this way for now and see what happens


#determine starting values
#data$ID[data$paramcombo == 1 & data$stability == "stable_node"] <- data$Equilibrium[data$paramcombo == 1 & data$stability == "stable_node"]
data$ID[data$paramcombo == 1 & data$stability == "stable_node"] <- 1 #things get messed up below if this doesn't start at 1
numequi_old <- length(data$Equilibrium[data$paramcombo == 1 & data$stability == "stable_node"]) 
equival_storage <- data.frame(ID_num=seq(1,paramcomb, by=1), C = NA, M = NA, ID = NA, norep = 0)
MaxID <- max(data$ID[data$paramcombo == 1 & data$stability == "stable_node"])
equivals_old <- data.frame(EquiNum=seq(1,numequi_old, by=1), C = NA, M = NA)
equivals_old$C <- data$C[data$paramcombo == 1 & data$stability == "stable_node"] 
equivals_old$M <- data$M[data$paramcombo == 1 & data$stability == "stable_node"] 


#only 1 to start with
idid <- data$ID[data$paramcombo == 1 & data$stability == "stable_node"]
equival_storage$ID[idid] <-  idid
equival_storage$C[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- data$C[data$paramcombo == 1 & data$stability == "stable_node"]
equival_storage$M[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- data$M[data$paramcombo == 1 & data$stability == "stable_node"]


prop_cutoff<- 0.05 #0.02,0.03 seemed a bit too picky
cutoff_add <- 0.01

warning <- rep(NA,paramcomb)
na.catcher <- rep("no",paramcomb)
bifurc <- rep(0, paramcomb)

#match the equilibria value with an ID
#check whether at bifurcation point or not (change in number of equi, OTHER THINGS?)
#check which of the next set of equilibria correspond to which of the first set, assign them IDs in line w said identification

#ran into some issue at i = 18 (fixed by assigning the first ID to 1), issue with 3123 bc noo stable node so need to skip
for(i in 2:20604){ #2:paramcomb
  if(i %% 10 == 0) {
    print(paste("Parameter combination ", i))
  }
  numequi <- length(data$Equilibrium[data$paramcombo == i & data$stability == "stable_node"]) #want the number of stable equi
  equivals <- data.frame(EquiNum=seq(1,numequi, by=1), C = NA, M = NA, matched = 0) #changed from matched <- 0
  equivals$C <- data$C[data$paramcombo == i & data$stability == "stable_node"] 
  equivals$M <- data$M[data$paramcombo == i & data$stability == "stable_node"] 
  if(numequi == numequi_old){ #match the IDs
    for(j in 1:numequi){ 
      Cone <- equivals$C[j]
      Mone <- equivals$M[j]
      for(k in 1:numequi){
        if(((Cone + (prop_cutoff*Cone + cutoff_add) > equivals_old$C[k]) & (Cone - (prop_cutoff*Cone + cutoff_add) < equivals_old$C[k])) & ((Mone + (prop_cutoff*Mone + cutoff_add) > equivals_old$M[k]) & (Mone - (prop_cutoff*Mone + cutoff_add) < equivals_old$M[k])) & equivals$matched[j] < 1){
          data$ID[data$paramcombo == i & data$stability == "stable_node"][j] <- data$ID[data$paramcombo == (i-1) & data$stability == "stable_node"][k]
          
          idid <- data$ID[data$paramcombo == i & data$stability == "stable_node"][j]
          equival_storage$ID[idid] <-  idid
          equival_storage$C[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- data$C[data$paramcombo == i & data$stability == "stable_node"][j]
          equival_storage$M[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- data$M[data$paramcombo == i & data$stability == "stable_node"][j]
          
          equivals$matched[j] <- 1 #can use this to try and make sure things aren't matched twice...
        }
      }
    }
    
    ifelse(sum(data$ID[data$paramcombo == i & data$stability == "stable_node"] == data$ID[data$paramcombo == (i-1) & data$stability == "stable_node"]) < numequi,warning[i]<-"re-ordered", warning[i]<-":)")
    #issue: ^ after hit a 'no matches' an NA is introduced and doesn't go away until there's a bifurcation
    
    #same number of equi, not all matched
    if(sum(equivals$matched) < numequi){
      numID <- length(na.omit(equival_storage$ID))
      
      for(j in 1:numequi){
        if(equivals$matched[j] < 1){ #not currently matched
          Cone <- equivals$C[j]
          Mone <- equivals$M[j]
          for(k in 1:numID){ #shouldn't get repeat values from this bc same criteria as above for the already set IDs
            ConeID <- equival_storage$C[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
            MoneID <- equival_storage$M[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
            if(((Cone + (prop_cutoff*Cone + cutoff_add) > ConeID) & (Cone - (prop_cutoff*Cone + cutoff_add) < ConeID)) & ((Mone + (prop_cutoff*Mone + cutoff_add) > MoneID) & (Mone - (prop_cutoff*Mone + cutoff_add) < MoneID)) & equivals$matched[j] < 1 & equival_storage$norep[equival_storage$ID == k & (is.na(equival_storage$ID)  == F)] < 1){
              
              data$ID[data$paramcombo == i & data$stability == "stable_node"][j] <- k
              #keep thinking about whether this is a problem to re-define thesee within the loop - made it so can't redo ID == k
              equival_storage$ID[k] <-  k
              equival_storage$C[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- data$C[data$paramcombo == i & data$stability == "stable_node"][j]
              equival_storage$M[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- data$M[data$paramcombo == i & data$stability == "stable_node"][j]
              equival_storage$norep[equival_storage$ID == k  & (is.na(equival_storage$ID)  == F)] <- 1 
              equivals$matched[j] <- 1
              
            }
          }
        }
      }
    }
    
    if(sum(equivals$matched) < numequi){	#still not matched?
      warning[i] <- "new IDs assigned"
      
      for(m in 1:numequi){
        if(equivals$matched[m] == 0){
          MaxID <- MaxID + 1
          data$ID[data$paramcombo == i & data$stability == "stable_node"][m] <- MaxID
          
          equival_storage$ID[MaxID] <- MaxID
          equival_storage$C[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- data$C[data$paramcombo == i & data$stability == "stable_node"][m]
          equival_storage$M[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- data$M[data$paramcombo == i & data$stability == "stable_node"][m]
        }	
      }
      
    } 
    if(sum(is.na(data$ID[data$paramcombo == i & data$stability == "stable_node"]))>0){
      na.catcher[i] <- "yes"
      warning[i] <- "NAs detected"} 
    if(length(data$ID[data$paramcombo == i & data$stability == "stable_node"]) > length(unique(data$ID[data$paramcombo == i & data$stability == "stable_node"])) & na.catcher[i] == "no"){warning[i] <- "repeat values"}		
    
    
  }
  if(numequi != numequi_old){ #bifurcation, try to continue assigning old IDs...assign new ones otherwise
    warning[i] <- "bifurcation"
    
    for(j in 1:numequi){ 
      Cone <- equivals$C[j]
      Mone <- equivals$M[j]
      for(k in 1:numequi_old){ 
        if(((Cone + (prop_cutoff*Cone + cutoff_add) > equivals_old$C[k]) & (Cone - (prop_cutoff*Cone + cutoff_add) < equivals_old$C[k])) & ((Mone + (prop_cutoff*Mone + cutoff_add) > equivals_old$M[k]) & (Mone - (prop_cutoff*Mone + cutoff_add) < equivals_old$M[k])) & equivals$matched[j] < 1){
          data$ID[data$paramcombo == i & data$stability == "stable_node"][j] <- data$ID[data$paramcombo == (i-1) & data$stability == "stable_node"][k]
          
          idid <- data$ID[data$paramcombo == i & data$stability == "stable_node"][j]
          equival_storage$ID[idid] <-  idid
          equival_storage$C[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- data$C[data$paramcombo == i & data$stability == "stable_node"][j]
          equival_storage$M[equival_storage$ID ==  idid & (is.na(equival_storage$ID)  == F)] <- data$M[data$paramcombo == i & data$stability == "stable_node"][j]
          
          equivals$matched[j] <- 1 #can use this to try and make sure things aren't matched twice...
        }
      }
    }
    if(sum(equivals$matched) < numequi){
      #found in old ID?
      numID <- length(na.omit(equival_storage$ID))
      
      for(j in 1:numequi){
        if(equivals$matched[j] < 1){ #not currently matched
          Cone <- equivals$C[j]
          Mone <- equivals$M[j]
          for(k in 1:numID){ #shouldn't get repeat values from this bc same criteria as above for the already set IDs
            ConeID <- equival_storage$C[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
            MoneID <- equival_storage$M[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)]
            #issue here at k = 1 when i = 18, not sure why
            if(((Cone + (prop_cutoff*Cone + cutoff_add) > ConeID) & (Cone - (prop_cutoff*Cone + cutoff_add) < ConeID)) & ((Mone + (prop_cutoff*Mone + cutoff_add) > MoneID) & (Mone - (prop_cutoff*Mone + cutoff_add) < MoneID)) & equivals$matched[j] < 1 & equival_storage$norep[equival_storage$ID == k & (is.na(equival_storage$ID)  == F)] < 1){
              
              data$ID[data$paramcombo == i & data$stability == "stable_node"][j] <- k
              #keep thinking about whether this is a problem to re-define thesee within the loop - made it so can't redo ID == k
              equival_storage$ID[k] <-  k
              equival_storage$C[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- data$C[data$paramcombo == i & data$stability == "stable_node"][j]
              equival_storage$M[equival_storage$ID ==  k & (is.na(equival_storage$ID)  == F)] <- data$M[data$paramcombo == i & data$stability == "stable_node"][j]
              equival_storage$norep[equival_storage$ID == k & (is.na(equival_storage$ID)  == F)] <- 1 
              equivals$matched[j] <- 1
              
            }
          }
        }
      } #ok so it goes through all the unmatched ones and tries to match them with a previous ID, if some are still unmatched...
    }	
    
    #still  unmatched? need to assign new IDs
    if(sum(equivals$matched) < numequi){	
      warning[i] <- "new IDs assigned_bifurc"
      
      for(m in 1:numequi){
        if(equivals$matched[m] == 0){
          MaxID <- MaxID + 1
          data$ID[data$paramcombo == i & data$stability == "stable_node"][m] <- MaxID
          
          equival_storage$ID[MaxID] <- MaxID
          equival_storage$C[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- data$C[data$paramcombo == i & data$stability == "stable_node"][m]
          equival_storage$M[equival_storage$ID == MaxID & (is.na(equival_storage$ID)  == F)] <- data$M[data$paramcombo == i & data$stability == "stable_node"][m]
        }	
      }
      
    }
    
    if(sum(is.na(data$ID[data$paramcombo == i & data$stability == "stable_node"]))>0){
      na.catcher[i] <- "yes"
      warning[i] <- "bifurc_NA"} 
    if(length(data$ID[data$paramcombo == i & data$stability == "stable_node"]) > length(unique(data$ID[data$paramcombo == i & data$stability == "stable_node"])) & na.catcher[i] == "no"){warning[i] <- "repeat values_bifurc"}		
    
  }
  
  equivals_old <- equivals
  numequi_old <- numequi
  equival_storage$norep  <- 0 #because just don't want reps w/n 1 paramcomb
}
