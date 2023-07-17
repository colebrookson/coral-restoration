# load packages and the data we need ===========================================
library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)
library(qs)


# BEGIN NOTE ###########
# The goal here is to add ID's to stable nodes in our information that's coming
# from matlab
# END NOTE #############

# checking of the nodes ========================================================

#does every paramcombo have a stable node?
check <- data %>% 
  #dplyr::mutate(paramcombo_fac = as.factor(paramcombo)) %>% 
  dplyr::group_by(paramcombo) %>% 
  dplyr::summarize(stable = any(stability == "stable_node"))

keep_combos <- check %>% 
  dplyr::filter(stable == "TRUE")

# remove the combos that don't have a stable node
data <- data[which(data$paramcombo %in% keep_combos$paramcombo), ] 

# note: the nice thing about just doing stable nodes is that they can't be next 
# to each other so if have to define the others later, it could be good to do 
# each separately  

# now let's make a new set of parameter combinations with only the ones
# that did in fact have a stable node
data <- data %>% 
  dplyr::select(-c(paramcombo, g_fac, a_fac, z_fac)) 
data <- data %>% 
  dplyr::group_by(g, a, z) %>% 
  dplyr::mutate(
    paramcombo = dplyr::cur_group_id()
  )

# reset ID column because only want to define the stable nodes anyways 
# (at least for now)
data$ID <- NA
data$ID[data$stability == "unstable_node"] <- 0
data$ID[data$stability == "bifurcation_point"] <- 0
data$ID[data$stability == "saddle_node"] <- 0

onlystabnodes <- data[data$stability == "stable_node",]
dim(onlystabnodes) 
#26699 x 15...we know that there is one stable node per param_combo at 
#least so 6095 have more than 1

# determine starting values ====================================================

#things get messed up below if this doesn't start at 1
data$ID[data$paramcombo == 1 & data$stability == "stable_node"] <- 1
numequi_old <-length(data$Equilibrium[data$paramcombo == 1 &
                                         data$stability == "stable_node"])
equival_storage <- data.frame(
  ID_num=seq(1,n_distinct(data$paramcombo), by=1),
  C = NA, M = NA, ID = NA, norep = 0
  )
MaxID <- max(data$ID[data$paramcombo == 1 & data$stability == "stable_node"])
equivals_old <- data.frame(EquiNum=seq(1,numequi_old, by=1), C = NA, M = NA)
equivals_old$C <- data$C[data$paramcombo == 1 & data$stability == "stable_node"]
equivals_old$M <- data$M[data$paramcombo == 1 & data$stability == "stable_node"]

# start with just one
#only 1 to start with
idid <- data$ID[data$paramcombo == 1 & data$stability == "stable_node"]
equival_storage$ID[idid] <- idid
equival_storage$C[equival_storage$ID ==  idid &
                    (is.na(equival_storage$ID)  == F)] <-
  data$C[data$paramcombo == 1 & data$stability == "stable_node"]
equival_storage$M[equival_storage$ID ==  idid &
                    (is.na(equival_storage$ID)  == F)] <-
  data$M[data$paramcombo == 1 & data$stability == "stable_node"]

prop_cutoff<- 0.05 #0.02,0.03 seemed a bit too picky
cutoff_add <- 0.01

warning <- rep(NA, max(data$paramcombo))
na.catcher <- rep("no", max(data$paramcombo))
bifurc <- rep(0, max(data$paramcombo))

# match the equilibria value with an ID ========================================

# check whether at bifurcation point or not
# check which of the next set of equilibria correspond to which of the 
# first set, assign them IDs in line w said identification


for(i in 1:max(data$paramcombo)){ #2:paramcomb
  if(i %% 10 == 0) {
    print(paste("Parameter combination ", i))
  }
  
  # get number of stable node equilibria
  numequi <- length(data$Equilibrium[data$paramcombo == i 
                                     #want the number of stable equi
                                     & data$stability == "stable_node"]) 
  
  # data frame of the size numequi and fill it 
  equivals <- data.frame(EquiNum=seq(1,numequi, by=1), C = NA, M = NA, 
                         matched = 0) #changed from matched <- 0
  equivals$C <- data$C[data$paramcombo == i & data$stability == "stable_node"] 
  equivals$M <- data$M[data$paramcombo == i & data$stability == "stable_node"] 
  
  # matching process
  if(numequi == numequi_old){ #match the IDs
    
    for(j in 1:numequi){ # go through each of the equilibria
      Cone <- equivals$C[j]
      Mone <- equivals$M[j]
      
      for(k in 1:numequi){
        # this is figuring out if the current values of the equilibria are 
        # close enough to be considered the same "ID" 
        if(((Cone + (prop_cutoff*Cone + cutoff_add) > equivals_old$C[k]) & 
            (Cone - (prop_cutoff*Cone + cutoff_add) < equivals_old$C[k])) & 
           ((Mone + (prop_cutoff*Mone + cutoff_add) > equivals_old$M[k]) & 
            (Mone - (prop_cutoff*Mone + cutoff_add) < equivals_old$M[k])) & 
           equivals$matched[j] < 1){
          # if true, we assign the id from the last one to the current one
          data$ID[data$paramcombo == i & data$stability == "stable_node"][j] <- 
            data$ID[data$paramcombo == (i-1) & 
                      data$stability == "stable_node"][k]
          # grabbing some info on the curr id to hold in the equival storage
          idid <- data$ID[data$paramcombo == i & 
                            data$stability == "stable_node"][j]
          equival_storage$ID[idid] <-  idid
          equival_storage$C[equival_storage$ID ==  idid & 
                              (is.na(equival_storage$ID)  == F)] <- 
            data$C[data$paramcombo == i & data$stability == "stable_node"][j]
          equival_storage$M[equival_storage$ID ==  idid & 
                              (is.na(equival_storage$ID)  == F)] <- 
            data$M[data$paramcombo == i & data$stability == "stable_node"][j]
          
          #can use this to try and make sure things aren't matched twice...
          equivals$matched[j] <- 1 
        }
      }
    }
    
    ifelse(sum(
      data$ID[data$paramcombo == i & 
                data$stability == "stable_node"] ==
        data$ID[data$paramcombo == (i-1) & data$stability == "stable_node"]) < 
        numequi,warning[i]<-"re-ordered", warning[i]<-":)")
    #issue: ^ after hit a 'no matches' an NA is introduced and doesn't go 
    #away until there's a bifurcation
    
    #same number of equi, not all matched
    if(sum(equivals$matched) < numequi){
      numID <- length(na.omit(equival_storage$ID))
      
      for(j in 1:numequi){
        if(equivals$matched[j] < 1){ #not currently matched
          Cone <- equivals$C[j]
          Mone <- equivals$M[j]
          
          #shouldn't get repeat values from this bc same criteria as above for 
          #the already set IDs
          for(k in 1:numID){ 
            ConeID <- equival_storage$C[equival_storage$ID ==  k & 
                                          (is.na(equival_storage$ID)  == F)]
            MoneID <- equival_storage$M[equival_storage$ID ==  k & 
                                          (is.na(equival_storage$ID)  == F)]
            if(((Cone + (prop_cutoff*Cone + cutoff_add) > ConeID) & 
                (Cone - (prop_cutoff*Cone + cutoff_add) < ConeID)) & 
               ((Mone + (prop_cutoff*Mone + cutoff_add) > MoneID) & 
                (Mone - (prop_cutoff*Mone + cutoff_add) < MoneID)) & 
               equivals$matched[j] < 1 & 
               equival_storage$norep[equival_storage$ID == k & 
                                     (is.na(equival_storage$ID)  == F)] < 1){
              
              data$ID[data$paramcombo == i & 
                        data$stability == "stable_node"][j] <- k
              
              #keep thinking about whether this is a problem to re-define these
              #within the loop - made it so can't redo ID == k
              
              equival_storage$ID[k] <-  k
              equival_storage$C[equival_storage$ID ==  k & 
                                  (is.na(equival_storage$ID)  == F)] <- 
                data$C[data$paramcombo == i & 
                         data$stability == "stable_node"][j]
              equival_storage$M[equival_storage$ID ==  k & 
                                  (is.na(equival_storage$ID)  == F)] <- 
                data$M[data$paramcombo == i & 
                         data$stability == "stable_node"][j]
              equival_storage$norep[equival_storage$ID == k  & 
                                      (is.na(equival_storage$ID)  == F)] <- 1 
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
          data$ID[data$paramcombo == i & 
                    data$stability == "stable_node"][m] <- MaxID
          
          equival_storage$ID[MaxID] <- MaxID
          equival_storage$C[equival_storage$ID == MaxID & 
                              (is.na(equival_storage$ID)  == F)] <- 
            data$C[data$paramcombo == i & data$stability == "stable_node"][m]
          equival_storage$M[equival_storage$ID == MaxID & 
                              (is.na(equival_storage$ID)  == F)] <- 
            data$M[data$paramcombo == i & data$stability == "stable_node"][m]
        }	
      }
      
    } 
    if(sum(is.na(data$ID[data$paramcombo == i & 
                         data$stability == "stable_node"]))>0){
      na.catcher[i] <- "yes"
      warning[i] <- "NAs detected"} 
    if(length(data$ID[data$paramcombo == i & 
                      data$stability == "stable_node"]) > 
       length(unique(data$ID[data$paramcombo == i & data$stability == 
                             "stable_node"])) & 
       na.catcher[i] == "no"){warning[i] <- "repeat values"}		
    
    
  }
  #bifurcation, try to continue assigning old IDs...assign new ones otherwise
  if(numequi != numequi_old){ 
    warning[i] <- "bifurcation"
    
    for(j in 1:numequi){ 
      Cone <- equivals$C[j]
      Mone <- equivals$M[j]
      for(k in 1:numequi_old){ 
        if(((Cone + (prop_cutoff*Cone + cutoff_add) > equivals_old$C[k]) & 
            (Cone - (prop_cutoff*Cone + cutoff_add) < equivals_old$C[k])) & 
           ((Mone + (prop_cutoff*Mone + cutoff_add) > equivals_old$M[k]) & 
            (Mone - (prop_cutoff*Mone + cutoff_add) < equivals_old$M[k])) & 
           equivals$matched[j] < 1){
          data$ID[data$paramcombo == i & data$stability == "stable_node"][j] <- 
            data$ID[data$paramcombo == (i-1) & 
                      data$stability == "stable_node"][k]
          
          idid <- data$ID[data$paramcombo == i & 
                            data$stability == "stable_node"][j]
          equival_storage$ID[idid] <-  idid
          equival_storage$C[equival_storage$ID ==  idid & 
                              (is.na(equival_storage$ID)  == F)] <- 
            data$C[data$paramcombo == i & data$stability == "stable_node"][j]
          equival_storage$M[equival_storage$ID ==  idid & 
                              (is.na(equival_storage$ID)  == F)] <- 
            data$M[data$paramcombo == i & data$stability == "stable_node"][j]
          
          #can use this to try and make sure things aren't matched twice...
          equivals$matched[j] <- 1 
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
          
          #shouldn't get repeat values from this bc same criteria as 
          #above for the already set IDs
          for(k in 1:numID){
            ConeID <- equival_storage$C[equival_storage$ID ==  k & 
                                          (is.na(equival_storage$ID)  == F)]
            MoneID <- equival_storage$M[equival_storage$ID ==  k & 
                                          (is.na(equival_storage$ID)  == F)]
            #issue here at k = 1 when i = 18, not sure why
            if(((Cone + (prop_cutoff*Cone + cutoff_add) > ConeID) & 
                (Cone - (prop_cutoff*Cone + cutoff_add) < ConeID)) & 
               ((Mone + (prop_cutoff*Mone + cutoff_add) > MoneID) & 
                (Mone - (prop_cutoff*Mone + cutoff_add) < MoneID)) & 
               equivals$matched[j] < 1 & 
               equival_storage$norep[equival_storage$ID == k & 
                                     (is.na(equival_storage$ID)  == F)] < 1){
              
              data$ID[data$paramcombo == i & 
                        data$stability == "stable_node"][j] <- k
              
              #keep thinking about whether this is a problem to re-define these
              #within the loop - made it so can't redo ID == k
              
              equival_storage$ID[k] <-  k
              equival_storage$C[equival_storage$ID ==  k & 
                                  (is.na(equival_storage$ID)  == F)] <- 
                data$C[data$paramcombo == i & data$stability == "stable_node"][j]
              equival_storage$M[equival_storage$ID ==  k & 
                                  (is.na(equival_storage$ID)  == F)] <- 
                data$M[data$paramcombo == i & data$stability == "stable_node"][j]
              equival_storage$norep[equival_storage$ID == k & 
                                      (is.na(equival_storage$ID)  == F)] <- 1 
              equivals$matched[j] <- 1
              
            }
          }
        }
      } #ok so it goes through all the unmatched ones and tries to match them 
      #with a previous ID, if some are still unmatched...
    }	
    
    #still  unmatched? need to assign new IDs
    if(sum(equivals$matched) < numequi){	
      warning[i] <- "new IDs assigned_bifurc"
      
      for(m in 1:numequi){
        if(equivals$matched[m] == 0){
          MaxID <- MaxID + 1
          data$ID[data$paramcombo == i & 
                    data$stability == "stable_node"][m] <- MaxID
          
          equival_storage$ID[MaxID] <- MaxID
          equival_storage$C[equival_storage$ID == MaxID & 
                              (is.na(equival_storage$ID)  == F)] <-
            data$C[data$paramcombo == i & data$stability == "stable_node"][m]
          equival_storage$M[equival_storage$ID == MaxID & 
                              (is.na(equival_storage$ID)  == F)] <-
            data$M[data$paramcombo == i & data$stability == "stable_node"][m]
        }	
      }
      
    }
    
    if(sum(is.na(data$ID[data$paramcombo == i & 
                         data$stability == "stable_node"]))>0){
      na.catcher[i] <- "yes"
      warning[i] <- "bifurc_NA"} 
    if(length(data$ID[data$paramcombo == i & 
                      data$stability == "stable_node"]) > 
       length(unique(data$ID[data$paramcombo == i & 
                             data$stability == "stable_node"])) & 
       na.catcher[i] == "no"){warning[i] <- "repeat values_bifurc"}		
    
  }
  
  equivals_old <- equivals
  numequi_old <- numequi
  equival_storage$norep  <- 0 #because just don't want reps w/n 1 paramcomb
  
  if(i %% 1000 == 0) {
    qs::qsave(data, 
              file = here("./data/parameter-data/allparam_data_ordered.qs"))
  }
}

#save(data, file = here("code", "allparam_data_ordered.RData"))
qs::qsave(data, file = here("./data/parameter-data/allparam_data_ordered.qs"))

if(range(data$ID, na.rm=TRUE)[2] != 434) {  #434!)
  stop("ERROR - incorrect range of equilibrium ID's")
}
