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
  equil_val_store <- data.frame(
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
}
