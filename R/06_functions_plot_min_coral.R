# load packages and the data we need ===========================================
library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)
library(qs)

# read in the data with all the IDs ============================================
data <- qs::qread(here("./data/parameter-data/allparam_data_ordered.qs"))

data <- data[, names(data)[which(!is.na(names(data)))]]
# get the correct IDs ==========================================================

gt30_ids <- data %>% 
  dplyr::filter(C >= 0.3) %>% 
  dplyr::distinct(ID)

# loop through all the files ===================================================

# NOTE
#' So what we'll do here is loop through each basin of attraction file, 
#' pull in the info, see if any of the ids are in in the file, if so, find 
#' the minimum coral value that corresponds to that id 

# get the list of all the files 
path <- here("./data/cc/basinofattractionID_files/")
files <- list.files(path)

# make empty df 
matching_df <- data.frame(
  recruitment = as.numeric(),
  grazing = as.numeric(),
  competition = as.numeric(),
  ID = as.integer(),
  min_coral_cover = as.numeric()
)

# loop through all files and add to the df
count <- 1
for(file in files) {
  
  # load in the file - each one is called "basinofattractionID"
  full_file <- paste0(path, file)
  if(file.exists(full_file) & file.size(full_file) > 0) {
    load(full_file)
  }
  else {
    next 
  }
  
  
  # check to see if any of the id's are the ones we want
  if(any(basinofattractionID$Equilibrium %in% gt30_ids$ID)) {
    
    # get the ID or IDs
    if(length(
      unique(
        (basinofattractionID %>% 
        dplyr::filter(Equilibrium %in% gt30_ids$ID))$Equilibrium
      )) == 1) {
      id_val <- as.integer(basinofattractionID %>% 
                             dplyr::filter(Equilibrium %in% gt30_ids$ID) %>% 
                             dplyr::select(Equilibrium) %>% 
                             unique())
      
      # get the info needed 
      # first, find the min coral cover that has an ID we want
      min_coral_val <- as.numeric(basinofattractionID %>% 
                                    dplyr::filter(Equilibrium 
                                                  %in% gt30_ids$ID) %>% 
                                    dplyr::summarize(min(initC1)))
      
      # now get the other parameter values
      recruit_val <- 
        as.numeric(stringr::str_split(
          string = stringr::str_split(file, pattern = 
                                        "basinofattractionID_recr", 
                                      n = 2)[[1]][2],
          pattern = "g", n = 3)[[1]][1])
      grazing_val <- 
        as.numeric(stringr::str_split(
          string = stringr::str_split(file, 
                                      pattern = "g",  n = 2)[[1]][2],
          pattern = "_mccomp", n = 3)[[1]][1])
      competition_val <- 
        as.numeric(stringr::str_split(
          string = stringr::str_split(file, 
                                      pattern = "_mccomp",  n = 2)[[1]][2],
          pattern = "_20000", n = 3)[[1]][1])
      
      # put values into a dataframe
      curr_row <- data.frame(
        recruitment = recruit_val,
        grazing = grazing_val,
        competition = competition_val,
        ID = id_val,
        min_coral_cover = min_coral_val
      )
      
      # bind to the higher level df
      matching_df <- rbind(
        matching_df, curr_row
      )
      } else {
        for(id_val in unique(basinofattractionID$Equilibrium)) {
          
          # get the df with just that eq 
          temp <- basinofattractionID %>% 
            dplyr::filter(Equilibrium == id_val) 
          
          # first, find the min coral cover that has an ID we want
          min_coral_val <- as.numeric(temp %>% 
                                        dplyr::filter(Equilibrium 
                                                      %in% gt30_ids$ID) %>% 
                                        dplyr::summarize(min(initC1)))
          
          # now get the other parameter values
          recruit_val <- 
            as.numeric(stringr::str_split(
              string = stringr::str_split(file, pattern = 
                                            "basinofattractionID_recr", 
                                          n = 2)[[1]][2],
              pattern = "g", n = 3)[[1]][1])
          grazing_val <- 
            as.numeric(stringr::str_split(
              string = stringr::str_split(file, 
                                          pattern = "g",  n = 2)[[1]][2],
              pattern = "_mccomp", n = 3)[[1]][1])
          competition_val <- 
            as.numeric(stringr::str_split(
              string = stringr::str_split(file, 
                                          pattern = "_mccomp",  n = 2)[[1]][2],
              pattern = "_20000", n = 3)[[1]][1])
          
          # put values into a dataframe
          curr_row <- data.frame(
            recruitment = recruit_val,
            grazing = grazing_val,
            competition = competition_val,
            ID = id_val,
            min_coral_cover = min_coral_val
          )
          
          # bind to the higher level df
          matching_df <- rbind(
            matching_df, curr_row
          )
        }
      }
  }
  if(count %% 1000 == 0){print(count)}
}
readr::write_csv(matching_df, here::here("./data/plotting-data/min-coral-ids.csv"))
ggplot(data = matching_df) + 
  geom_point(aes(x = grazing, y = min_coral_cover, 
                 shape = )) 
