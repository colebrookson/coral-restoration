# load packages and the data we need ===========================================
library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)
library(qs)

`%notin%` <- Negate(`%in%`)

allparam_data_ordered <- qs::qread(
  here::here("./data/parameter-data/allparam_data_ordered.qs")
)

all_g <- unique(allparam_data_ordered$g)
all_z <- unique(allparam_data_ordered$z)
all_a <- unique(allparam_data_ordered$a)

# find which IDs have > 0.3 coral and < 0.3 MA =================================

good_ids <- allparam_data_ordered[
  which(allparam_data_ordered$C >= 0.3 & allparam_data_ordered$M < 0.3 &
          allparam_data_ordered$stability == "stable_node"), 
]

# process all files ============================================================

# NOTE
#' So what we'll do here is loop through each basin of attraction file, 
#' pull in the info, see if any of the ids are in in the file, if so, find 
#' the minimum coral value that corresponds to that id 

# make a df to put all the percentages in 
matching_df <- expand.grid(g = all_g, a = all_a, z = all_z)
matching_df$min_coral <- NA

# process one file 
process_prop <- function(a, g, z, matching_df, basinofattractionID, 
                         allparam_data_ordered, good_ids) {
  
  # count number of different initial macroalgae values for each initial coral 
  # cover value
  num_malg <- basinofattractionID %>%
    count(initC1)
  
  # if there's no ID's return NA
  if(!any(basinofattractionID$Equilibrium %in% good_ids$ID)) {
    matching_df[
      which(matching_df$g == g 
            & matching_df$a == a & 
              matching_df$z == z), "min_coral"
    ] <- NA
    return(matching_df)
  }
  
  # get only the ID's that are in the good ids
  temp <- basinofattractionID[
    which(basinofattractionID$Equilibrium %in% good_ids$ID), 
  ]
  
  # determine at what initial coral cover value all initial macroalgae values 
  # go to a good basin
  # do so by figuring out the smallest initial coral cover value that is fully 
  # represented in temp (i.e. all of the original basinofattractionID rows 
  # are in there)
  for(i in 1:dim(num_malg)[1]){
    if(length(temp$InitCond[temp$initC1 == num_malg$initC1[i]]) == 
       num_malg$n[num_malg$initC1==num_malg$initC1[i]]){
      minC1 <- num_malg$initC1[i]
      break
    }
  }
  
  matching_df[
    which(matching_df$g == g 
          & matching_df$a == a & 
            matching_df$z == z), "min_coral"
  ] <- minC1 
  
  return(matching_df)
  
}

for(row in seq_len(nrow(matching_df))) {
  a = matching_df$a[row]
  g = matching_df$g[row]
  z = matching_df$z[row]
  
  path <- here::here("./data/cc/basinofattractionID_files/")
  file <- paste0(path, "basinofattractionID_recr",z,"g",g,"_mccomp",
                 a,"_20000.RData")
  
  
  if(file.exists(file) & file.size(file) > 0) {
    load(
      file
    )
    matching_df <- process_prop(
      a = a,
      g = g,
      z = z,
      basinofattractionID = basinofattractionID,
      matching_df = matching_df,
      allparam_data_ordered = allparam_data_ordered,
      good_ids = good_ids
    )
  } else {
    matching_df[
      which(matching_df$g == g & matching_df$a == a & matching_df$z == z), "min_coral"
    ] <- -999999
  }
  if(row %% 100 == 0){print(row)}
}


readr::write_csv(matching_df, here::here("./data/plotting-data/min-coral-ids_all.csv"))