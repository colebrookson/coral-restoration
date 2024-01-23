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

macro <- c("low", "med", "high")

matching_macro_df <- expand.grid(g = all_g, a = all_a, z = all_z, macro = macro)
matching_macro_df$min_coral <- NA

# NOTE
#' So what we'll do here is loop through each basin of attraction file, 
#' pull in the info, see if any of the ids are in in the file, if so, find 
#' the minimum coral value that corresponds to that id 

# process one file 
process_prop_macro <- function(a, g, z, matching_macro_df, basinofattractionID, 
                         allparam_data_ordered, good_ids, macro_level) {
  
  # if there's no ID's return NA
  if(!any(basinofattractionID$Equilibrium %in% good_ids$ID)) {
    matching_macro_df[
      which(matching_macro_df$g == g 
            & matching_macro_df$a == a & 
              matching_macro_df$z == z &
              matching_macro_df$macro == macro_level), "min_coral"
    ] <- NA
    return(matching_macro_df)
  }
  
  # get only the ID's that are in the good ids
  temp <- basinofattractionID[
    which(basinofattractionID$Equilibrium %in% good_ids$ID), 
  ]
  if(macro_level == "low") {
    temp <- temp[which(temp$initM1 <= 0.33), ]
  } else if(macro_level == "med") {
    temp <- temp[which(temp$initM1 > 0.33 & temp$initM1 <= 0.66), ]
  } else if(macro_level == "high") {
    temp <- temp[which(temp$initM1 > 0.66), ]
  }
  
  # count number of different initial macroalgae values for each initial coral 
  # cover value
  num_malg <- temp %>%
    count(initC1)
  if(nrow(num_malg) == 0) {
    matching_macro_df[
      which(matching_macro_df$g == g 
            & matching_macro_df$a == a & 
              matching_macro_df$z == z &
              matching_macro_df$macro == macro_level), "min_coral"
    ] <- NA
    return(matching_macro_df)
  }
  
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
  
  matching_macro_df[
    which(matching_macro_df$g == g 
          & matching_macro_df$a == a & 
            matching_macro_df$z == z &
            matching_macro_df$macro == macro_level), "min_coral"
  ] <- minC1 
  
  return(matching_macro_df)
  
}

for(row in seq_len(nrow(matching_macro_df))) {
  a = matching_macro_df$a[row]
  g = matching_macro_df$g[row]
  z = matching_macro_df$z[row]
  
  path <- here::here("./data/cc/basinofattractionID_files/")
  file <- paste0(path, "basinofattractionID_recr",z,"g",g,"_mccomp",
                 a,"_20000.RData")
  
  
  if(file.exists(file) & file.size(file) > 0) {
    load(
      file
    )
    for(macro_level in c("low", "med", "high")) {
      matching_macro_df <- process_prop_macro(
        a = a,
        g = g,
        z = z,
        basinofattractionID = basinofattractionID,
        matching_macro_df = matching_macro_df,
        allparam_data_ordered = allparam_data_ordered,
        good_ids = good_ids,
        macro_level = macro_level
      )
    }
  } else {
    matching_macro_df[
      which(matching_macro_df$g == g & matching_macro_df$a == a & 
              matching_macro_df$z == z), 
      "min_coral"
    ] <- -999999
  }
  if(row %% 100 == 0){print(row)}
}

matching_macro_df[which(is.infinite(matching_macro_df$min_coral)), "min_coral"]

readr::write_csv(matching_macro_df, 
                 here::here("./data/plotting-data/min-coral-ids-macro.csv"))

