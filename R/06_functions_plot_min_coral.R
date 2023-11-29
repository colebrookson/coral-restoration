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

# process all files ============================================================

# process the files to find the percentage of ICs for each parameter combo that
# end up at a stable equilibrium with the "good" conditions

# make a df to put all the percentages in 
matching_df <- expand.grid(g = all_g, a = all_a, z = all_z)
matching_df$min_coral <- NA

# process one file 
process_prop <- function(a, g, z, matching_df, basinofattractionID, 
                         allparam_data_ordered, good_ids) {
  
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
  
  matching_df[
    which(matching_df$g == g 
          & matching_df$a == a & 
            matching_df$z == z), "min_coral"
  ] <- min(temp$initC1)  

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


readr::write_csv(matching_df, here::here("./data/plotting-data/min-coral-ids.csv"))

matching_df <- readr::read_csv(here::here("./data/plotting-data/min-coral-ids.csv"))

matching_df <- matching_df %>% dplyr::filter(min_coral != -999999)

matching_df$grazing_level <- NA
matching_df[which(matching_df$g <= 0.33),"grazing_level"] <- "low"
matching_df[which(matching_df$g > 0.33 & 
                matching_df$g <= 0.66),"grazing_level"] <- "med"
matching_df[which(matching_df$g > 0.66),"grazing_level"] <- "high"
matching_df$grazing_level <- factor(matching_df$grazing_level,
                                levels = c("low", "med", "high"))

matching_df$recruit_level <- NA
matching_df[which(matching_df$a <= 0.33),"recruit_level"] <- "low"
matching_df[which(matching_df$a > 0.33 & 
                matching_df$a <= 0.66),"recruit_level"] <- "med"
matching_df[which(matching_df$a > 0.66),"recruit_level"] <- "high"
matching_df$recruit_level <- factor(matching_df$recruit_level,
                                levels = c("low", "med", "high"))

matching_df$overgrow_level <- NA
matching_df[which(matching_df$z <= 0.33),"overgrow_level"] <- "low"
matching_df[which(matching_df$z > 0.33 & 
                matching_df$z <= 0.66),"overgrow_level"] <- "med"
matching_df[which(matching_df$z > 0.66),"overgrow_level"] <- "high"
matching_df$overgrow_level <- factor(matching_df$overgrow_level,
                                 levels = c("low", "med", "high"))

ggplot(data = matching_df) + 
  geom_point(aes(x = a, y = g, colour = min_coral)) + 
  facet_grid(~overgrow_level) + 
  theme_base() + 
  scale_color_viridis_c("Minimum Coral", option = "plasma", direction = -1) + 
  labs(x = "Recruitment", y = "Grazing") +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~ . , name = "Coral/Macroalgae Comp.", 
                        breaks = NULL, labels = NULL)) 
ggplot(data = matching_df) + 
  geom_point(aes(x = z, y = g, colour = min_coral)) + 
  facet_grid(~recruit_level) + 
  theme_base() + 
  scale_color_viridis_c("Minimum Coral", option = "plasma", direction = -1) + 
  labs(x = "Coral/Macroalgae Comp.", y = "Grazing") + 
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~ . , name = "Recruitment", 
                        breaks = NULL, labels = NULL)) 
















