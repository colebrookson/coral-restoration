# load packages and the data we need ===========================================
library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)
library(qs)

`%notin%` <- Negate(`%in%`)

matching_df <- readr::read_csv(matching_df, 
                 here::here("./data/plotting-data/min-coral-ids.csv"))

# set up the data for the different levels =====================================
matching_df <- matching_df %>% dplyr::filter(min_coral != -999999)

matching_df$grazing_level <- NA
matching_df[which(matching_df$g <= 0.2),"grazing_level"] <- "0-0.2"
matching_df[which(matching_df$g > 0.2 & 
                    matching_df$g <= 0.4),"grazing_level"] <- "0.2-0.4"
matching_df[which(matching_df$g > 0.4),"grazing_level"] <- ">0.4"
matching_df$grazing_level <- factor(matching_df$grazing_level,
                                    levels = c("0-0.2", "0.2-0.4", ">0.4"))

matching_df$overgrow_level <- NA
matching_df[which(matching_df$a <= 0.33),"overgrow_level"] <- "low"
matching_df[which(matching_df$a > 0.33 & 
                    matching_df$a <= 0.66),"overgrow_level"] <- "med"
matching_df[which(matching_df$a > 0.66),"overgrow_level"] <- "high"
matching_df$overgrow_level <- factor(matching_df$overgrow_level,
                                     levels = c("low", "med", "high"))

matching_df$recruit_level <- NA
matching_df[which(matching_df$z <= 0.33),"recruit_level"] <- "low"
matching_df[which(matching_df$z > 0.33 & 
                    matching_df$z <= 0.66),"recruit_level"] <- "med"
matching_df[which(matching_df$z > 0.66),"recruit_level"] <- "high"
matching_df$recruit_level <- factor(matching_df$recruit_level,
                                    levels = c("low", "med", "high"))