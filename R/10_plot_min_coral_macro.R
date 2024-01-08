# load packages and the data we need ===========================================
library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)
library(qs)
library(viridis)

source(here::here("./R/00_global_funs.R"))

`%notin%` <- Negate(`%in%`)

matching_df <- readr::read_csv(
  here::here("./data/plotting-data/min-coral-ids-macro.csv")) %>% 
  dplyr::mutate(macro = factor(macro, levels = c("low", "med", "high")))

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

# make the plots ===============================================================
div_by_macro_recruit <- ggplot(data = matching_df) + 
  geom_point(aes(x = a, y = g, fill = min_coral)) + 
  geom_point(aes(x = a, y = g, color = as.factor(min_coral))) + 
  facet_grid(macro~recruit_level) + 
  theme_base() + 
  scale_fill_gradientn("Minimum Coral",
                     breaks = c(0.01,0.25,0.5,0.75,0.96),
                     colors = c("grey", rev(viridis((4)))))+
  scale_color_manual("Minimum Coral",
                     breaks = seq(0.01,0.96,0.01),
                     values = c("grey", rev(viridis((95)))))+
  #scale_color_viridis_c("Minimum Coral", option = "plasma", direction = 1,
  #breaks = c(0.01, 0.5, 0.96)) + 
  labs(x = "Coral/Macroalgae Comp.", y = "Grazing") +
  scale_x_continuous(
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.99),
    labels = c(0.01, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~ . , name = "Recruitment", 
                        breaks = NULL, labels = NULL)) +
  scale_y_continuous(
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.99),
    labels = c(0.01, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~ . , name = "Macroalgae Level", 
                        breaks = NULL, labels = NULL)
  ) + 
  guides(color="none")
ggsave(
  here::here("./graphs/conclusions-plots/min-coral-by-macro-div-by-recruit.png"),
  div_by_macro_recruit
)
div_by_macro_comp <- ggplot(data = matching_df) + 
  geom_point(aes(x = z, y = g, fill = min_coral)) + 
  geom_point(aes(x = z, y = g, color = as.factor(min_coral))) + 
  facet_grid(macro~overgrow_level) + 
  theme_base() + 
  scale_fill_gradientn("Minimum Coral",
                       breaks = c(0.01,0.25,0.5,0.75,0.96),
                       colors = c("grey", rev(viridis((4)))))+
  scale_color_manual("Minimum Coral",
                     breaks = seq(0.01,0.96,0.01),
                     values = c("grey", rev(viridis((95)))))+
  #scale_color_viridis_c("Minimum Coral", option = "plasma", direction = 1,
  #breaks = c(0.01, 0.5, 0.96)) +
  labs(x = "Recruitment", y = "Grazing") + 
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~ . , name = "Coral/Macroalgae Comp.", 
                        breaks = NULL, labels = NULL)) +
  scale_y_continuous(
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.99),
    labels = c(0.01, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~ . , name = "Macroalgae Level", 
                        breaks = NULL, labels = NULL)
  ) + 
  guides(color="none")
ggsave(
  here::here("./graphs/conclusions-plots/min-coral-by-macro-div-by-comp.png"),
  div_by_macro_comp
)

div_by_macro_grazing <- ggplot(data = matching_df) +   
  geom_point(aes(x = a, y = z, fill = min_coral)) + 
  geom_point(aes(x = a, y = z, color = as.factor(min_coral))) + 
  facet_grid(macro~grazing_level) + 
  theme_base() + 
  scale_fill_gradientn("Minimum Coral",
                       breaks = c(0.01,0.25,0.5,0.75,0.96),
                       colors = c("grey", rev(viridis((4)))))+
  scale_color_manual("Minimum Coral",
                     breaks = seq(0.01,0.96,0.01),
                     values = c("grey", rev(viridis((95)))))+
  #scale_color_viridis_c("Minimum Coral", option = "plasma", direction = 1,
  #breaks = c(0.01, 0.5, 0.96)) +
  labs(x = "Coral/Macroalgae Comp.", y = "Recruitment") + 
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~ . , name = "Grazing", 
                        breaks = NULL, labels = NULL)) +
  scale_y_continuous(
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.99),
    labels = c(0.01, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~ . , name = "Macroalgae Level", 
                        breaks = NULL, labels = NULL)
  ) + 
  guides(color="none")

ggsave(
  here::here("./graphs/conclusions-plots/min-coral-by-macro-div-by-grazing.png"),
  div_by_macro_grazing
)
