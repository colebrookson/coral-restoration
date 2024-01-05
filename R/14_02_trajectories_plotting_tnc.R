# load packages and the data we need ===========================================
library(tidyverse)
library(here)
library(qs)

source(here::here("./R/cc/00_functions.R"))
source(here::here("./R/00_global_funs.R"))
`%notin%` <- Negate(`%in%`)

tnc_df <- qs::qread(here::here("./data/plotting-data/tnc-simulation.qs"))

mean_tnc <- qs::qread(here::here("./data/plotting-data/mean-tnc-sims.qs")) 
mean_tnc <- mean_tnc[which(mean_tnc$time <= 50), ] %>% 
  #dplyr::select(time, group_num, )
  tidyr::pivot_longer(
    cols = c(M, C),
    names_to = "Cover",
    values_to = "vals"
  )

tnc_df <- qs::qread(here::here("./data/plotting-data/tnc-simulation.qs"))
tnc_df <- tnc_df[which(tnc_df$time <= 50), ] 
tnc_df <- tnc_df %>% 
  dplyr::group_by(g_val, mc_comp, recruitvalue) %>% 
  dplyr::rename(M = M1, C = C1) %>% 
  dplyr::mutate(group_num = dplyr::cur_group_id()) %>% 
  #dplyr::select(time, group_num, )
  tidyr::pivot_longer(
    cols = c(M, C),
    names_to = "Cover",
    values_to = "vals"
  )

tnc_plot <- ggplot() + 
  geom_line(data = tnc_df[which(tnc_df$Cover == "M"),],
            aes(x = time, y = vals, group = group_num),
            colour = "#bc9e82", alpha = 0.02)  +
  geom_line(data = tnc_df[which(tnc_df$Cover == "C"),], 
            aes(x = time, y = vals, group = group_num),
            colour = "pink1", alpha = 0.2) + 
  geom_line(data = mean_tnc, #[which(mean_tnc$Cover == "M"),],
            aes(x = time, y = vals, colour = Cover),
            linewidth = 1.5) +
  theme_base() + 
  xlim(0, 50) + ylim(0, 1) + 
  labs(x = "Time", y = "Cover Proportions")  + 
  scale_colour_manual("TNC Cover", values = c("pink1", "#80461b"), 
                      labels = c("Coral", "Macroalgae")) + 
  theme(
    legend.position = c(0.8, 0.4)
  ) 
ggplot2::ggsave(
  here::here("./graphs/conclusions-plots/tnc-scenario.png"),
  tnc_plot
)  

tnc_plot_c <- ggplot() + 
  # geom_line(data = tnc_df[which(tnc_df$Cover == "M"),], 
  #           aes(x = time, y = vals, group = group_num), 
  #           colour = "#80461b", alpha = 0.05)  +
  geom_line(data = tnc_df[which(tnc_df$Cover == "C"),], 
            aes(x = time, y = vals, group = group_num),
            colour = "pink1", alpha = 0.05) + 
  geom_line(data = mean_tnc[which(mean_tnc$Cover == "C"),], #[which(mean_tnc$Cover == "M"),],
            aes(x = time, y = vals, colour = Cover),
            linewidth = 1.5) +
  theme_base() + 
  xlim(0, 50) + ylim(0, 1) + 
  labs(x = "Time", y = "Cover Proportions")  + 
  scale_colour_manual("TNC Cover", values = c("pink1", "#80461b"), 
                      labels = c("Coral", "Macroalgae")) + 
  theme(
    legend.position = c(0.8, 0.4)
  )
ggplot2::ggsave(
  here::here("./graphs/conclusions-plots/tnc-scenario-only-coral.png"),
  tnc_plot_c
)  

tnc_plot_m <- ggplot() + 
  geom_line(data = tnc_df[which(tnc_df$Cover == "M"),],
            aes(x = time, y = vals, group = group_num),
            colour = "#80461b", alpha = 0.05)  +
  # geom_line(data = tnc_df[which(tnc_df$Cover == "C"),], 
  #           aes(x = time, y = vals, group = group_num),
  #           colour = "#80461b", alpha = 0.05) + 
  geom_line(data = mean_tnc[which(mean_tnc$Cover == "M"),], #[which(mean_tnc$Cover == "M"),],
            aes(x = time, y = vals, colour = Cover),
            linewidth = 1.5) +
  theme_base() + 
  xlim(0, 50) + ylim(0, 1) + 
  labs(x = "Time", y = "Cover Proportions")  + 
  scale_colour_manual("TNC Cover", values = c("#80461b"), 
                      labels = c("Macroalgae")) + 
  theme(
    legend.position = c(0.8, 0.4)
  )
ggplot2::ggsave(
  here::here("./graphs/conclusions-plots/tnc-scenario-only-macroalgae.png"),
  tnc_plot_m
)  
