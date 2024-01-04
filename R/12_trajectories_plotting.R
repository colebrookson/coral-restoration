# load packages and the data we need ===========================================
library(tidyverse)
library(here)
library(qs)

source(here::here("./R/cc/00_functions.R"))
source(here::here("./R/00_global_funs.R"))
`%notin%` <- Negate(`%in%`)

mean_nhrcp <- qs::qread(here::here("./data/plotting-data/mean-nhrcp-sims.qs")) 
mean_nhrcp <- mean_nhrcp[which(mean_nhrcp$time <= 50), ] %>% 
  #dplyr::select(time, group_num, )
  tidyr::pivot_longer(
    cols = c(M, C),
    names_to = "Cover",
    values_to = "vals"
  )

nhrcp_df <- qs::qread(here::here("./data/plotting-data/nhrcp-simulation.qs"))
nhrcp_df <- nhrcp_df[which(nhrcp_df$time <= 50), ] 
nhrcp_df <- nhrcp_df %>% 
  dplyr::group_by(g_val, mc_comp, recruitvalue) %>% 
  dplyr::rename(M = M1, C = C1) %>% 
  dplyr::mutate(group_num = dplyr::cur_group_id()) %>% 
  #dplyr::select(time, group_num, )
  tidyr::pivot_longer(
    cols = c(M, C),
    names_to = "Cover",
    values_to = "vals"
  )

gc()

nhrcp_plot <- ggplot() + 
  geom_line(data = nhrcp_df[which(nhrcp_df$Cover == "M"),], 
            aes(x = time, y = vals, group = group_num), 
            colour = "green4", alpha = 0.02)  +
  geom_line(data = nhrcp_df[which(nhrcp_df$Cover == "C"),], 
            aes(x = time, y = vals, group = group_num),
            colour = "pink1", alpha = 0.02) + 
  geom_line(data = mean_nhrcp, #[which(mean_nhrcp$Cover == "M"),],
            aes(x = time, y = vals, colour = Cover),
            linewidth = 1.5) +
  theme_base() + 
  xlim(0, 50) + 
  labs(x = "Time", y = "Macroalgae Proportion")  + 
  scale_colour_manual("NHRCP Cover", values = c("pink1", "green4"), 
                      labels = c("Coral", "Macroalgae")) + 
  theme(
    legend.position = c(0.8, 0.4)
  )
ggplot2::ggsave(
  here::here("./graphs/conclusions-plots/nhrcp-scenario.png"),
  nhrcp_plot
)  
