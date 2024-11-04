# load packages and the data we need ===========================================
library(tidyverse)
library(here)
library(qs)
library(ggpubr)
library(grid)

source(here::here("./R/cc/00_functions.R"))
source(here::here("./R/00_global_funs.R"))
source(here::here("./R/14_01_trajectories_plotting_nhrcp.R"))
`%notin%` <- Negate(`%in%`)

# initial version ==============================================================

tnc_df <- qs::qread(here::here("./data/plotting-data/tnc-simulation.qs"))

mean_tnc <- qs::qread(here::here("./data/plotting-data/mean-tnc-sims.qs"))
mean_tnc <- mean_tnc[which(mean_tnc$time <= 50), ] %>%
  # dplyr::select(time, group_num, )
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
  # dplyr::select(time, group_num, )
  tidyr::pivot_longer(
    cols = c(M, C),
    names_to = "Cover",
    values_to = "vals"
  )

tnc_plot <- ggplot() +
  geom_line(
    data = tnc_df[which(tnc_df$Cover == "M"), ],
    aes(x = time, y = vals, group = group_num),
    colour = "#382619", alpha = 0.05
  ) +
  geom_line(
    data = tnc_df[which(tnc_df$Cover == "C"), ],
    aes(x = time, y = vals, group = group_num),
    colour = "#fe8da5", alpha = 0.05
  ) +
  geom_line(
    data = mean_tnc, # [which(mean_tnc$Cover == "M"),],
    aes(x = time, y = vals, colour = Cover),
    linewidth = 1.5
  ) +
  theme_base() +
  xlim(0, 50) +
  ylim(0, 1) +
  labs(x = "Time", y = "Cover Proportions") +
  scale_colour_manual("Cover",
    values = c("#fe8da5", "#382619"),
    labels = c("Coral", "Macroalgae")
  ) +
  theme(
    legend.position = c(0.8, 0.4),
  )
ggplot2::ggsave(
  here::here("./graphs/conclusions-plots/tnc-scenario.png"),
  tnc_plot,
  height = 5, width = 6
)

tnc_plot_c <- ggplot() +
  # geom_line(data = tnc_df[which(tnc_df$Cover == "M"),],
  #           aes(x = time, y = vals, group = group_num),
  #           colour = "#382619", alpha = 0.05)  +
  geom_line(
    data = tnc_df[which(tnc_df$Cover == "C"), ],
    aes(x = time, y = vals, group = group_num),
    colour = "#fe8da5", alpha = 0.05
  ) +
  geom_line(
    data = mean_tnc[which(mean_tnc$Cover == "C"), ], # [which(mean_tnc$Cover == "M"),],
    aes(x = time, y = vals, colour = Cover),
    linewidth = 1.5
  ) +
  theme_base() +
  xlim(0, 50) +
  ylim(0, 1) +
  labs(x = "Time", y = "Cover Proportions") +
  scale_colour_manual("TNC Cover",
    values = c("#fe8da5", "#382619"),
    labels = c("Coral", "Macroalgae")
  ) +
  theme(
    legend.position = c(0.8, 0.4)
  )
ggplot2::ggsave(
  here::here("./graphs/conclusions-plots/tnc-scenario-only-coral.png"),
  tnc_plot_c
)

tnc_plot_m <- ggplot() +
  geom_line(
    data = tnc_df[which(tnc_df$Cover == "M"), ],
    aes(x = time, y = vals, group = group_num),
    colour = "#382619", alpha = 0.05
  ) +
  # geom_line(data = tnc_df[which(tnc_df$Cover == "C"),],
  #           aes(x = time, y = vals, group = group_num),
  #           colour = "#382619", alpha = 0.05) +
  geom_line(
    data = mean_tnc[which(mean_tnc$Cover == "M"), ], # [which(mean_tnc$Cover == "M"),],
    aes(x = time, y = vals, colour = Cover),
    linewidth = 1.5
  ) +
  theme_base() +
  xlim(0, 50) +
  ylim(0, 1) +
  labs(x = "Time", y = "Cover Proportions") +
  scale_colour_manual("TNC Cover",
    values = c("#382619"),
    labels = c("Macroalgae")
  ) +
  theme(
    legend.position = c(0.8, 0.4)
  )
ggplot2::ggsave(
  here::here("./graphs/conclusions-plots/tnc-scenario-only-macroalgae.png"),
  tnc_plot_m
)

# plot recovery version ========================================================

mean_tnc <- qs::qread(
  here::here("./data/plotting-data/mean-tnc-sims-recovery.qs")
)
mean_tnc <- mean_tnc[which(mean_tnc$time <= 50), ] %>%
  # dplyr::select(time, group_num, )
  tidyr::pivot_longer(
    cols = c(M, C),
    names_to = "Cover",
    values_to = "vals"
  )

tnc_df <- qs::qread(
  here::here("./data/plotting-data/tnc-simulation-recovery.qs")
)
tnc_df <- tnc_df[which(tnc_df$time <= 50), ]
tnc_df <- tnc_df %>%
  dplyr::group_by(g_val, mc_comp, recruitvalue) %>%
  dplyr::rename(M = M1, C = C1) %>%
  dplyr::mutate(group_num = dplyr::cur_group_id()) %>%
  # dplyr::select(time, group_num, )
  tidyr::pivot_longer(
    cols = c(M, C),
    names_to = "Cover",
    values_to = "vals"
  )

tnc_plot_rec <- ggplot() +
  geom_line(
    data = tnc_df[which(tnc_df$Cover == "M"), ],
    aes(x = time, y = vals, group = group_num),
    colour = "#382619", alpha = 0.05
  ) +
  geom_line(
    data = tnc_df[which(tnc_df$Cover == "C"), ],
    aes(x = time, y = vals, group = group_num),
    colour = "#fe8da5", alpha = 0.05
  ) +
  geom_line(
    data = mean_tnc, # [which(mean_tnc$Cover == "M"),],
    aes(x = time, y = vals, colour = Cover),
    linewidth = 1.5
  ) +
  theme_base() +
  xlim(0, 50) +
  ylim(0, 1) +
  labs(x = "Time", y = "Cover Proportions") +
  scale_colour_manual("Cover",
    values = c("#fe8da5", "#382619"),
    labels = c("Coral", "Macroalgae")
  ) +
  theme(
    legend.position = c(0.8, 0.4)
  )
ggplot2::ggsave(
  here::here("./graphs/conclusions-plots/tnc-recovered-scenario.png"),
  tnc_plot
)

tnc_plot_c <- ggplot() +
  # geom_line(data = tnc_df[which(tnc_df$Cover == "M"),],
  #           aes(x = time, y = vals, group = group_num),
  #           colour = "#382619", alpha = 0.05)  +
  geom_line(
    data = tnc_df[which(tnc_df$Cover == "C"), ],
    aes(x = time, y = vals, group = group_num),
    colour = "#fe8da5", alpha = 0.05
  ) +
  geom_line(
    data = mean_tnc[which(mean_tnc$Cover == "C"), ], # [which(mean_tnc$Cover == "M"),],
    aes(x = time, y = vals, colour = Cover),
    linewidth = 1.5
  ) +
  theme_base() +
  xlim(0, 50) +
  ylim(0, 1) +
  labs(x = "Time", y = "Cover Proportions") +
  scale_colour_manual("TNC Cover",
    values = c("#fe8da5", "#382619"),
    labels = c("Coral", "Macroalgae")
  ) +
  theme(
    legend.position = c(0.8, 0.4)
  )
ggplot2::ggsave(
  here::here("./graphs/conclusions-plots/tnc-recovered-scenario-only-coral.png"),
  tnc_plot_c
)

tnc_plot_m <- ggplot() +
  geom_line(
    data = tnc_df[which(tnc_df$Cover == "M"), ],
    aes(x = time, y = vals, group = group_num),
    colour = "#382619", alpha = 0.05
  ) +
  # geom_line(data = tnc_df[which(tnc_df$Cover == "C"),],
  #           aes(x = time, y = vals, group = group_num),
  #           colour = "#382619", alpha = 0.05) +
  geom_line(
    data = mean_tnc[which(mean_tnc$Cover == "M"), ], # [which(mean_tnc$Cover == "M"),],
    aes(x = time, y = vals, colour = Cover),
    linewidth = 1.5
  ) +
  theme_base() +
  xlim(0, 50) +
  ylim(0, 1) +
  labs(x = "Time", y = "Cover Proportions") +
  scale_colour_manual("TNC Cover",
    values = c("#382619"),
    labels = c("Macroalgae")
  ) +
  theme(
    legend.position = c(0.8, 0.4)
  )
ggplot2::ggsave(
  here::here("./graphs/conclusions-plots/tnc-recovered-scenario-only-macroalgae.png"),
  tnc_plot_m
)

# plot them all (the nhrcp ones too) in one plot ===============================
all_scen_plots <- ggarrange(
  tnc_plot + rremove("ylab") + rremove("xlab") +
    theme(
      legend.text = element_text(size = rel(2)),
      legend.title = element_text(size = rel(2.5)),
      axis.text = element_text(size = rel(2)),
      plot.margin = margin(1, 1, 1, 1.2, "cm")
    ),
  nhrcp_plot + rremove("ylab") + rremove("xlab") +
    theme(
      legend.text = element_text(size = rel(2)),
      legend.title = element_text(size = rel(2.5)),
      axis.text = element_text(size = rel(2)),
      plot.margin = margin(1, 1, 1, 1.2, "cm")
    ),
  tnc_plot_rec + rremove("ylab") + rremove("xlab") +
    theme(
      legend.text = element_text(size = rel(2)),
      legend.title = element_text(size = rel(2.5)),
      axis.text = element_text(size = rel(2)),
      plot.margin = margin(1, 1, 1, 1.2, "cm")
    ),
  nhrcp_plot_rec + rremove("ylab") + rremove("xlab") +
    theme(
      legend.text = element_text(size = rel(2)),
      legend.title = element_text(size = rel(2.5)),
      axis.text = element_text(size = rel(2)),
      plot.margin = margin(1, 1, 1, 1.2, "cm")
    ),
  labels = c("A", "B", "C", "D"),
  font.label = list(color = "black", size = 30),
  widths = c(0.9, 0.9, 0.9, 0.9), heights = c(0.9, 0.9, 0.9, 0.9),
  ncol = 2, nrow = 2, common.legend = TRUE, legend = "right"
)
all_scen_plots_labeled <- annotate_figure(
  all_scen_plots,
  left = text_grob("Cover Proportions",
    rot = 90, size = 50
  ),
  bottom = text_grob("Time", size = 50),
  top = text_grob("TNC Case Study                    NHRCP Case Study",
    size = 50, face = "bold"
  )
)
ggplot2::ggsave(
  here::here("./graphs/conclusions-plots/nhrcp-tnc-all-scen.png"),
  all_scen_plots_labeled,
  width = 26,
  height = 17,
  bg = "white"
)
