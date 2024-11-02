# load packages and the data we need ===========================================
library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)
library(qs)

`%notin%` <- Negate(`%in%`)
source(here::here("./R/00_global_funs.R"))

# read in data
matching_df <- readr::read_csv(
  here::here("./data/plotting-data/min-coral-ids_all.csv")
)

# set up the data for the different levels =====================================
matching_df <- matching_df %>% dplyr::mutate(
  min_coral = ifelse(min_coral != -999999, min_coral, NA)
)

matching_df$grazing_level <- NA
matching_df[which(matching_df$g <= 0.2), "grazing_level"] <- "0-0.2"
matching_df[which(matching_df$g > 0.2 &
  matching_df$g <= 0.4), "grazing_level"] <- "0.2-0.4"
matching_df[which(matching_df$g > 0.4), "grazing_level"] <- ">0.4"
matching_df$grazing_level <- factor(matching_df$grazing_level,
  levels = c("0-0.2", "0.2-0.4", ">0.4")
)

matching_df$overgrow_level <- NA
matching_df[which(matching_df$a <= 0.33), "overgrow_level"] <- "low"
matching_df[which(matching_df$a > 0.33 &
  matching_df$a <= 0.66), "overgrow_level"] <- "med"
matching_df[which(matching_df$a > 0.66), "overgrow_level"] <- "high"
matching_df$overgrow_level <- factor(matching_df$overgrow_level,
  levels = c("low", "med", "high")
)

matching_df$recruit_level <- NA
matching_df[which(matching_df$z <= 0.33), "recruit_level"] <- "low"
matching_df[which(matching_df$z > 0.33 &
  matching_df$z <= 0.66), "recruit_level"] <- "med"
matching_df[which(matching_df$z > 0.66), "recruit_level"] <- "high"
matching_df$recruit_level <- factor(matching_df$recruit_level,
  levels = c("low", "med", "high")
)

# define colours for the plots =================================================
# 0.01 is the lowest value and 0.96 is the highest value of min_coral, want these two to have distinctly different colours
# the values of min_coral are max 2 decimal points, so there are only 96 possible discrete values
highestval <- 96
generate_colors <- function(n, high, mid, low) {
  # Create a color ramp palette function using high, mid, and low colors
  color_palette <- colorRampPalette(c(low, mid, high))

  # Generate a sequence of n colors
  colors <- color_palette(n)

  return(colors)
}

# Example usage
n <- 182
low <- "#178b8b" # Red for the highest value
mid <- "#51516C" # White for the midpoint
high <- "#8B174D" # Blue for the lowest value

cols <- generate_colors(n, high, mid, low)

# make the plots ===============================================================
div_by_recruit <- ggplot(data = matching_df) +
  geom_point(aes(x = a, y = g, fill = min_coral), size = 2, alpha = 0.3) +
  geom_point(aes(x = a, y = g, color = as.factor(min_coral)),
    size = 2,
    alpha = 0.3
  ) +
  facet_grid(~recruit_level) +
  theme_base() +
  scale_fill_gradientn("Minimum Coral",
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.96),
    colors = cols,
    na.value = "grey80"
  ) +
  scale_color_manual("Minimum Coral",
    breaks = seq(0.01, 0.96, 0.01),
    values = cols,
    na.value = "grey80"
  ) +
  labs(x = "Coral/Macroalgae Comp.", y = "Grazing") +
  scale_x_continuous(
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.99),
    labels = c(0.01, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~.,
      name = "Recruitment",
      breaks = NULL, labels = NULL
    )
  ) +
  guides(color = "none")
ggsave(
  here::here("./graphs/conclusions-plots/min-coral_all-div-by-recruit.png"),
  div_by_recruit,
  height = 6, width = 10
)
div_by_comp <- ggplot(data = matching_df) +
  geom_point(aes(x = z, y = g, fill = min_coral), size = 2) +
  geom_point(aes(x = z, y = g, colour = as.factor(min_coral)),
    size = 2,
    alpha = 0.3
  ) +
  facet_grid(~overgrow_level) +
  theme_base() +
  scale_fill_gradientn("Minimum Coral",
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.96),
    colors = cols,
    na.value = "grey80"
  ) +
  scale_color_manual("Minimum Coral",
    breaks = seq(0.01, 0.96, 0.01),
    values = cols,
    na.value = "grey80"
  ) +
  labs(x = "Recruitment", y = "Grazing") +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~.,
      name = "Coral/Macroalgae Comp.",
      breaks = NULL, labels = NULL
    )
  ) +
  guides(color = "none")
ggsave(
  here::here("./graphs/conclusions-plots/min-coral_all-div-by-comp.png"),
  div_by_comp,
  height = 6, width = 10
)

div_by_grazing <- ggplot(data = matching_df) +
  geom_point(aes(x = z, y = z, fill = min_coral), size = 2) +
  geom_point(aes(x = a, y = z, colour = as.factor(min_coral)),
    size = 2,
    alpha = 0.3
  ) +
  facet_grid(~grazing_level) +
  theme_base() +
  scale_fill_gradientn("Minimum Coral",
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.96),
    colors = cols,
    na.value = "grey80"
  ) +
  scale_color_manual(
    breaks = seq(0.01, 0.96, 0.01), values = cols,
    na.value = "grey80"
  ) +
  # scale_color_viridis_c("Minimum Coral", option = "plasma", direction = 1,
  #  breaks = c(0.01, 0.5, 0.96)) +
  labs(x = "Coral/Macroalgae Comp.", y = "Recruitment") +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~.,
      name = "Grazing",
      breaks = NULL, labels = NULL
    )
  ) +
  guides(color = "none")

ggsave(
  here::here("./graphs/conclusions-plots/min-coral_all-div-by-grazing.png"),
  div_by_grazing,
  height = 6, width = 10
)
