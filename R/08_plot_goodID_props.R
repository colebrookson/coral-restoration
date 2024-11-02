# set up =======================================================================

library(here)
library(magrittr)
library(dplyr)
library(ggplot2)
library(viridis)

# a - macro algae comp
# z - recruitment
# g - grazing

source(here("./R/00_global_funs.R"))

`%notin%` <- Negate(`%in%`)
prop_df <- readr::read_csv(
  here::here(paste0(
    "./data/plotting-data/proportion",
    "-of-ICs-to-good-coral-state.csv"
  ))
)

prop_df <- prop_df[which(prop_df$prop >= 0), ]

# seprate out grazing into three groups ========================================

prop_df$grazing_level <- NA
prop_df[which(prop_df$g <= 0.2), "grazing_level"] <- "0-0.2"
prop_df[which(prop_df$g > 0.2 &
  prop_df$g <= 0.4), "grazing_level"] <- "0.2-0.4"
prop_df[which(prop_df$g > 0.4), "grazing_level"] <- ">0.4"
prop_df$grazing_level <- factor(prop_df$grazing_level,
  levels = c("0-0.2", "0.2-0.4", ">0.4")
)

prop_df$overgrow_level <- NA
prop_df[which(prop_df$a <= 0.33), "overgrow_level"] <- "low"
prop_df[which(prop_df$a > 0.33 &
  prop_df$a <= 0.66), "overgrow_level"] <- "med"
prop_df[which(prop_df$a > 0.66), "overgrow_level"] <- "high"
prop_df$overgrow_level <- factor(prop_df$overgrow_level,
  levels = c("low", "med", "high")
)

prop_df$recruit_level <- NA
prop_df[which(prop_df$z <= 0.33), "recruit_level"] <- "low"
prop_df[which(prop_df$z > 0.33 &
  prop_df$z <= 0.66), "recruit_level"] <- "med"
prop_df[which(prop_df$z > 0.66), "recruit_level"] <- "high"
prop_df$recruit_level <- factor(prop_df$recruit_level,
  levels = c("low", "med", "high")
)

# define colours for the plots =================================================
# prop has 182 different levels and ranges from 0 to 1, want to highlight 0 and 1
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

proplvls <- levels(as.factor(prop_df$prop))
prop_df$prop_cols <- NA
for (i in 1:length(proplvls)) {
  prop_df$prop_cols[prop_df$prop == proplvls[i]] <- cols[i]
}

div_by_recruit <- ggplot(data = prop_df) +
  geom_point(aes(x = a, y = g, fill = prop), alpha = 0.3) +
  geom_point(aes(x = a, y = g, colour = as.factor(prop)), alpha = 0.3) + # colour = prop
  facet_grid(~recruit_level) +
  theme_base() +
  scale_color_manual(breaks = proplvls, values = cols, na.value = "grey80") +
  scale_fill_gradientn("Minimum Coral",
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.96),
    colors = cols
  ) +
  # scale_color_manual("Minimum Coral",
  #   breaks = seq(0.01, 0.96, 0.01),
  #   values = cols,
  #   #na.value = "grey60"
  # ) +
  labs(x = "Coral/Macroalgae Comp.", y = "Grazing") +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~.,
      name = "Recruitment",
      breaks = NULL, labels = NULL
    )
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
  scale_y_continuous(
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.99),
    labels = c(0.01, 0.25, 0.5, 0.75, 1.0)
  ) +
  guides(color = "none")
ggsave(
  here::here("./graphs/conclusions-plots/prop_div_by_recruit.png"),
  div_by_recruit,
  height = 6, width = 10
)

div_by_comp <- ggplot(data = prop_df) +
  geom_point(aes(x = a, y = g, fill = prop), alpha = 0.3) +
  geom_point(aes(x = z, y = g, colour = as.factor(prop)), alpha = 0.3) + # colour = prop
  facet_grid(~overgrow_level) +
  theme_base() +
  scale_color_manual(breaks = proplvls, values = cols, na.value = "grey80") +
  guides(color = FALSE) +
  # scale_color_gradient("Good Eq. Prop", low = "seagreen", high = "skyblue") +
  labs(x = "Recruitment", y = "Grazing") +
  scale_fill_gradientn("Minimum Coral",
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.96),
    colors = cols
  ) +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~.,
      name = "Coral/Macroalgae Comp.",
      breaks = NULL, labels = NULL
    )
  )
ggsave(
  here::here("./graphs/conclusions-plots/prop_div_by_comp.png"),
  div_by_comp,
  height = 6, width = 10
)

div_by_grazing <- ggplot(data = prop_df) +
  geom_point(aes(x = a, y = z, fill = prop), alpha = 0.3) +
  geom_point(aes(x = a, y = z, colour = as.factor(prop)), alpha = 0.3) +
  facet_grid(~grazing_level) +
  theme_base() +
  scale_color_manual(breaks = proplvls, values = cols, na.value = "grey80") +
  guides(color = FALSE) +
  scale_fill_gradientn("Minimum Coral",
    breaks = c(0.01, 0.25, 0.5, 0.75, 0.96),
    colors = cols
  ) +
  # scale_color_gradient("Good Eq. Prop", low = "seagreen", high = "skyblue") +
  labs(x = "Coral/Macroalgae Comp.", y = "Recruitment") +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~.,
      name = "Grazing",
      breaks = NULL, labels = NULL
    )
  )
ggsave(
  here::here("./graphs/conclusions-plots/prop_div_by_grazing.png"),
  div_by_grazing,
  height = 6, width = 10
)
