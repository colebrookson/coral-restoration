# set up =======================================================================

library(here)
library(plotly)
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
  here::here(paste0("./data/plotting-data/proportion",
                    "-of-ICs-to-good-coral-state.csv")))

prop_df <- prop_df[which(prop_df$prop >= 0),]

# plot in 3d ===================================================================

# Plot
fig <- plotly::plot_ly(prop_df,
               marker = list(color = ~prop, 
                             colorscale = list(c(0, 1), 
                                               c("seagreen", "skyblue")), 
                             showscale = TRUE)) %>% 
  plotly::add_trace(., x = ~a, y = ~g, z = ~z,
            type = "scatter3d", mode = "markers",
            opacity = .05) %>% 
  plotly::layout(scene = list(xaxis = list(title = 'Recruitment'),
                      yaxis = list(title = 'Grazing'),
                      zaxis = list(title = 'Coral/Macroalgae Comp.')),
         annotations = list(  
           x = 1.13,
           y = 1.05,
           text = 'Prop in a "Good" eq',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))

fig 
plotly::orca(fig, here::here("./graphs/conclusions-plots/prop-goodIDs-3d.png"))
  plotly::orca(
    toImageButtonOptions = list(
      format = "png",
      filename = here::here("./graphs/conclusions-plots/prop-goodIDs-3d.png"),
      width = 600,
      height = 700
    )
  )
plotly::save_image(fig, )
# seprate out grazing into three groups ========================================

prop_df$grazing_level <- NA
prop_df[which(prop_df$g <= 0.2),"grazing_level"] <- "0-0.2"
prop_df[which(prop_df$g > 0.2 & 
                    prop_df$g <= 0.4),"grazing_level"] <- "0.2-0.4"
prop_df[which(prop_df$g > 0.4),"grazing_level"] <- ">0.4"
prop_df$grazing_level <- factor(prop_df$grazing_level,
                                    levels = c("0-0.2", "0.2-0.4", ">0.4"))

prop_df$overgrow_level <- NA
prop_df[which(prop_df$a <= 0.33),"overgrow_level"] <- "low"
prop_df[which(prop_df$a > 0.33 & 
                prop_df$a <= 0.66),"overgrow_level"] <- "med"
prop_df[which(prop_df$a > 0.66),"overgrow_level"] <- "high"
prop_df$overgrow_level <- factor(prop_df$overgrow_level,
                                levels = c("low", "med", "high"))

prop_df$recruit_level <- NA
prop_df[which(prop_df$z <= 0.33),"recruit_level"] <- "low"
prop_df[which(prop_df$z > 0.33 & 
                prop_df$z <= 0.66),"recruit_level"] <- "med"
prop_df[which(prop_df$z > 0.66),"recruit_level"] <- "high"
prop_df$recruit_level <- factor(prop_df$recruit_level,
                                 levels = c("low", "med", "high"))

# define colours for the plots =================================================
#prop has 182 different levels and ranges from 0 to 1, want to highlight 0 and 1
cols <- c("#b8e3d0", plasma(180),"pink")
proplvls <- levels(as.factor(prop_df$prop))
prop_df$prop_cols <- NA
for(i in 1:length(proplvls)){
  prop_df$prop_cols[prop_df$prop == proplvls[i]] <- cols[i]
}


div_by_recruit <- ggplot(data = prop_df) + 
  geom_point(aes(x = a, y = g, colour = as.factor(prop))) + #colour = prop
  facet_grid(~recruit_level) + 
  theme_base() + 
  scale_color_manual(breaks = proplvls, values = c("#b8e3d0", plasma(180),"pink"))+
  guides(color = FALSE)+
  #scale_color_gradient("Good Eq. Prop", low = "seagreen", high = "skyblue") + 
  labs(x = "Coral/Macroalgae Comp.", y = "Grazing") +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~ . , name = "Recruitment", 
                        breaks = NULL, labels = NULL)) 
ggsave(
  here::here("./graphs/conclusions-plots/prop_div_by_recruit.png"),
  div_by_recruit
)

div_by_comp <- ggplot(data = prop_df) + 
  geom_point(aes(x = z, y = g, colour = as.factor(prop))) + #colour = prop
  facet_grid(~overgrow_level) + 
  theme_base() + 
  scale_color_manual(breaks = proplvls, values = c("#b8e3d0", plasma(180),"pink"))+
  guides(color = FALSE)+
  #scale_color_gradient("Good Eq. Prop", low = "seagreen", high = "skyblue") + 
  labs(x = "Recruitment", y = "Grazing") + 
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~ . , name = "Coral/Macroalgae Comp.", 
                        breaks = NULL, labels = NULL)) 
ggsave(
  here::here("./graphs/conclusions-plots/prop_div_by_comp.png"),
  div_by_comp
)

div_by_grazing <- ggplot(data = prop_df) + 
  geom_point(aes(x = a, y = z, colour = as.factor(prop))) + 
  facet_grid(~grazing_level) + 
  theme_base() + 
  scale_color_manual(breaks = proplvls, values = c("#b8e3d0", plasma(180),"pink"))+
  guides(color = FALSE)+
  #scale_color_gradient("Good Eq. Prop", low = "seagreen", high = "skyblue") + 
  labs(x = "Coral/Macroalgae Comp.", y = "Recruitment") +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 0.99),
    labels = c(0, 0.25, 0.5, 0.75, 1.0),
    sec.axis = sec_axis(~ . , name = "Grazing", 
                        breaks = NULL, labels = NULL)) 
ggsave(
  here::here("./graphs/conclusions-plots/prop_div_by_grazing.png"),
  div_by_grazing
)
