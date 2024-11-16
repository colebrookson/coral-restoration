#' I just need a single file to run if I make plotting updates because that
#' will make my life easier lol

# run this code before calling ggplot2 function
guides_merge <- function(gdefs) {
    gdefs <- lapply(gdefs, function(g) {
        g$hash <-
            paste(g$order, g$hash, sep = "z")
        g
    })
    tapply(
        gdefs, sapply(gdefs, function(g) g$hash),
        function(gs) Reduce(guide_merge, gs)
    )
}
environment(guides_merge) <- environment(ggplot)
assignInNamespace("guides_merge", guides_merge, pos = "package:ggplot2")


source(here::here("./R/07_plot_min_coral.R"))
source(here::here("./R/08_plot_goodID_props.R"))
source(here::here("./R/10_plot_min_coral_macro.R"))
source(here::here("./R/13_plot_all_min_coral.R"))
source(here::here("./R/14_01_trajectories_plotting_nhrcp.R"))
source(here::here("./R/14_02_trajectories_plotting_tnc.R"))
