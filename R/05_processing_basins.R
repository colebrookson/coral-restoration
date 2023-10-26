# set up =======================================================================

library(scales)
library(deSolve)
library(geometry)
library(fields)
library(here)
library(plotly)
library(tidyverse)

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

# process the files to find the percentage of ICs for each parameter combo that
# end up at a stable equilibrium with the "good" conditions

# make a df to put all the percentages in 
prop_df <- expand.grid(g = all_g, a = all_a, z = all_z)
prop_df$prop <- NA

# process one file 
process_prop <- function(a, g, z, prop_df, basinsabr, allparam_data_ordered, 
                         good_ids) {
  
  # get only the ID's that have size > 0
  temp <- basinsabr[
    which(basinsabr$Size > 0), "EquilibriumID"
  ]
  
  # if only one ID, process is easy 
  if(length(temp) == 1) {
    
    # get proportion of all 210 init conditions go to that id
    val <- basinsabr[which(basinsabr$Size > 0), "Size"] / 210
    
    # now if check if the ID is a "good" id
    if(temp %notin% good_ids$ID) {
      # if not a good ID, note 
      prop_df[
        which(prop_df$g == g & prop_df$a == a & prop_df$z == z), "prop"
        ] <- 0
    } else {
      # if the ID is good, make sure the param combo is valid 
      prop_df[
        which(prop_df$g == g & prop_df$a == a & prop_df$z == z), "prop"
        ] <- 
        ifelse( # checks if there is in fact that combo in df
          nrow(good_ids[which(good_ids$ID == temp & 
                                good_ids$a == a & 
                                good_ids$g == g & 
                                good_ids$z == z),]) == 0,
          0, # if the combo is NOT valid, assign zero
          val # if combo IS vaid, assign the val
      )
    }
  } else if(length(temp) > 1) { # if multiple IDs
    
    # keep track of how many of the trajs go to a good ID, might not be all
    sum <- 0
    for(i in seq_len(length(temp))) {
      
      # check for current id if it's a good one
      if(temp[i] %notin% good_ids$ID) {
        # if not a good idea, sum stays zero
        next 
      } else { # if the current id IS a good one
        val <- basinsabr[
          which(basinsabr$EquilibriumID == temp[i]), "Size"
        ]/210
        # if the ID is good, make sure the param combo is valid 
        sum <- 
          ifelse(
          nrow(good_ids[which(good_ids$ID == temp[i] & 
                                good_ids$a == a & 
                                good_ids$g == g & 
                                good_ids$z == z),]) == 0,
          sum + 0, 
          sum + val # not assigning to the prop df, just adding to sum
        )
      }
    }
    # once all the ID's have been checked and the sum is checked, put into 
    # the prop df
    prop_df[
      which(prop_df$g == g & prop_df$a == a & prop_df$z == z), "prop"
            ] <- sum
  }
  return(prop_df)
}

# loop through the function runs ===============================================

for(row in seq_len(nrow(prop_df))) {
  a = prop_df$a[row]
  g = prop_df$g[row]
  z = prop_df$z[row]
  
  path <- here::here("./data/cc/basinsabr_files/")
  file <- paste0(path, "basins_recr",z,"g",g,"_mccomp",a,"_20000.RData")

  
  if(file.exists(file)) {
    load(
      file
    )
    prop_df <- process_prop(
      a = a,
      g = g,
      z = z,
      basinsabr = basinsabr,
      prop_df = prop_df,
      allparam_data_ordered = allparam_data_ordered,
      good_ids = good_ids
    )
  } else {
    prop_df[
      which(prop_df$g == g & prop_df$a == a & prop_df$z == z), "prop"
    ] <- -999999
  }
  if(row %% 100 == 0){print(row)}
}


# we want coral > 0.3 and MacroAlgae < 0.3 

# plot in 3d ===================================================================


# Plot
library(plotly)

prop_df <- prop_df[which(prop_df$prop >= 0),]
fig <- plot_ly(prop_df,
               marker = list(color = ~prop, 
                             colorscale = list(c(0, 1), 
                                               c("seagreen", "skyblue")), 
                             showscale = TRUE)) %>% 
  add_trace(fig, x = ~a, y = ~g, z = ~z,
                 type = "scatter3d", mode = "markers",
          opacity = .2) %>% 
  layout(scene = list(xaxis = 
                        list(title = 'Dispersal'),
                      yaxis = list(title = 'Grazing'),
                      zaxis = list(title = 'Coral/Macroalgae Comp.')),
                      annotations = list(  
                        x = 1.13,
                        y = 1.05,
                        text = 'Proportion in a "Good" eq',
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                      ))

fig
