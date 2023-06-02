# load packages and the data we need ===========================================

library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)

allparam_data <- readr::read_csv(
  here("./data/full_restoration_model_output.csv")
)

# for some reason there's no column names? 
names(allparam_data) <- c("r","d","y","a","g","z",
                          "Equilibrium","C","M","eig_1","eig_2")

#remove the rows that are empty
allparam_data_abr <- allparam_data[allparam_data$C <= 1,]

# add in a stability column
allparam_data_abr <- allparam_data_abr %>% 
  dplyr::mutate(
    # denotes the stability type based on the values of the two eigenvalues 
    # we're considering
    stability = dplyr::case_when(
      eig_1 > 1 & eig_2 > 0   ~ "unstable_node",
      eig_1 < 0 & eig_2 < 0   ~ "stable_node",
      eig_1 == 0 & eig_2 == 0 ~ "bifurcation_point",
      FALSE ~ "saddle_node"
    ),
    # include a colour here 
    Colour = dplyr::case_when(
      stability == "stable_node"       ~ "black",
      stability == "unstable_node"     ~ "gold",
      stability == "saddle_node"       ~ "purple",
      stability == "bifurcation_point" ~ "green"
    )
  )

# make grazing bifurcation diagrams ============================================

# the parameter values
dispersal_val = c(0,0.05,0.25,0.5)
a_val = seq(0,1,0.01)
g_val = seq(0,0.5,0.01)

#Grazing bifurcation diagrams
for(i in 1:length(a_val)){ #loop through the 'a' values
  for(k in 1:length(dispersal_val)){ #loop through the dispersal values
    
    temp_df <- allparam_data_abr[which(
      allparam_data_abr$z == dispersal_val[k] & 
        allparam_data_abr$a == round(a_val[i],2)), ]
    
    #all   
    pdf(here(
      "graphs","bifurcationdiagrams","grazing_bifurcation_diagrams",
      "pdf","Normal",paste0("Dispersal",dispersal_val[k]),
      paste0("GrazingBifurcationGraph_dispersal",dispersal_val[k],
             "MCcomp",a_val[i],".pdf")))
        plot(
          x = temp_df$g, 
          y = temp_df$C, 
          col = temp_df$colour,
          xlim = c(0,1), ylim = c(0,1), pch = 16, 
          xlab = "g", ylab = "C cover", 
          main = paste("Dispersal = ", dispersal_val[k], "and a =", a_val[i]))
        
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle",
                        "Bifurcation Point?"), 
           col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    png(here(
      "graphs","bifurcationdiagrams","grazing_bifurcation_diagrams",
      "png","Normal",paste0("Dispersal",dispersal_val[k]),
      paste0("GrazingBifurcationGraph_dispersal",dispersal_val[k],
             "MCcomp",a_val[i],".png")))
    
    plot(
      x = temp_df$g, 
      y = temp_df$C, 
      col = temp_df$colour,
      xlim = c(0,1), ylim = c(0,1), pch = 16, 
      xlab = "g", ylab = "C cover", 
      main = paste("Dispersal = ", dispersal_val[k], "and a =", a_val[i]))
    
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle",
                        "Bifurcation Point?"), 
           col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    ## plot them again but just transparent ====================================    
    
    pdf(here("graphs","bifurcationdiagrams","grazing_bifurcation_diagrams",
             "pdf","Transparent",paste0("Dispersal",
                                        dispersal_val[k]),
             paste0("Transparent_GrazingBifurcationGraph_dispersal",
                    dispersal_val[k],"MCcomp",a_val[i],".pdf")))
    
    plot(
      x = temp_df$g, 
      y = temp_df$C, 
      col = alpha(temp_df$colour, 0.4), 
      xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g",
      ylab = "C cover", 
      main = paste("Dispersal = ", dispersal_val[k], "and a =", a_val[i]))
    
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", 
                        "Bifurcation Point?"), 
           col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
    dev.off()
    
    png(here("graphs","bifurcationdiagrams","grazing_bifurcation_diagrams",
             "png","Transparent",paste0("Dispersal",dispersal_val[k]),
             paste0("Transparent_GrazingBifurcationGraph_dispersal",
                    dispersal_val[k],"MCcomp",a_val[i],".png")))
    
    plot(
      x = temp_df$g, 
      y = temp_df$C, 
      col = alpha(temp_df$colour, 0.4), 
      xlim = c(0,1), ylim = c(0,1), pch = 16, 
      xlab = "g",
      ylab = "C cover", 
      main = paste("Dispersal = ", dispersal_val[k], "and a =", a_val[i]))
    
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", 
                        "Bifurcation Point?"), 
           col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
    dev.off()
  }
}

# Macroalg-coral bifurcation diagrams ==========================================
for(i in 1:length(g_val)){ #loop through the grazing values
  for(k in 1:length(dispersal_val)){ #loop through the dispersal values
    
    temp_df <- allparam_data_abr[which(allparam_data_abr$z == dispersal_val[k] &
                                   allparam_data_abr$g == round(g_val[i],2)), ]
    
    #all   
    pdf(here("graphs","bifurcationdiagrams","MCcomp_bifurcation_diagrams",
             "pdf","Normal",paste0("Dispersal",
                                   dispersal_val[k]), 
             paste0("MCcompBifurcationGraph_dispersal",
                    dispersal_val[k],"grazing",g_val[i],".pdf")))
    
    plot(
      x =temp_df$a, 
      y = temp_df$z, 
      col = temp_df$Colour, 
      xlim = c(0,1), ylim = c(0,1), pch = 16, 
      xlab = "MCcomp", 
      ylab = "C cover", 
      main = paste("Dispersal = ", dispersal_val[k], "and grazing =", g_val[i]))
    
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", 
                        "Bifurcation Point?"), 
           col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    png(here("graphs","bifurcationdiagrams","MCcomp_bifurcation_diagrams",
             "png","Normal",paste0("Dispersal",dispersal_val[k]), 
             paste0("MCcompBifurcationGraph_dispersal",
                    dispersal_val[k],"grazing",g_val[i],".png")))
    plot(
      x =temp_df$a, 
      y = temp_df$z, 
      col = temp_df$Colour, 
      xlim = c(0,1), ylim = c(0,1), pch = 16, 
      xlab = "MCcomp", 
      ylab = "C cover", 
      main = paste("Dispersal = ", dispersal_val[k], "and grazing =", g_val[i]))
    
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", 
                        "Bifurcation Point?"), 
           col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    ## plot them again but transparent =========================================   
    pdf(here("graphs","bifurcationdiagrams","MCcomp_bifurcation_diagrams",
             "pdf","Transparent",paste0("Dispersal",dispersal_val[k]), 
             paste0("Transparent_MCcompBifurcationGraph_dispersal",
                    dispersal_val[k],"grazing",g_val[i],".pdf")))
    plot(
      x =temp_df$a, 
      y = temp_df$z, 
      col = alpha(temp_df$Colour, 0.4),
      xlim = c(0,1), ylim = c(0,1), pch = 16, 
      xlab = "MCcomp", 
      ylab = "C cover", 
      main = paste("Dispersal = ", dispersal_val[k], "and grazing =", g_val[i]))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", 
                        "Bifurcation Point?"), 
           col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
    dev.off()
    
    png(here("graphs","bifurcationdiagrams","MCcomp_bifurcation_diagrams",
             "png","Transparent",paste0("Dispersal",dispersal_val[k]), 
             paste0("Transparent_MCcompBifurcationGraph_dispersal",
                    dispersal_val[k],"grazing",g_val[i],".png")))
    
    plot(
      x =temp_df$a, 
      y = temp_df$z, 
      col = alpha(temp_df$Colour, 0.4),
      xlim = c(0,1), ylim = c(0,1), pch = 16, 
      xlab = "MCcomp", 
      ylab = "C cover", 
      main = paste("Dispersal = ", dispersal_val[k], "and grazing =", g_val[i]))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", 
                        "Bifurcation Point?"), 
           col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
    dev.off()
    
  }
}

# dispersal bifurcation diagrams ===============================================
#Dispersal bifurcation diagrams
for(i in 1:length(g_val)){ #loop through the grazing values
  for(k in 1:length(a_val)){ #loop through the a values
    if(0 <= g_val[i] & g_val[i] <= 0.1){
      g_bin <- "low"
    }
    if(0.1 < g_val[i] & g_val[i] <= 0.4){
      g_bin <- "medium"
    }
    if(0.4 < g_val[i] & g_val[i] <= 1){
      g_bin <- "high"
    }
    
    # get the temporary dataset 
    temp_df <- allparam_data_abr[which(allparam_data_abr$a == a_val[k] & 
                                   allparam_data_abr$g == round(g_val[i],2)), ]
    
    #all   
    pdf(here("graphs","bifurcationdiagrams","dispersal_bifurcation_diagrams",
             "pdf","Normal",paste0(g_bin,"_grazing"), 
             paste0("DispersalBifurcationGraph_MCcomp",
                    a_val[k],"grazing",g_val[i],".pdf")))
    plot(
      x = temp_df$z,
      y = temp_df$C,
      col = temp_df$Colour,
      xlim = c(0,1), 
      ylim = c(0,1), 
      pch = 16,
      xlab = "Dispersal", ylab = "C cover", 
      main = paste("MCcomp = ", a_val[k], "and grazing =", g_val[i]))
    
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle",
                        "Bifurcation Point?"), 
           col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    png(here("graphs","bifurcationdiagrams","dispersal_bifurcation_diagrams",
             "png","Normal",paste0(g_bin,"_grazing"), 
             paste0("DispersalBifurcationGraph_MCcomp",a_val[k],
                    "grazing",g_val[i],".png")))
    plot(
      x = temp_df$z,
      y = temp_df$C,
      col = temp_df$Colour,
      xlim = c(0,1), 
      ylim = c(0,1), 
      pch = 16,
      xlab = "Dispersal", ylab = "C cover", 
      main = paste("MCcomp = ", a_val[k], "and grazing =", g_val[i]))
    
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle",
                        "Bifurcation Point?"), 
           col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    #transparent   
    pdf(here("graphs","bifurcationdiagrams","dispersal_bifurcation_diagrams",
             "pdf","Transparent",paste0(g_bin,"_grazing"), 
             paste0("Transparent_DispersalBifurcationGraph_MCcomp",
                    a_val[k],"grazing",g_val[i],".pdf")))
    
    plot(
      x = temp_df$z,
      y = temp_df$C,
      col = alpha(temp_df$Colour, 0.4),
      xlim = c(0,1), 
      ylim = c(0,1), 
      pch = 16,
      xlab = "Dispersal", ylab = "C cover", 
      main = paste("MCcomp = ", a_val[k], "and grazing =", g_val[i]))
    
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle",
                        "Bifurcation Point?"), 
           col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    png(here("graphs","bifurcationdiagrams","dispersal_bifurcation_diagrams",
             "png","Transparent",paste0(g_bin,"_grazing"), 
             paste0("Transparent_DispersalBifurcationGraph_MCcomp",
                    a_val[k],"grazing",g_val[i],".png")))
    plot(
      x = temp_df$z,
      y = temp_df$C,
      col = alpha(temp_df$Colour, 0.4),
      xlim = c(0,1), 
      ylim = c(0,1), 
      pch = 16,
      xlab = "Dispersal", ylab = "C cover", 
      main = paste("MCcomp = ", a_val[k], "and grazing =", g_val[i]))
    
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle",
                        "Bifurcation Point?"), 
           col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
  }
}