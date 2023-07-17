# load packages and the data we need ===========================================

library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)


data_g1 <- readr::read_csv(
  here::here("./data/parameter-data/all-param-vals-unordered-1.csv"))
data_g2 <- readr::read_csv(
  here::here("./data/parameter-data/all-param-vals-unordered-2.csv"))
data_g3 <- readr::read_csv(
  here::here("./data/parameter-data/all-param-vals-unordered-3.csv"))

data <- rbind(data_g1, data_g2, data_g3)

# make grazing bifurcation diagrams ============================================

# the parameter values
dispersal_val = unique(data$z)
a_val = unique(data$a)
g_val = unique(data$g)

#Grazing bifurcation diagrams
for(i in 1:length(a_val)){ #loop through the 'a' values
  for(k in 1:length(dispersal_val)){ #loop through the dispersal values
    
    temp_df <- data[which(
      data$z == dispersal_val[k] & 
        data$a == round(a_val[i],2)), ]
    
    #all   
    pdf(here(
      "graphs","bifurcationdiagrams","grazing_bifurcation_diagrams",
      "pdf","Normal",
      #paste0("Dispersal",dispersal_val[k]),
      paste0("GrazingBifurcationGraph_dispersal",dispersal_val[k],
             "MCcomp",a_val[i],".pdf")))
        plot(
          x = temp_df$g, 
          y = temp_df$C, 
          col = temp_df$Colour,
          xlim = c(0,1), ylim = c(0,1), pch = 16, 
          xlab = "g", ylab = "C cover", 
          main = paste("Dispersal = ", dispersal_val[k], "and a =", a_val[i]))
        
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle",
                        "Bifurcation Point?"), 
           col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    png(here(
      "graphs","bifurcationdiagrams","grazing_bifurcation_diagrams",
      "png","Normal",
      #paste0("Dispersal",dispersal_val[k]),
      paste0("GrazingBifurcationGraph_dispersal",dispersal_val[k],
             "MCcomp",a_val[i],".png")))
    
    plot(
      x = temp_df$g, 
      y = temp_df$C, 
      col = temp_df$Colour,
      xlim = c(0,1), ylim = c(0,1), pch = 16, 
      xlab = "g", ylab = "C cover", 
      main = paste("Dispersal = ", dispersal_val[k], "and a =", a_val[i]))
    
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle",
                        "Bifurcation Point?"), 
           col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    ## plot them again but just transparent ====================================    
    
    pdf(here("graphs","bifurcationdiagrams","grazing_bifurcation_diagrams",
             "pdf","Transparent",
             #paste0("Dispersal", dispersal_val[k]),
             paste0("Transparent_GrazingBifurcationGraph_dispersal",
                    dispersal_val[k],"MCcomp",a_val[i],".pdf")))
    
    plot(
      x = temp_df$g, 
      y = temp_df$C, 
      col = alpha(temp_df$Colour, 0.4), 
      xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g",
      ylab = "C cover", 
      main = paste("Dispersal = ", dispersal_val[k], "and a =", a_val[i]))
    
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", 
                        "Bifurcation Point?"), 
           col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
    dev.off()
    
    png(here("graphs","bifurcationdiagrams","grazing_bifurcation_diagrams",
             "png","Transparent",
             #paste0("Dispersal",dispersal_val[k]),
             paste0("Transparent_GrazingBifurcationGraph_dispersal",
                    dispersal_val[k],"MCcomp",a_val[i],".png")))
    
    plot(
      x = temp_df$g, 
      y = temp_df$C, 
      col = alpha(temp_df$Colour, 0.4), 
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
    
    temp_df <- data[which(data$z == dispersal_val[k] &
                                   data$g == round(g_val[i],2)), ]
    
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
             "png","Normal",
             #paste0("Dispersal",dispersal_val[k]), 
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
             "png","Transparent",
             #paste0("Dispersal",dispersal_val[k]), 
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
    temp_df <- data[which(data$a == a_val[k] & 
                                   data$g == round(g_val[i],2)), ]
    
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