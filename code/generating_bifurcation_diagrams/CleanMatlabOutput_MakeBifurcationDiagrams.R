library(scales)
library(deSolve)
library(tidyverse)
library(readxl)
library(here)

here() #here is now set to your root directory with .Rproj file

allparam_data <- read.csv("~/GitHub/Cole_CoralRestorationModelling/Coral-Restoration-Modeling/code/equilibria/full_restoration_model_output.csv",header = FALSE)
head(allparam_data)

#assigning column names in accordance with their contents
names(allparam_data) <- c("r","d","y","a","g","z","Equilibrium","C","M","eig_1","eig_2")
head(allparam_data)

#remove the rows that are empty
allparam_data_abr <- allparam_data[allparam_data$C <= 1,]
head(allparam_data_abr)

#need to add a 'stability' column
allparam_data_abr$stability <- NA
allparam_data_abr$stability[allparam_data_abr$eig_1 > 0 & allparam_data_abr$eig_2 > 0] <- 'unstable_node'
allparam_data_abr$stability[allparam_data_abr$eig_1 < 0 & allparam_data_abr$eig_2 < 0] <- 'stable_node'
allparam_data_abr$stability[allparam_data_abr$eig_1 == 0 | allparam_data_abr$eig_2 == 0] <- 'bifurcation_point'
allparam_data_abr$stability[is.na(allparam_data_abr$stability)] <- 'saddle_node'

#give each of the stability designations an associated colour
allparam_data_abr$Colour <- NA
allparam_data_abr$Colour[allparam_data_abr$stability == "stable_node"] <- 'black'
allparam_data_abr$Colour[allparam_data_abr$stability == "unstable_node"] <- 'gold'
allparam_data_abr$Colour[allparam_data_abr$stability == "saddle_node"] <- 'purple'
allparam_data_abr$Colour[allparam_data_abr$stability == "bifurcation_point"] <- 'green'

save(allparam_data_abr, file = here("code", "allparam_data_pre_ordering.RData"))

#maybe at some point have the stable nodes ordered to be on top of the unstable nodes and saddle nodes like did for the two patch model but that may not be necessary

#what values of the different parameters am i looking at?
#allparam_data_abr %>%
  #distinct(a) #[0.0,0.5] by 0.01
  #distinct(g) #[0.0,0.5] by 0.01
  #distinct(z) #[0.0,0.05,0.25,0.50]
dispersal_val = c(0,0.05,0.25,0.5)
a_val = seq(0,1,0.01)
g_val = seq(0,0.5,0.01)

#Grazing bifurcation diagrams
for(i in 1:length(a_val)){ #loop through the 'a' values
	  for(k in 1:length(dispersal_val)){ #loop through the dispersal values

#all   
pdf(here("graphs","bifurcationdiagrams","grazing_bifurcation_diagrams","pdf","Normal",paste0("Dispersal",dispersal_val[k]),paste0("GrazingBifurcationGraph_dispersal",dispersal_val[k],"MCcomp",a_val[i],".pdf")))
#png(here("graphs","bifurcationdiagrams","grazing_bifurcation_diagrams","png","Normal",paste0("Dispersal",dispersal_val[k]),paste0("GrazingBifurcationGraph_dispersal",dispersal_val[k],"MCcomp",a_val[i],".png")))
plot(x = allparam_data_abr$g[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$a == round(a_val[i],2)], y = allparam_data_abr$C[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$a == round(a_val[i],2)], col = allparam_data_abr$Colour[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$a == round(a_val[i],2)], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Dispersal = ", dispersal_val[k], "and a =", a_val[i]))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#transparent  
pdf(here("graphs","bifurcationdiagrams","grazing_bifurcation_diagrams","pdf","Transparent",paste0("Dispersal",dispersal_val[k]),paste0("Transparent_GrazingBifurcationGraph_dispersal",dispersal_val[k],"MCcomp",a_val[i],".pdf")))
#png(here("graphs","bifurcationdiagrams","grazing_bifurcation_diagrams","png","Transparent",paste0("Dispersal",dispersal_val[k]),paste0("Transparent_GrazingBifurcationGraph_dispersal",dispersal_val[k],"MCcomp",a_val[i],".png")))
plot(x = allparam_data_abr$g[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$a == round(a_val[i],2)], y = allparam_data_abr$C[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$a == round(a_val[i],2)], col = alpha(allparam_data_abr$Colour[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$a == round(a_val[i],2)],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Dispersal = ", dispersal_val[k], "and a =", a_val[i]))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
dev.off()

	}
}

#Macroalg-coral bifurcation diagrams
for(i in 1:length(g_val)){ #loop through the grazing values
  for(k in 1:length(dispersal_val)){ #loop through the dispersal values
    
    #all   
    pdf(here("graphs","bifurcationdiagrams","MCcomp_bifurcation_diagrams","pdf","Normal",paste0("Dispersal",dispersal_val[k]), paste0("MCcompBifurcationGraph_dispersal",dispersal_val[k],"grazing",g_val[i],".pdf")))
    #png(here("graphs","bifurcationdiagrams","MCcomp_bifurcation_diagrams","png","Normal",paste0("Dispersal",dispersal_val[k]), paste0("MCcompBifurcationGraph_dispersal",dispersal_val[k],"grazing",g_val[i],".png")))
    plot(x = allparam_data_abr$a[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$g == round(g_val[i],2)], y = allparam_data_abr$C[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$g == round(g_val[i],2)], col = allparam_data_abr$Colour[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$g == round(g_val[i],2)], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "MCcomp", ylab = "C cover", main = paste("Dispersal = ", dispersal_val[k], "and grazing =", g_val[i]))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    #transparent   
    pdf(here("graphs","bifurcationdiagrams","MCcomp_bifurcation_diagrams","pdf","Transparent",paste0("Dispersal",dispersal_val[k]), paste0("Transparent_MCcompBifurcationGraph_dispersal",dispersal_val[k],"grazing",g_val[i],".pdf")))
    #png(here("graphs","bifurcationdiagrams","MCcomp_bifurcation_diagrams","png","Transparent",paste0("Dispersal",dispersal_val[k]), paste0("Transparent_MCcompBifurcationGraph_dispersal",dispersal_val[k],"grazing",g_val[i],".png")))
    plot(x = allparam_data_abr$a[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$g == round(g_val[i],2)], y = allparam_data_abr$C[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$g == round(g_val[i],2)], col = alpha(allparam_data_abr$Colour[allparam_data_abr$z == dispersal_val[k] & allparam_data_abr$g == round(g_val[i],2)],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "MCcomp", ylab = "C cover", main = paste("Dispersal = ", dispersal_val[k], "and grazing =", g_val[i]))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
    dev.off()
    
  }
}

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
    #all   
    #pdf(here("graphs","bifurcationdiagrams","dispersal_bifurcation_diagrams","pdf","Normal",paste0(g_bin,"_grazing"), paste0("DispersalBifurcationGraph_MCcomp",a_val[k],"grazing",g_val[i],".pdf")))
    png(here("graphs","bifurcationdiagrams","dispersal_bifurcation_diagrams","png","Normal",paste0(g_bin,"_grazing"), paste0("DispersalBifurcationGraph_MCcomp",a_val[k],"grazing",g_val[i],".png")))
    plot(x = allparam_data_abr$z[allparam_data_abr$a == a_val[k] & allparam_data_abr$g == round(g_val[i],2)], y = allparam_data_abr$C[allparam_data_abr$a == a_val[k] & allparam_data_abr$g == round(g_val[i],2)], col = allparam_data_abr$Colour[allparam_data_abr$a == a_val[k] & allparam_data_abr$g == round(g_val[i],2)], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Dispersal", ylab = "C cover", main = paste("MCcomp = ", a_val[k], "and grazing =", g_val[i]))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
    dev.off()
    
    #transparent   
    #pdf(here("graphs","bifurcationdiagrams","dispersal_bifurcation_diagrams","pdf","Transparent",paste0(g_bin,"_grazing"), paste0("Transparent_DispersalBifurcationGraph_MCcomp",a_val[k],"grazing",g_val[i],".pdf")))
    png(here("graphs","bifurcationdiagrams","dispersal_bifurcation_diagrams","png","Transparent",paste0(g_bin,"_grazing"), paste0("Transparent_DispersalBifurcationGraph_MCcomp",a_val[k],"grazing",g_val[i],".png")))
    plot(x = allparam_data_abr$z[allparam_data_abr$a == a_val[k] & allparam_data_abr$g == round(g_val[i],2)], y = allparam_data_abr$C[allparam_data_abr$a == a_val[k] & allparam_data_abr$g == round(g_val[i],2)], col = alpha(allparam_data_abr$Colour[allparam_data_abr$a == a_val[k] & allparam_data_abr$g == round(g_val[i],2)],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "Dispersal", ylab = "C cover", main = paste("MCcomp = ", a_val[k], "and grazing =", g_val[i]))
    legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
    dev.off()
    
  }
}
