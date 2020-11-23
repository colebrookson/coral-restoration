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

#maybe at some point have the stable nodes ordered to be on top of the unstable nodes and saddle nodes like did for the two patch model but that may not be necessary

#what values of the different parameters am i looking at?
allparam_data_abr %>%
  #distinct(a) #[0.0,0.5] by 0.01
  #distinct(g) #[0.0,0.5] by 0.01
   distinct(z) #[0.0,0.05,0.25,0.50]

#loop through all recruitment values, and all 3 levels of g2
g_lvl = c(0.1,0.3,0.5)
for(i in 1:100){
	for(j in 1:3){
recruitvalue = round((i-1)/100,4)
g_level = g_lvl[j]	

#stable only
#pdf(paste0("StableOnly_Heterograz_GrazingBifurcationGraph_dispersal",recruitvalue,"grazinglevel",g_level,".pdf"))
png(paste0("StableOnly_Heterograz_GrazingBifurcationGraph_dispersal",recruitvalue,"grazinglevel",g_level,".png"))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level & twopatch_ext_complete$ID > 0], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level & twopatch_ext_complete$ID > 0], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level & twopatch_ext_complete$ID > 0], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Only Stable Nodes, Dispersal = ", recruitvalue, "and g_level =", g_level))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#all
#pdf(paste0("Heterograz_GrazingBifurcationGraph_dispersal",recruitvalue,"grazinglevel",g_level,".pdf"))
png(paste0("Heterograz_GrazingBifurcationGraph_dispersal",recruitvalue,"grazinglevel",g_level,".png")) 
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level], col = twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level], xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Dispersal = ", recruitvalue, "and g_level =", g_level))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = c("black","gold","purple","green"), pch = c(20,20))
dev.off()

#transparent
#pdf(paste0("TransparentHeterograz_GrazingBifurcationGraph_dispersal",recruitvalue,"grazinglevel",g_level,".pdf"))
png(paste0("TransparentHeterograz_GrazingBifurcationGraph_dispersal",recruitvalue,"grazinglevel",g_level,".png"))
plot(x = twopatch_ext_complete$g[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level], y = twopatch_ext_complete$C1[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level], col = alpha(twopatch_ext_complete$Colour[twopatch_ext_complete$p_c == recruitvalue & twopatch_ext_complete$p_m == recruitvalue & twopatch_ext_complete$q_c == recruitvalue & twopatch_ext_complete$q_m == recruitvalue & twopatch_ext_complete$g2 == g_level],0.4), xlim = c(0,1), ylim = c(0,1), pch = 16, xlab = "g", ylab = "C cover", main = paste("Dispersal = ", recruitvalue, "and g_level =", g_level))
legend("topleft", c("Stable Node", "Unstable Node", "Saddle", "Bifurcation Point?"), col = alpha(c("black","gold","purple","green"),0.4), pch = c(20,20))
dev.off()
	}
}
