#note: can eventually use the code Ariel wrote for bistab paper (making basinsfinal code) to do this but for right now this is sufficient, gives an idea of the categories to look for 

load('~/GitHub/Cole_CoralRestorationModelling/Coral-Restoration-Modeling/code/allparam_data_ordered.RData')
#check what IDs are in the 48 combos im looking at rn
recruitvalue_vec = rep(c(0, 0.05, 0.25, 0.5), each = 3*4)
g_val_vec = rep(rep(c(0.1, 0.21, 0.5), each = 4),4)
mc_comp_vec = rep(c(0.05, 0.3, 0.5, 0.99),4*3)
for(j in 1:length(g_val_vec)){ #just manually increased j because was too lazy to get it to print 'data' correctly
  recruitvalue <- recruitvalue_vec[j]
  g_val <- g_val_vec[j]
  mc_comp <- mc_comp_vec[j]
  print(paste("recruit value =", recruitvalue, "grazing rate = ", g_val, "mc comp = ", mc_comp))
  data[data$a == mc_comp & data$z == recruitvalue & data$g == g_val & data$stability == "stable_node",]
  #print(paste("IDs = ", data$ID[data$a == mc_comp_vec[j] & data$z == recruitvalue_vec[j] & data$g == g_val_vec[j]])) 
}
#(C*, M*) [using colours from here: https://i.pinimg.com/originals/05/66/2d/05662d0154e4b1742045e16fb32db5c0.png]

#M > 0, C = 0 -> brown (or #E76F51)
#ID = 7, 32, 52, 101: (0.0000000, 0.8701299), (0.0000000, 0.7272727)
#ID = 52, 101: (0.0000000, 0.3506494)

#M = 0, C > 0 -> pink (or #264653)
#ID = 2: (0.5636364, 0.0000000), (0.59734069, 0.0000000), (0.5973407, 0)
#ID = 213: (0.68083339, 0.0000000), (0.6808334, 0)
#ID = 330: (0.7396405, 0.0000000)

#M > 0, C > 0, C << M -> orange (or #F4A261)
#ID = 111: (0.01711402, 0.8518658), (0.07011178, 0.6411505)
#ID = 138: (0.008190083, 0.8640583), (0.02449782, 0.7054705)
#ID = 159: (0.005777521, 0.8673493), (0.01623038, 0.7170821)
#ID = 209: (0.003355275, 0.8706513), (0.008898608, 0.7273665), (0.07462366, 0.3196058)
#ID = 225: (0.09285074, 0.7700156)
#ID = 252: (0.03870482, 0.8412759), (0.1302112, 0.6069647)
#ID = 273: (0.02642826, 0.8573374), (0.07613033, 0.6781039)
#ID = 324: (0.01487641, 0.8724191), (0.03869262, 0.7273505)
#ID = 363: (0.07261025, 0.8156411)
#ID = 383: (0.04783937, 0.84683), (0.1434679, 0.6312844)
#ID = 433: (0.02611853, 0.8741093), (0.06700281, 0.7268306)

#M > 0, C > 0, C < M -> ? (or #E9C46A)
#ID = 339: (0.2249690, 0.6220717)

onlymalg <- c(7,32,52,101)
onlycoral <- c(2,213,330)
mostlymalg <- c(111,138,159,209,225,252,273,324,363,383,433)
malgcoral <- 339

basinofattractionID$colour <- NA
basinofattractionID$colour[basinofattractionID$Equilibrium == onlymalg] <- "#E76F51"
basinofattractionID$colour[basinofattractionID$Equilibrium == onlycoral] <- "#264653"
basinofattractionID$colour[basinofattractionID$Equilibrium == mostlymalg] <- "#F4A261"
basinofattractionID$colour[basinofattractionID$Equilibrium == malgcoral] <- "#E9C46A"
