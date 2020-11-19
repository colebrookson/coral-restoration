import numpy as np
from mpmath import *

#### define variables here
# making each one as a list with the number of spaces (20) to store the results
# then multiplying them by the number of combinations we have

#number of combos:
combos = 51*51*4

r = 0.55 # rate coral overgrow turf
r_vec = [0.55]*(20*3)

d = 0.24 # rate of natural coral mortality
d_vec = [0.24]*(20*3)

y = 0.77 # rate a unit macroalgae overgrows a unit of turf algae
y_vec = [0.77]*(20*3)

a = [0.1, 0.3, 0.5] # rate macroalgae overgrow coral
a_vec = [0.1]*20 + [0.3]*20 + [0.5]*20



g = np.arange(0,0.51,0.01) # rate of grazing on macroalgae and turf algae
g_vec = []
for i in range(0, int(combos/len(g))):
    g_vec_temp_1 = []
    for j in g:
        g_vec_temp_2 = [j]*(20)
        g_vec_temp_1.extend(g_vec_temp_2)
    g_vec.extend(g_vec_temp_1)

z = [0, 0.05, 0.25, 0.5] # rate coral larvae recruit and overgrow turf algae
z_vec = []
for i in z:
    z_vec_temp = [i]*(20*int(combos/4))
    z_vec.extend(z_vec_temp)

#### make a vector for the number of equilibrium values
equil_num = list(range(1,21))
equil_num = equil_num*(3)

#### define the matrix for the data
#this will have 60 rows (20 results * 3 iterations of parameter combinations)
#and 11 columns - 2 for C & M, 2 for eigenvalues, and 7 parameters
rows = 60
cols = 11
shape = (rows, cols)
results_matrix = np.ones(shape) #initialize as ones
results_matrix[:,:] = results_matrix[:,:] + 1 #scale up to twos

#### populate matrix with data
results_matrix[:,0] = r_vec
results_matrix[:,1] = a_vec
results_matrix[:,2] = d_vec
results_matrix[:,3] = y_vec
results_matrix[:,4] = g_vec
results_matrix[:,5] = z_vec
results_matrix[:,6] = equil_num
