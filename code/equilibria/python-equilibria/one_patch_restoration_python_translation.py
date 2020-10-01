##########
##########
# This code contains the notes on translation of the MATLAB code that solves
# one-patch equilibria for a simple model to a Python script
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2020-09-03
##########
##########
import numpy as np

#### define variables here
# making each one as a list with the number of spaces (20) to store the results
# then multiplying them by the number of combinations we have -- here it's just
# three since we're only varrying a which has three possible values

r = 0.55 # rate coral overgrow turf
r_vec = [0.55]*(20*3)

a = [0.1, 0.3, 0.5] # rate macroalgae overgrow coral
a_vec = [0.1]*20 + [0.3]*20 + [0.5]*20

d = 0.24 # rate of natural coral mortality
d_vec = [0.24]*(20*3)

y = 0.77 # rate a unit macroalgae overgrows a unit of turf algae
y_vec = [0.77]*(20*3)

g = 0.3 # rate of grazing on macroalgae and turf algae
g_vec = [0.3]*(20*3)

z = 0 # rate coral larvae recruit and overgrow turf algae
z_vec = [0]*(20*3)

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
