import numpy as np
import scipy.integrate as spi
import pandas as pd
import matplotlib.pyplot as plt
#formats the output of a plot, keeps the plot attached to the cell
%matplotlib inline

a = [0.1, 0.3, 0.5]  # rate macroalgae overgrow coral
g = [0.1, 0.3, 0.5]  # grazing rate
z = [0, 0.05, 0.25, 0.5] #rate coral larvae recruit to + overgrow turf
r = 0.55 # rate coral overgrow turf
d = 0.24 # natural coral mortality rate
y = 0.77 #rate macroalgae overgrow turf

# Set the initial conditions
#x and y coords of all of the initial starting points
x_coords = np.arange(0.01,1,0.05)
M_temp, C_temp = np.meshgrid(x_coords, x_coords) #this makes a mesh grid at those points (the whole square)

#only includes the points I want (the lower triangular in M x C space)
C_points = C_temp[M_temp + C_temp <=1]
M_points = M_temp[M_temp + C_temp <=1]
T_points = 1 - C_points - M_points
len(C_points)

%pylab inline

#plot only the parts of the mesh grid that I want to use as initial conditions
plt.scatter(C_points, M_points)

#run the model
from scipy.integrate import solve_ivp

#initialize a list for res #res.y.T.shape to figure out dimensions
#res_output = [] #res_output = [None] * 5000 if want to modify elements of a list out of order
#initialize an M array and a C array - array = np.zeros((nrows, ncols))
M_array = np.zeros((50,209)) #,4949 when x_coords = np.arange(0.01,1,0.01)
C_array = np.zeros((50,209))

for i in range(0,209):
    a_current = 0.3 #NEED TO CHANGE
    g_current = 0.3 #NEED TO CHANGE
    z_current = 0.05 #NEED TO CHANGE
    #v[0] = C, v[1] = M, v[2] = T
    #input the equations into an initial value problem solver (default is RK45)
    def rhs(s,v):
        a = a_current
        g = g_current
        z = z_current
        r = 0.55 # rate coral overgrow turf
        d = 0.24 # natural coral mortality rate
        y = 0.77 #rate macroalgae overgrow turf
        return [r*v[0]*v[2] + z*r*v[2] - d*v[0] - a*v[0]*v[1], a*v[0]*v[1] - (g*v[1])/(v[1]+v[2]) + y*v[1]*v[2],-r*v[0]*v[2] - z*r*v[2] + d*v[0] + (g*v[1])/(v[1]+v[2]) - y*v[1]*v[2]]
    res = solve_ivp(rhs, (0, 50), [C_points[i], M_points[i], T_points[i]], t_eval =np.arange(0,50,1)) #solves from t =0 -> t = 5000 and for initial values C_points, M_points, T_points
    #res_output.append(res.y.T) #appends to the end
    #instead of saving in a list, save into an M array and a C array (4949 columns in each) and then should just be able to plot like [14]
    M_array[:,i] = res.y.T[:,1]
    C_array[:,i] = res.y.T[:,0] #[0:50,2]

#testblock
len(C_points)
#M_array = np.zeros((21,4949))
#C_array = np.zeros((21,4949))
#print(len(C_array[:,1]))
#print(len(res.y.T[:,1]))
i

#viewing output
#res_output[0] #res_output[0][:,2] to just view the third column of data
res.y.T[:,1]
#res.y.T

#plot one output, through time
plt.plot(res.t, res.y.T) #green is turf, guessing the orange is macroalgae since the grazing is low? idk

#plot all outputs, M vs C
#plt.plot(res.y.T[:,0], res.y.T[:,1])
plt.plot(M_array, C_array, color = 'blue')

#plot all outputs, M vs C with equilibria above
#a = 0.3, g = 0.3, z = 0.05
#equilibria (C*,M*): (0.59734069,0) [stable], (0.357181934,0.17588241)[saddle],(0.056338804,0.552740329) [stable]
M_equiz = [0,0.17588241,0.552740329]
C_equiz = [0.59734069,0.357181934,0.056338804]
clrs = ['black','purple','black']
plt.plot(M_array, C_array, color = 'blue')
plt.scatter(M_equiz, C_equiz, color = clrs)
