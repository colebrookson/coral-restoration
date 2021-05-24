# %%
import numpy as np
import scipy.integrate as spi
from scipy.integrate import solve_ivp

# %%
x_coords = np.arange(0.01,1,0.05)
#this makes a mesh grid at those points (the whole square)
M_temp, C_temp = np.meshgrid(x_coords, x_coords)

#only includes the points I want (the lower triangular in M x C space)
C_points = C_temp[M_temp + C_temp <=1]
M_points = M_temp[M_temp + C_temp <=1]
T_points = 1 - C_points - M_points
# initialize array
M_array = np.zeros((5000,len(C_points)))
C_array = np.zeros((5000,len(C_points)))

# %%
for i in range(0,len(C_points)):
    def rhs(s,v):
        a = 0.25
        g = 0.3
        z = 0.05
        r = 0.55 
        d = 0.24 
        y = 0.77 
        return [r*v[0]*v[2] + z*r*v[2] - d*v[0] - a*v[0]*v[1], a*v[0]*v[1] -
        (g*v[1])/(v[1]+v[2]) + y*v[1]*v[2],-r*v[0]*v[2] - z*r*v[2] + d*v[0]+
        (g*v[1])/(v[1]+v[2]) - y*v[1]*v[2]]

    res = solve_ivp(rhs, (0, 5000),
    [C_points[i], M_points[i], T_points[i]],
    t_eval =np.arange(0,5000,1)) #solves from t =0 -> t = 50000
    M_array[:,i] = res.y.T[:,1]
    C_array[:,i] = res.y.T[:,0] #[0:50,2]

trajectories = np.ones(len(C_points)*len(M_array[:,i]),\
    dtype = {'names': ['M', 'C'], 'formats':[np.float64, np.float64]})
trajectories['M'] = M_array.flatten()
trajectories['C'] = C_array.flatten()

print(trajectories['M'][1049900:1049999])
