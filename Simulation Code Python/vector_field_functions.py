###########################
###########################
## This code creates a series of trajectories for the basins of attraction
## to create plots of the basin of attraction itself
###########################
###########################
## date: 2021-03-30
## author: Cole B. Brookson, Ariel G. Greiner
###########################
###########################

## set-up ======================================================================

import numpy as np
import scipy.integrate as spi
import pandas as pd
import matplotlib.pyplot as plt
import decimal
from scipy.integrate import solve_ivp
import os


def creat_prereq_objects(a_current, z_current, g_current):
    #x and y coords of all of the initial starting points
    x_coords = np.arange(0.01,1,0.05)
    #this makes a mesh grid at those points (the whole square)
    M_temp, C_temp = np.meshgrid(x_coords, x_coords)

    #only includes the points I want (the lower triangular in M x C space)
    C_points = C_temp[M_temp + C_temp <=1]
    M_points = M_temp[M_temp + C_temp <=1]
    T_points = 1 - C_points - M_points

    # initialize array
    M_array = np.zeros((50,len(C_points)))
    C_array = np.zeros((50,len(C_points)))

    for i in range(0,len(C_points)):
        a_run = a_current
        g_run = g_current
        z_run = z_current
        #v[0] = C, v[1] = M, v[2] = T
        #input the equations into  initial value problem solver (default is RK45)
        def rhs(s,v):
            a = a_run
            g = g_run
            z = z_run
            r = 0.55 # rate coral overgrow turf
            d = 0.24 # natural coral mortality rate
            y = 0.77 #rate macroalgae overgrow turf
            return [r*v[0]*v[2] + z*r*v[2] - d*v[0] - a*v[0]*v[1], a*v[0]*v[1] -
            (g*v[1])/(v[1]+v[2]) + y*v[1]*v[2],-r*v[0]*v[2] - z*r*v[2] + d*v[0]+
            (g*v[1])/(v[1]+v[2]) - y*v[1]*v[2]]
        res = solve_ivp(rhs, (0, 50),
        [C_points[i], M_points[i], T_points[i]],
        t_eval =np.arange(0,50,1)) #solves from t =0 -> t = 5000 and for initial
        #values C_points, M_points, T_points
        #res_output.append(res.y.T) #appends to the end
        #instead of saving in a list, save into an M array and a C array (4949
        #columns in each) and then should just be able to plot like [14]
        M_array[:,i] = res.y.T[:,1]
        C_array[:,i] = res.y.T[:,0] #[0:50,2]
    with np.printoptions(threshold=np.inf):
        print(C_array)

    # define matrix for the data
    rows = 20*combos
    cols = 7
    basins_init = np.ones(shape = (rows, cols)) #initialize as ones

    #name the columns
    dtype_float64 = [np.float64]*9
    basins_dt = {'names':['r','d','y','a','g','z','equil_id','size','numNA'], \
    'formats':dtype_float64}
    basins = np.ones(rows, dtype = basins_dt) #now make the real df

    #### populate matrix with data
    basins['r'] = r_vec
    basins['d'] = d_vec
    basins['y'] = y_vec
    basins['a'] = a_vec
    basins['g'] = g_vec
    basins['z'] = z_vec
    basins['equil_id'] = equil_id
    basins['size'] = 0
    basins['numNA'] = -1

    # make initial conditions
    def drange(x, y, jump):
      while x < y:
        yield float(x)
        x += decimal.Decimal(jump)
    times = list(drange(0,2000.1, 0.1))
    num_points = len(times)
    num_trajectory = len(M_points)
    init_M = M_points
    init_C = C_points
    init_T = T_points

    # make matrices for the trajectories and the basinofattractionID
    run = list(range(1,num_trajectory+1))*num_points
    M = [2]*len(run)
    C = [2]*len(run)
    T = [2]*len(run)
    time_step = list(range(1,num_points+1))*210

    trajectories_init =  np.ones(shape = (len(run), 5))

    # make the dt object so we can use column names
    trajectories_dt = {'names':['run', 'M', 'C', 'T', 'time_step'], \
    'formats':[np.float64, np.float64, np.float64, np.float64, np.float64]}
    trajectories = np.ones(len(run), dtype = trajectories_dt)
    trajectories['run'] = run
    trajectories['M'] = M
    trajectories['C'] = C
    trajectories['T'] = T
    trajectories['time_step'] = time_step

    basinofattraction_id_dt = {'names': ['init_cond', 'equilibrium', \
                                        'init_M', 'init_C', 'init_T'], \
            'formats':[np.float64, np.float64, np.float64, np.float64, np.float64]}
    basinofattraction_id = np.ones(num_trajectory, \
                            dtype = basinofattraction_id_dt)
    basinofattraction_id['init_cond'] = range(1,num_trajectory+1)
    basinofattraction_id['equilibrium'] = [0]*(num_trajectory)
    basinofattraction_id['init_M'] = init_M[0:num_trajectory]
    basinofattraction_id['init_C'] = init_C[0:num_trajectory]
    basinofattraction_id['init_T'] = init_T[0:num_trajectory]
    basinofattraction_id = pd.DataFrame(basinofattraction_id)
    # read in data into pre-formatted array
    ordered_param_data = pd.read_csv("C:/Users/brookson/Documents/Github/"
                                        "Coral-Resotration-Modeling/data/"
                                        "intermediate-files/"
                                        "all_parameters_ordered.csv")
