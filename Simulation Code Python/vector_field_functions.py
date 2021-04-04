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

# set-up =======================================================================

import numpy as np
import scipy.integrate as spi
import pandas as pd
import math
import matplotlib.pyplot as plt
import decimal
from scipy.integrate import solve_ivp
import os

# make function to create objects needed for basin calculations ================

def create_prereq_objects(a_current, z_current, g_current):

    # all (51*101*4) of parameters
    combos = (51*101*4)
    # rate coral overgrow turf
    r_vec = [0.55]*(20*(51*101*4))

    # rate of natural coral mortality
    d_vec = [0.24]*(20*(51*101*4))

    # rate a unit macroalgae overgrows a unit of turf algae
    y_vec = [0.77]*(20*(51*101*4))

    # rate coral larvae recruit and overgrow turf algae
    z = [0, 0.05, 0.25, 0.5]
    z_vec = []
    for i in z:
        z_vec_temp = [i]*(20*int((51*101*4)/4))
        z_vec.extend(z_vec_temp)

    # rate macroalgae overgrow coral
    a = np.arange(0,1.01,0.01)
    a_vec = []
    for i in range(0, len(z)):
        a_vec_temp_1 = []
        for j in a:
            a_vec_temp_2 = [j]*20
            a_vec_temp_2 = a_vec_temp_2*51
            a_vec_temp_1.extend(a_vec_temp_2)
        a_vec.extend(a_vec_temp_1)

    # rate of grazing on macroalgae and turf algae
    g = np.arange(0,0.51,0.01)
    g_vec = []
    for i in range(0, int((51*101*4)/len(g))):
        g_vec_temp_1 = []
        for j in g:
            g_vec_temp_2 = [j]*(20)
            g_vec_temp_1.extend(g_vec_temp_2)
        g_vec.extend(g_vec_temp_1)

    # equil id is just 20 times the length of the (51*101*4)
    equil_id = list(range(1,21))
    equil_id = equil_id*(51*101*4)

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
    #with np.printoptions(threshold=np.inf):
        #print(C_array)

    # define matrix for the data
    rows = 20*(51*101*4)
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
    basins = pd.DataFrame(basins)
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


    output = [basinofattraction_id, basins, trajectories, num_trajectory]
    return output

# make function to get all basins of attraction values =========================

def basin_finder(grazing_level, recruit_level, competition_level, \
                 ordered_param_data, basinofattraction_id, basins, \
                 trajectories, num_trajectory, radius, times, final_time):

    """ This function takes in a series of parameters and finds whether or not
    a particular basin of attraction is stable?

    Parameters:
        grazing_level (float): the parameter value for grazing (g)
        recruit_level (float): the parameterfor recruitment level (z)
        competition_level (float): the parameter value for competition level (a)
        ordered_param_data (np.array): data holding all of the ordered
            equilibria across the different parameter values. Read in via
            loadtext() above
        basinofattraction_id (np.array): array holding initial conditions for
            the different equilibria values.
        basins ():
        num_trajectory:
        radius:
        times:
        final_time:

    """

    print("In BOA, grazing level (g) = ", grazing_level, ", competition level ",
    "(a) = ",competition_level,", and recruitment level (z) = ", recruit_level)

    # number of stable equilibria at that parameter combo
    grazing_level = 0
    competition_level = 0
    recruit_level = 0
    radius = 0.005
    times = np.linspace(start = 0, stop = 2000, num = 20000)
    final_time = math.floor(len(times)*0.1)
    shape = (ordered_param_data['g'] == grazing_level) & \
            (ordered_param_data['a'] == competition_level) & \
            (ordered_param_data['z'] == recruit_level)
    ordered_param_shape = ordered_param_data[shape]
    stable_ordered_param = ordered_param_shape[ordered_param_shape['stability']\
                                                == "stable_node"]
    num_eq = len(stable_ordered_param)

    # get coordinates of the stable equilibria at that parameter combo
    m_equi = stable_ordered_param['M'].tolist()
    c_equi = stable_ordered_param['C'].tolist()
    print("m coordinate is = ", m_equi, " and c coordinate is = ", c_equi)

    # loop through all num_trajectories for each stable equilibrium
    i = 1
    while i <= num_trajectory:
        j = 0
        while j <= (num_eq-1):
            # set up conditions to deal with trajectories shape
            time_diff = (len(times) - final_time)
            traj_shape = (trajectories['run'] == i) & \
                         (trajectories['time_step'] > time_diff)
            traj_j = trajectories[traj_shape]
            if len(np.unique(traj_j['M'])) > 1:
                sys.exit("more than one unique value for traj_j['M']")
            # set up data to get the appropriate part
            assign_shape = (stable_ordered_param['M'] == m_equi[j]) & \
                           (stable_ordered_param['C'] == c_equi[j])

            # get basins shape
            basins_shape = (basins['g'] == grazing_level) & \
                           (basins['a'] == competition_level) & \
                           (basins['z'] == recruit_level) & \
                           (basins['equil_id'] == \
                            stable_ordered_param[assign_shape]['ID'].tolist())

            # big if statement to test if the trajectory stays within radius
            if ((m_equi[j] - radius) < np.unique(traj_j['M'])[0]) and \
            ((m_equi[j] + radius) < np.unique(traj_j['M'])[0]) and \
            ((c_equi[j] - radius) < np.unique(traj_j['C'])[0]) and \
            ((c_equi[j] + radius) < np.unique(traj_j['C'])[0]):
                print('yes')
                basinofattraction_id.loc[basinofattraction_id.init_cond == i, \
                                        'equilibrium']= \
                    np.unique(stable_ordered_param[assign_shape]['ID'])[0]
                equil_id_iter = \
                    stable_ordered_param[assign_shape]['ID'].tolist()[0]
                basins.loc[(basins['equil_id'] == equil_id_iter) & \
                           (basins['g'] == grazing_level) & \
                           (basins['a'] == competition_level) & \
                           (basins['z'] == recruit_level), \
                           'size'] += 1
            else:
                print('no')
            j=j+1
        i = i+1
        print(i,j)
        #print('Current i is', i)
    output = [basinofattraction_id, basins]
    return output
