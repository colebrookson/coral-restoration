###########################
###########################
## This code creates a series of trajectories for the basins of attraction
## to create plots of the basin of attraction itself
###########################
###########################
## date: 2021-02-04
## author: Cole B. Brookson, Ariel G. Greiner
###########################
###########################

## set-up ======================================================================

import numpy as np
import scipy.integrate as spi
import pandas as pd
import math
import matplotlib.pyplot as plt
import decimal
from scipy.integrate import solve_ivp
import os
from vector_field_functions import create_prereq_objects
from vector_field_functions import basin_finder

# get relative path
path = ("C:/Users/brookson/Documents/Github/"
        "Coral-Restoration-Modeling/data/all_parameters_ordered.csv")
start = "Users / brookson / Documents / Github /"
relative_path = os.path.relpath(path, start)

# get all unique parameter combinations to loop through
z = [0, 0.05, 0.25, 0.5]
a = np.arange(0,1.01,0.01)
g = np.arange(0,0.51,0.01)

# read in data in
ordered_param_data = pd.read_csv("C:/Users/brookson/Documents/Github/"
                                        "Coral-Resotration-Modeling/data/"
                                        "intermediate-files/"
                                        "all_parameters_ordered.csv")
a_essential = [0.05, 0.3, 0.5, 0.99]
g_essential = [0.05, 0.21, 0.5]
z_essential = [0, 0.05, 0.25, 0.5]
essential_list = [[i, j, k] for i in a_essential
                            for j in g_essential
                            for k in z_essential]
param_combo = essential_list[0]
for param_combo in essential_list:
    a_current = param_combo[0]
    g_current = param_combo[1]
    z_current = param_combo[2]

    times = np.linspace(start = 0, stop = 2000, num = 20000)
    # get objects
    prereq_obs = create_prereq_objects(a_current = a_current, \
                                       z_current = g_current, \
                                       g_current = z_current)

    basin_output = basin_finder(grazing_level = g_current,\
                               recruit_level = z_current, \
                               competition_level = a_current, \
                               ordered_param_data = ordered_param_data, \
                               basinofattraction_id = prereq_obs[0], \
                               basins = prereq_obs[1], \
                               trajectories = prereq_obs[2], \
                               num_trajectory = prereq_obs[3], \
                               radius = 0.005, \
                               times = times,\
                               final_time =  math.floor(len(times)*0.1))



# run actual model =============================================================
output_test = basin_finder(grazing_level = g_current,\
                           recruit_level = z_current, \
                           competition_level = a_current, \
                           ordered_param_data = ordered_param_data, \
                           basinofattraction_id = prereq_obs[0], \
                           basins = prereq_obs[1], \
                           num_trajectory = prereq_obs[2], \
                           radius = 0.005, \
                           times = np.linspace(start = 0, stop = 2000, \
                                               num = 20000),\
                           final_time =  math.floor(len(times)*0.1))
output_basinofattraction = pd.DataFrame(output_test[0])
file_name = print("C:/Users/brookson/Documents/Github/"
                    "Coral-Resotration-Modeling/data/"
                    "intermediate-files/basins_ouput/"
                    "output_basinofattraction_a",a_current,'_g',g_current,'_z',\
                  z_current, '.csv')
output_basinofattraction.to_csv("C:/Users/brookson/Documents/Github/Coral-Resotration-Modeling/data/intermediate-files/basins_output/output_basinofattraction_a0.3_g0.3_z0.05.csv")
output_basins = pd.DataFrame(output_test[1])
output_basins.to_csv("C:/Users/brookson/Documents/Github/Coral-Resotration-Modeling/data/intermediate-files/basins_output/output_basins_a0.3_g0.3_z0.05.csv")


# make figures =================================================================
groups = output_basinofattraction.groupby('equilibrium')
for name, group in groups:
    plt.plot(group.init_M, group.init_C, marker = 'o', linestyle = '',\
             label = name)
plt.legend()

plt.scatter(output_basinofattraction['init_M'], \
            output_basinofattraction['init_C'], \
            c = output_basinofattraction['equilibrium'])
