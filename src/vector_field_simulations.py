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
import time
from vector_field_functions import create_prereq_objects
from vector_field_functions import basin_finder

# get all unique parameter combinations to loop through
z = [0, 0.05, 0.25, 0.5]
a = np.arange(0,1.01,0.01)
g = np.arange(0,0.51,0.01)

# read in data in
ordered_param_data = pd.read_csv("C:/Users/brookson/Documents/Github/"
                                        "Coral-Resotration-Modeling/data/"
                                        "intermediate-files/"
                                        "all_parameters_ordered.csv")
# make lists of first values to loop through (i.e. essential demo values)
a_essential = [0.05, 0.3, 0.5, 0.99]
g_essential = [0.05, 0.21, 0.5]
z_essential = [0, 0.05, 0.25, 0.5]
# make all unique combos of demo values
essential_list = [[i, j, k] for i in a_essential
                            for j in g_essential
                            for k in z_essential]
start = time.time()
for param_combo in essential_list:
    # set a, z, and g parameters
    a_current = 0.05#param_combo[0]
    g_current = 0.21#param_combo[1]
    z_current = 0#param_combo[2]
    # make amount of time for stability calculation
    times = np.linspace(start = 0, stop = 2000, num = 20000)
    # get objects from function in other script
    prereq_obs = create_prereq_objects(a_current = a_current, \
                                       z_current = z_current, \
                                       g_current = g_current)
    # run actual model
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
    # separate the different output objects
    output_basinofattraction = pd.DataFrame(basin_output[0])
    output_basins = pd.DataFrame(basin_output[1])
    # write output objects to file
    output_basinofattraction.to_csv("C:/Users/brookson/Documents/Github/"
                                    "Coral-Resotration-Modeling/data/"
                                    "intermediate-files/basins_output/"
                                    "basinofattraction_a%.2f_g%.2f_z%.2f.csv"\
                                    % (g_current, z_current, a_current))
    output_basins.to_csv("C:/Users/brookson/Documents/Github/"
                         "Coral-Resotration-Modeling/data/"
                         "intermediate-files/basins_output/"
                         "basins_a%.2f_g%.2f_z%.2f.csv"\
                         % (g_current, z_current, a_current))
    if len(output_basinofattraction.equilibrium.unique()) > 1:
        print('a = ', a_current, ', z = ', z_current, ', g = ', g_current, \
              'has more than one unique equilibrium value')
    # make (basic plot showing the different trajectory outcomes)
    #groups = output_basinofattraction.groupby('equilibrium')
    #for name, group in groups:
    #    plt.plot(group.init_M, group.init_C, marker = 'o', linestyle = '',\
    #             label = name)
    #    plt.xlabel('Proportion Macroalgae', fontsize = 15)
    #    plt.ylabel('Proportion Coral', fontsize = 15)
    #    plt.title('Grazing = %.2f, Recruitment = %.2f, Competition = %.2f' \
    #                % (g_current, z_current, a_current))
    #    plt.legend()
    #plt.legend()
    # save plot
    #plt.savefig("C:/Users/brookson/Documents/Github/"
    #            "Coral-Resotration-Modeling/graphs/"
    #            "basinplots/essential_combinations/"
    #            "basin_attract_g_%.2f_z_%.2f_a%.2f.png" \
    #            % (g_current, z_current, a_current))

# figure out how long loop takes
print('DONE')
elapsed_time_fl = (time.time() - start)
