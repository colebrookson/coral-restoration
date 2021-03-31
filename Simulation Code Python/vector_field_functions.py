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
