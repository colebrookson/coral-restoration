###########################
###########################
## This script installs python and the packages needed for analysis, and was 
## written specifically for use on Compute Canada servers
###########################
###########################
## date: 2021-09-12
## author: Cole B. Brookson, Ariel G. Greiner
###########################
###########################

# load python itself
module load python/3.9.6

# load SciPy stack
module load scipy-stack

# now create a new project environment
virtualenv --no-download coral-restoration

# activate the virtual environment 
source coral-restoration/bin/activate

# upgrade pip in the environment
pip install --no-index --upgrade pip
