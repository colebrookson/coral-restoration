#!/bin/bash

#SBATCH --mail-user=cole.brookson@gmail.com
#SBATCH --account=def-bat3man
#SBATCH --mail-type=BEGIN 
#SBATCH --mail-type=END 
#SBATCH --mail-type=FAIL 
#SBATCH --mail-type=REQUEUE 
#SBATCH --job-name='sims 3101 to 3131'
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --mem-per-cpu=3GB
#SBATCH --time=0-01:00:00

module load StdEnv/2020 gcc/9.3.0 openmpi/4.0.3 glost/0.3.1 r/4.2.1

export R_LIBS=/home/brookson/scratch/.local/R/$EBVERSIONR/

srun glost_launch /home/brookson/scratch/coral-restoration/R/cc/job-files/simulation-tasks-3101-3131.txt
