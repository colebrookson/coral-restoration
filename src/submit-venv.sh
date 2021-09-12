#!/bin/bash
#SBATCH --acount=def-brookson
#SBATCH --mem-per-cpu


module load python/3.9.6
virtualenv --no-download $SLURM_TMPDIR/coral-restoration
source $SLURM_TMPDIR/coral-restoration/bin/activate
pip install --no-index --upgrade pip

pip install --no-index -r requirements.txt 
