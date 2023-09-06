#!/bin/bash
for i in {1..4000..31}
do 
    j=$((${i} + 30))
    sbatch /home/brookson/scratch/coral-restoration/R/cc/job-files/run-simulation-${i}-${j}.sh
done
sbatch /home/brookson/scratch/coral-restoration/R/cc/job-files/run-simulation-4000-4000.sh