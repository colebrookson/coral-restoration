#!/bin/bash
for i in {1..4000..31}
do
    if [ "${i}" -lt "3999" ] 
    then
        j=$(($i + 30))
    fi
    if [ "${i}" -eq "4000" ]
    then 
        j=$(($i + 0))
    fi
    var='$EBVERSIONR'
    echo "#!/bin/bash

#SBATCH --mail-user=cole.brookson@gmail.com
#SBATCH --account=def-bat3man
#SBATCH --mail-type=BEGIN 
#SBATCH --mail-type=END 
#SBATCH --mail-type=FAIL 
#SBATCH --mail-type=REQUEUE 
#SBATCH --job-name='sims ${i} to ${j}'
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --mem-per-cpu=3GB
#SBATCH --time=0-01:00:00

module load StdEnv/2020 gcc/9.3.0 openmpi/4.0.3 glost/0.3.1 r/4.2.1

export R_LIBS=/home/brookson/scratch/.local/R/$var/

srun glost_launch /home/brookson/scratch/coral-restoration/R/cc/job-files/simulation-tasks-${i}-${j}.txt" >> ~/Github/coral-restoration/R/cc/job-files//run-simulation-${i}-${j}.sh 
done 