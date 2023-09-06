#!/bin/bash
for i in {1..4000..31}
do
    if [ "${i}" -lt "3999" ] 
    then
        var=$(($i + 30))
    fi
    if [ "${i}" -eq "4000" ]
    then 
        var=$(($i + 0))
    fi
    for ((j=$i; j<=$var; j++))
    do
        if [ "${j}" -lt "$var" ] # this is because the last line can't be empty for the GLOST to work properly apparently
        then
            envname='${nargument}'
            echo "nargument=${j} && Rscript /home/brookson/scratch/coral-restoration/R/cc/01_simulation.R ${envname}" >> ~/Github/coral-restoration/R/cc/job-files/simulation-tasks-${i}-${var}.txt
        fi
        if [ "${j}" -eq "$var" ]
        then
            envname='${nargument}'
            echo -n "nargument=${j} && Rscript /home/brookson/scratch/coral-restoration/R/cc/01_simulation.R ${envname}" >> ~/Github/coral-restoration/R/cc/job-files/simulation-tasks-${i}-${var}.txt 
        fi
    done
done