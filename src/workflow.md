Steps to solve equations
1. Solve equations (find alternative to MATLAB)
2. manipulate matlab output to make readable
3. read into python,  do work there
4. visualize results 


In Compute Canada
- start by creating a virtual environment 
- use `requirements.txt` to standardize package versions etc
- use a `submit-venv.sh` to create the same environment on each node for the job 
- 

# Where different parts of the analysis come from 

## Equilibria information

Right now, the equilibria information come from a Matlab script that exists but is not built into this pipeline. That would be a nice-to-have, but would require some tinkering to figure out. Probably possible to reporduce in Julia, but not immediately sure how. 

That script produces `./data/parameter-data/all-parameter-vals-equilibria-unordered.csv`, which gets incorporated to the `_targets.R` file. This is where the pipeline is "managed" from. 