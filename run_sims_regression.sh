#!/bin/bash

#SBATCH -p cpu
#SBATCH -o logs/killarney_output_regression.out
#SBATCH --mem=120000
#SBATCH -t 1-00:00:00
#SBATCH -J simulations

module load StdEnv/2023
module load r/4.6.1

R CMD BATCH --quiet --no-restore --no-save "linear-regression-1.R" "./logs/killarney_output_linear_regression1.Rout"
R CMD BATCH --quiet --no-restore --no-save "linear-regression-2.R" "./logs/killarney_output_linear_regression2.Rout"
R CMD BATCH --quiet --no-restore --no-save "linear-regression-3.R" "./logs/killarney_output_linear_regression3.Rout"

