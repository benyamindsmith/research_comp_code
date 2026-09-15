#!/bin/bash

#SBATCH -p cpu
#SBATCH -o logs/killarney_output_mean.out
#SBATCH --mem=120000
#SBATCH -t 1-00:00:00
#SBATCH -J simulations

module load StdEnv/2023
module load r/4.6.1

R CMD BATCH --quiet --no-restore --no-save "mean-estimation-1.R" "./logs/killarney_output_mean_estimation1.Rout"
R CMD BATCH --quiet --no-restore --no-save "mean-estimation-2.R" "./logs/killarney_output_mean_estimation2.Rout"
R CMD BATCH --quiet --no-restore --no-save "mean-estimation-3.R" "./logs/killarney_output_mean_estimation3.Rout"


