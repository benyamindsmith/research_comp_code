#!/bin/bash

#SBATCH -p cpu
#SBATCH -o logs/killarney_output_logistic.out
#SBATCH --mem=120000
#SBATCH -t 1-00:00:00
#SBATCH -J simulations

module load StdEnv/2023
module load r/4.6.1

R CMD BATCH --quiet --no-restore --no-save "logistic-1.R" "./logs/killarney_output_logistic1.Rout"
R CMD BATCH --quiet --no-restore --no-save "logistic-2.R" "./logs/killarney_output_logistic2.Rout"
R CMD BATCH --quiet --no-restore --no-save "logistic-3.R" "./logs/killarney_output_logistic3.Rout"


