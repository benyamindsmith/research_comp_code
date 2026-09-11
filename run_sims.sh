#!/bin/bash

#SBATCH -p cpu
#SBATCH -o logs/killarney_output.out
#SBATCH --mem=120000
#SBATCH -t 48:00:00
#SBATCH -J simulations

module load StdEnv/2023
module load r/4.6.1

R CMD BATCH --quiet --no-restore --no-save "mean-estimation-1.R" "./logs/killarney_output_mean_estimation1.Rout"
R CMD BATCH --quiet --no-restore --no-save "mean-estimation-2.R" "./logs/killarney_output_mean_estmation2.Rout"
R CMD BATCH --quiet --no-restore --no-save "mean-estimation-3.R" "./logs/killarney_output_mean_estimation3.Rout"
R CMD BATCH --quiet --no-restore --no-save "linear-regression-1.R" "./logs/killarney_output_linear_regression1.Rout"
R CMD BATCH --quiet --no-restore --no-save "linear-regression-2.R" "./logs/killarney_output_linear_regression2.Rout"
R CMD BATCH --quiet --no-restore --no-save "linear-regression-3.R" "./logs/killarney_output_linear_regression3.Rout"
R CMD BATCH --quiet --no-restore --no-save "logistic-1.R" "./logs/killarney_output_logistic_1.Rout"
R CMD BATCH --quiet --no-restore --no-save "logistic-2.R" "./logs/killarney_output_logistic_2.Rout"
R CMD BATCH --quiet --no-restore --no-save "logistic-3.R" "./logs/killarney_output_logistic_3.Rout"

