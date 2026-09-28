#!/bin/bash
#SBATCH -p cpu
#SBATCH -J reg_simulations
#SBATCH -t 1-00:00:00
#SBATCH --mem=120000
#SBATCH --cpus-per-task=1
#SBATCH --array=0-8
#SBATCH -o logs/%x_%A_%a.out


module load StdEnv/2023
module load r/4.6.1

SCRIPTS=(
  "linear-regression-1.R"
  "linear-regression-2.R"
  "linear-regression-3.R"
  )

TARGET_SCRIPT="${SCRIPTS[$SLURM_ARRAY_TASK_ID]}"

BASENAME="$(basename "$TARGET_SCRIPT" .R)"
OUTPUT_LOG="./logs/killarney_output_${BASENAME}.Rout"

echo "Task $SLURM_ARRAY_TASK_ID executing: $TARGET_SCRIPT"

R CMD BATCH --quiet --no-restore --no-save "$TARGET_SCRIPT" "$OUTPUT_LOG"