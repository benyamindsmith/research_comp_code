#!/bin/bash
#SBATCH -p cpu
#SBATCH -J simulations
#SBATCH -t 1-00:00:00
#SBATCH --mem=120000
#SBATCH --cpus-per-task=1
#SBATCH --array=0-8
#SBATCH -o logs/%x_%A_%a.out

# Load cluster environment and R
module load StdEnv/2023
module load r/4.6.1

# Define all 9 scripts in an indexed array
SCRIPTS=(
  "logistic-1.R"
  "logistic-2.R"
  "logistic-3.R"
  "mean-estimation-1.R"
  "mean-estimation-2.R"
  "mean-estimation-3.R"
  "linear-regression-1.R"
  "linear-regression-2.R"
  "linear-regression-3.R"
)

# Select the target script for this specific array task
TARGET_SCRIPT="${SCRIPTS[$SLURM_ARRAY_TASK_ID]}"

# Create a clean log name by stripping the '.R' extension
BASENAME="$(basename "$TARGET_SCRIPT" .R)"
OUTPUT_LOG="./logs/killarney_output_${BASENAME}.Rout"

echo "Task $SLURM_ARRAY_TASK_ID executing: $TARGET_SCRIPT"

# Run the R script
R CMD BATCH --quiet --no-restore --no-save "$TARGET_SCRIPT" "$OUTPUT_LOG"