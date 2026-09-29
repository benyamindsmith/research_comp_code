# Simulation Code for "Post-Prediction Inference under Model Misspecification"

This repository contains the replication codes for the report "Post-Prediction Inference under Model Misspecification" by Benjamin Smith. 

To ensure a smooth replication experience, please clone the repository and install the required packages listed below:

```r
# If you have not installed the 'pak' package, run
# install.packages("pak")

pak::pkg_install(
  c(
    "foreach",
    "doParallel",
    "dplyr",
    "readr",
    "ipd",
    "MASS",
    "mvnfast",
    "caret",
    "randomForest",
    "quantreg",
    "ggplot2",
    "patchwork"
  )
)
```

For best results, ensure you are running `R` version 4.6.1. 

# Simulation Study

To run the simulation studies locally, run the following commands in `R` after cloning this repository:

```r
source("1-run-sims.R")
```

To run simulation studies on a SLURM cluster, run the following command in the terminal on your login node after cloning this repository:

```sh
sbatch run_sims.sh
```

To post-process the data for Figures 1-6, run in `R`:

```r
source(2-load-transform.R)
source(3-visuals.R)
```

# Data Analysis

To run the data analysis and replicate the results from Table 1, run in `R`:

```r
source("data-analysis.R")
```