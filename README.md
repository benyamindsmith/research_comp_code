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
    "ggplot2"
    "ipd",
    "jlgrons/stratifiedSSL",
    "caret",
    "MASS",
    "mvnfast"
  )
)
```

TODO:
- DELETE ALL LOGISTIC CODE