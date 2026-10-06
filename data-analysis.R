# Replication of Gan et al. (2024), Table 1 -- Los Angeles homeless dataset

# Replication of Gan et al. (2024), Table 1 -- Los Angeles homeless dataset

library(ipd)
library(MASS)
library(caret)
library(readr)
library(dplyr)
library(randomForest)

source("utils/azriel_et_al_2022_code.R")
source("utils/song_et_al_2024_code/semi_supervised_methods.R")
source("utils/song_et_al_2024_code/SupervisedEstimation.R")
source("utils/ML-Assisted-Inference/Scripts/method_functions.R")
source("utils/table_functions.R")

# 1. Load Data
census_data <- readr::read_csv("utils/song_et_al_2024_code/WorkingExampleMatrix.csv")

# 2. Partition Data according to Table counts
data_hot <- census_data %>% filter(selection == 2)
data_dl  <- census_data %>% filter(selection == 1)
data_du  <- census_data %>% filter(selection == 0 | is.na(StTotal))

predictors <- c("Perc.Vacant", "Perc.Minority")

# Check to ensure that all data is complete
stopifnot(
  !anyNA(data_dl[,  c("StTotal", predictors)]),
  !anyNA(data_du[,  predictors]),
  !anyNA(data_hot[, c("StTotal", predictors)])
)

# 3. Train Predictive Random Forest on the 244 Hot Tracts
set.seed(20220122)
rf_hot <- randomForest(StTotal ~ Perc.Vacant + Perc.Minority, data = data_hot)

# 4. Generate Predictions & Design Matrices for the ipd package
# (these DO take an intercept column):
X_l <- cbind(1, as.matrix(data_dl[, predictors]))
X_u <- cbind(1, as.matrix(data_du[, predictors]))
Y_l <- matrix(data_dl$StTotal, ncol = 1)

pred_l <- matrix(predict(rf_hot, newdata = data_dl), ncol = 1)   # mu(x)
pred_u <- matrix(predict(rf_hot, newdata = data_du), ncol = 1)

stopifnot(nrow(pred_l) == nrow(X_l), nrow(pred_u) == nrow(X_u))

# For PI_se() / PSSE()
# (matrix, response first, no intercept column)
labelled_mat   <- as.matrix(data.frame(StTotal = data_dl$StTotal,
                                       data_dl[, predictors]))
unlabelled_mat <- as.matrix(data_du[, predictors])

# For pb_estimation()
# (one stacked data frame: labeled rows marked set == "testing",
#  predictions in a column named `pred`; hot tracts excluded)
dat_pb <- dplyr::bind_rows(
  data_dl %>% mutate(set = "testing",   pred = as.vector(pred_l)),
  data_du %>% mutate(set = "unlabeled", pred = as.vector(pred_u))
) %>%
  dplyr::select(StTotal, all_of(predictors), pred, set) %>%
  as.data.frame()

# 5. Fit Models

# Supervised Estimator (SUP) Benchmark
sup_mod  <- lm(StTotal ~ Perc.Vacant + Perc.Minority, data = data_dl)
sup_coef <- coef(sup_mod)
sup_se   <- summary(sup_mod)$coefficients[, "Std. Error"]

#  Azriel et al. (2022) PI
# intercept_se = "influence" fills in the intercept SE, which Azriel et al.'s
# eq. (28) does not cover. Necessary for replication.
azriel_fit <- PI_se(labelled_mat, unlabelled_mat, intercept_se = "influence")
azriel_est <- azriel_fit$Hattheta
azriel_se  <- azriel_fit$se

#  PDC, PPI++, PPI, PSPA
fit_pdc   <- ipd::pdc_ols(X_l = X_l, Y_l = Y_l, f_l = pred_l,
                          X_u = X_u, f_u = pred_u)

fit_ppipp <- ipd::ppi_plusplus_ols(X_l = X_l, Y_l = Y_l, f_l = pred_l,
                                   X_u = X_u, f_u = pred_u)

fit_ppi   <- ipd::ppi_ols(X_l = X_l, Y_l = Y_l, f_l = pred_l,
                          X_u = X_u, f_u = pred_u)

pspa_fit  <- ipd::pspa_ols(X_l = X_l, Y_l = Y_l, f_l = pred_l,
                           X_u = X_u, f_u = pred_u)

# Song et al. (2024) PSSE
song_fit <- PSSE(labelled_data   = labelled_mat,
                 unlabelled_data = unlabelled_mat,
                 type = "linear", sd = TRUE)

song_est <- song_fit$Hattheta
song_se  <- song_fit$sd.of.hattheta

#  Chen-Chen, PPI and PDC via pb_estimation()
# family must be a string ("gaussian"), not gaussian().
pb_formula <- StTotal ~ Perc.Vacant + Perc.Minority
pb_types   <- c("chen-chen", "ppi", "pdc","ppi_plusplus")

fit_pb <- lapply(pb_types, function(type) {
  pb_estimation(dat_tv = dat_pb, formula = pb_formula,
                family = "gaussian", est_type = type, alpha = 0.05)
})
names(fit_pb) <- pb_types

# 6. Summary Table
table_predictors <- c("Intercept", "Perc. vacant", "Perc. minority")

methods <- list(
  SUP         = list(est = sup_coef,      se = sup_se),
  Azriel      = list(est = azriel_est,    se = azriel_se),
  PDC         = list(est = fit_pdc$est,   se = fit_pdc$se),
  `PPI++`     = list(est = fit_ppipp$est, se = fit_ppipp$se),
  PPI         = list(est = fit_ppi$est,   se = fit_ppi$se),
  PSPA        = list(est = pspa_fit$est,  se = pspa_fit$se),
  Song        = list(est = song_est,      se = song_se),
  `Chen-Chen (EE)` = list(est = fit_pb[["chen-chen"]]$Estimate,
                          se  = fit_pb[["chen-chen"]]$Std.Error),
  `PPI (EE)`  = list(est = fit_pb[["ppi"]]$Estimate,
                     se  = fit_pb[["ppi"]]$Std.Error),
  `PPI++ (EE)`  = list(est = fit_pb[["ppi_plusplus"]]$Estimate,
                       se  = fit_pb[["ppi_plusplus"]]$Std.Error),
  `PDC (EE)`  = list(est = fit_pb[["pdc"]]$Estimate,
                     se  = fit_pb[["pdc"]]$Std.Error)
)

results <- build_results(methods, table_predictors,
                         baseline = "SUP", reference = "PDC")



latex_results(results, reference = "PDC", file = "tables/data-analysis.tex")  # \input{} this