library(dplyr)

# Master estimator order -- used for every figure so legends line up
estimator_levels <- c("PDC", "PDC1", "PDC2", "SEMI", "PPI", "SUP",
                      "PPI++", "SONG", "POP-Inf", "Chen-Chen")

# Raw simulation labels -> paper labels
estimator_lookup <- c(
  "Naive"  = "SUP",
  "Azriel" = "SEMI",
  "Zhang"  = "SEMI",
  "Song"   = "SONG",
  "PSPA"   = "POP-Inf",
  "Chen-Chen" = "CC"
)

relabel_estimators <- function(df, extra = character()) {
  lookup <- c(extra, estimator_lookup)   # `extra` takes precedence
  out <- df |>
    dplyr::mutate(
      Estimator = as.character(Estimator),
      Estimator = ifelse(Estimator %in% names(lookup),
                         unname(lookup[Estimator]), Estimator),
      Estimator = factor(Estimator, levels = estimator_levels)
    )
  stopifnot("Unrecognised estimator label" = !anyNA(out$Estimator))
  out
}

# Load Data
mean_estimation_1 <- readRDS("data/mean-estimation-1-results.rds")
mean_estimation_2 <- readRDS("data/mean-estimation-2-results.rds")
mean_estimation_3 <- readRDS("data/mean-estimation-3-results.rds")

linear_regression_1 <- readRDS("data/linear-regression-1-results.rds")
linear_regression_2 <- readRDS("data/linear-regression-2-results.rds")
linear_regression_3 <- readRDS("data/linear-regression-3-results.rds")

# Mean estimation: PPI++ is reported as PDC
mean_keep <- c("Naive", "PPI", "PPI++", "Song", "Zhang", "PSPA")

mean_estimation_1 <- mean_estimation_1 |>
  dplyr::filter(Estimator %in% mean_keep) |>
  relabel_estimators(extra = c("PPI++" = "PDC"))

mean_estimation_2 <- mean_estimation_2 |>
  dplyr::filter(Estimator %in% mean_keep) |>
  relabel_estimators(extra = c("PPI++" = "PDC"))

mean_estimation_3 <- mean_estimation_3 |>
  dplyr::filter(Estimator %in% mean_keep) |>
  relabel_estimators(extra = c("PPI++" = "PDC"))

# Linear regression
linear_regression_1 <- relabel_estimators(linear_regression_1)
linear_regression_2 <- relabel_estimators(linear_regression_2)
linear_regression_3 <- relabel_estimators(linear_regression_3)