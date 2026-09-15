library(dplyr)
# Load Data
mean_estimation_1 <- readRDS("data/mean-estimation-1-results.rds")
mean_estimation_2 <- readRDS("data/mean-estimation-2-results.rds")
mean_estimation_3 <- readRDS("data/mean-estimation-3-results.rds")

linear_regression_1 <- readRDS("data/linear-regression-1-results.rds")
linear_regression_2 <- readRDS("data/linear-regression-2-results.rds")
linear_regression_3 <- readRDS("data/linear-regression-3-results.rds")

logistic_1 <- readRDS("data/logistic-1-results.rds")
logistic_2 <- readRDS("data/logistic-2-results.rds")
logistic_3 <- readRDS("data/logistic-3-results.rds")
mean_estimation_1 <- mean_estimation_1 |>
  dplyr::filter(Estimator %in% c("Naive", "PPI", "PPI++", "Song", "Zhang", "PSPA")) |>
  dplyr::mutate(Estimator = dplyr::case_match(Estimator,
                                              "PPI++" ~ "PDC",
                                              "Naive" ~ "SUP",
                                              "Zhang" ~ "SEMI",
                                              "Song" ~ "SONG",
                                              "PSPA" ~ "POP-Inf",
                                              .default = Estimator
  ) |>
    factor(x = _, levels = c("PDC", "SEMI", "PPI", "SUP", "SONG", "POP-Inf")))

mean_estimation_2 <- mean_estimation_2 |>
  dplyr::filter(Estimator %in% c("Naive", "PPI", "PPI++", "Song", "Zhang", "PSPA")) |>
  dplyr::mutate(Estimator = dplyr::case_match(Estimator,
                                              "PPI++" ~ "PDC",
                                              "Naive" ~ "SUP",
                                              "Zhang" ~ "SEMI",
                                              "Song" ~ "SONG",
                                              "PSPA" ~ "POP-Inf",
                                              .default = Estimator
  ) |>
    factor(x = _, levels = c("PDC", "SEMI", "PPI", "SUP", "SONG", "POP-Inf")))

mean_estimation_3 <- mean_estimation_3 |>
  dplyr::filter(Estimator %in% c("Naive", "PPI", "PPI++", "Song", "Zhang", "PSPA")) |>
  dplyr::mutate(Estimator = dplyr::case_match(Estimator,
                                              "PPI++" ~ "PDC",
                                              "Naive" ~ "SUP",
                                              "Zhang" ~ "SEMI",
                                              "Song" ~ "SONG",
                                              "PSPA" ~ "POP-Inf",
                                              .default = Estimator
  ) |>
    factor(x = _, levels = c("PDC", "SEMI", "PPI", "SUP", "SONG", "POP-Inf")))

linear_regression_1 <- linear_regression_1 |>
  dplyr::mutate(Estimator = dplyr::case_match(Estimator,
                                              "Naive" ~ "SUP",
                                              "Azriel" ~ "SEMI",
                                              "Song" ~ "SONG",
                                              "PSPA" ~ "POP-Inf",
                                              .default = Estimator
  ) |>
    factor(x = _, levels = c("PDC", "SEMI", "PPI", "SUP", "PPI++", "SONG", "POP-Inf")))

linear_regression_2 <- linear_regression_2 |>
  dplyr::mutate(Estimator = dplyr::case_match(Estimator,
                                              "Naive" ~ "SUP",
                                              "Azriel" ~ "SEMI",
                                              "Song" ~ "SONG",
                                              "PSPA" ~ "POP-Inf",
                                              .default = Estimator
  ) |>
    factor(x = _, levels = c("PDC", "SEMI", "PPI", "SUP", "PPI++", "SONG", "POP-Inf")))

linear_regression_3 <- linear_regression_3 |>
  dplyr::mutate(Estimator = dplyr::case_match(Estimator,
                                              "Naive" ~ "SUP",
                                              "Azriel" ~ "SEMI",
                                              "Song" ~ "SONG",
                                              "PSPA" ~ "POP-Inf",
                                              "PDC1" ~ "PDC 1",
                                              "PDC2" ~ "PDC 2",
                                              .default = Estimator
  ) |>
    factor(x = _, levels = c("PDC","PDC 1", "PDC 2", "SEMI", "PPI", "SUP", "PPI++", "SONG", "POP-Inf")))


logistic_1 <- logistic_1 |>
  dplyr::mutate(Estimator = dplyr::case_match(Estimator,
                                              "Naive" ~ "SUP",
                                              "Song" ~ "SONG",
                                              "PSPA" ~ "POP-Inf",
                                              .default = Estimator
  ) |>
    factor(x = _, levels = c("PDC", "SEMI", "PPI", "SUP", "PPI++", "SONG", "POP-Inf")))





logistic_2 <- logistic_2 |>
  dplyr::mutate(Estimator = dplyr::case_match(Estimator,
                                              "Naive" ~ "SUP",
                                              "Song" ~ "SONG",
                                              "PSPA" ~ "POP-Inf",
                                              .default = Estimator
  ) |>
    factor(x = _, levels = c("PDC", "SEMI", "PPI", "SUP", "PPI++", "SONG", "POP-Inf")))

logistic_3 <- logistic_3 |>
  dplyr::mutate(Estimator = dplyr::case_match(Estimator,
                                              "Naive" ~ "SUP",
                                              "Song" ~ "SONG",
                                              "PSPA" ~ "POP-Inf",
                                              .default = Estimator
  ) |>
    factor(x = _, levels = c("PDC", "SEMI", "PPI", "SUP", "PPI++", "SONG", "POP-Inf")))




