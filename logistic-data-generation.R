library(mvnfast)

# ============================================================
# Design constants relevant to the true target parameters
# ============================================================
p <- 10
theta0 <- c(-4, 1, 1, 0.5, 0.5, rep(0, p - 4))   # theta0[1] = intercept

# rho now varies across the simulation grid rather than being fixed at
# source time, so Sigma is built on demand inside gen_x().
make_Sigma <- function(rho) {
  Sigma <- matrix(3 * rho, p, p)
  diag(Sigma) <- 3
  Sigma
}

# ============================================================
# Data-generating mechanisms
# ============================================================
gen_x <- function(n, rho) {
  Sigma <- make_Sigma(rho)
  Z <- rmvn(n, mu = rep(0, p), sigma = Sigma)
  B <- rbinom(n, 3, 0.3)
  Z + B   # adds scalar B to every column
}

g_expit        <- function(eta) plogis(eta)
g_probit_shift <- function(eta) pnorm((eta + 2) / 2)   # misspecified link, DGM 2

gen_y_dgm1 <- function(X) {
  eta <- cbind(1, X) %*% theta0
  rbinom(nrow(X), 1, g_expit(eta))
}
gen_y_dgm2 <- function(X) {
  eta <- cbind(1, X) %*% theta0
  rbinom(nrow(X), 1, g_probit_shift(eta))
}
gen_y_dgm3 <- function(X) {
  eta <- cbind(1, X) %*% theta0 + X[, 3] * X[, 4]
  rbinom(nrow(X), 1, g_expit(eta))
}

dgm_list <- list(dgm1 = gen_y_dgm1, dgm2 = gen_y_dgm2, dgm3 = gen_y_dgm3)

# ============================================================
# rho grid
# ============================================================
# Valid range for this compound-symmetry Sigma (diag = 3, off-diag = 3*rho,
# p = 10) is rho in [-1/9, 1] for positive semi-definiteness. 0 to 0.9 in
# steps of 0.1 sweeps independence -> strong correlation within that range.
rho_grid <- seq(0, 0.9, by = 0.1)

# ============================================================
# Monte Carlo "true" theta* per (DGM, rho) combination
# (the pseudo-true parameter the working logistic model converges to
# under misspecification, e.g. DGM 2 / DGM 3). Must be recomputed per
# rho because the covariate distribution -- and hence the pseudo-true
# parameter under misspecification -- depends on rho. DGM1 is correctly
# specified, so its true theta is theta0 regardless of rho, but we
# still compute it via the same MC procedure for consistency; it will
# simply converge numerically to theta0.
# ============================================================
compute_true_theta <- function(dgm_name, rho, n_mc = 50000, seed = 20260901) {
  set.seed(seed)
  X_mc <- gen_x(n_mc, rho)
  Y_mc <- dgm_list[[dgm_name]](X_mc)
  coef(glm(Y_mc ~ X_mc, family = binomial))
}

true_thetas <- lapply(names(dgm_list), function(dgm_name) {
  thetas_by_rho <- lapply(rho_grid, function(rho) compute_true_theta(dgm_name, rho))
  names(thetas_by_rho) <- paste0("rho_", rho_grid)
  thetas_by_rho
})
names(true_thetas) <- names(dgm_list)

saveRDS(true_thetas, "data/gronsbell-true-thetas.rds")

# ============================================================
# K-fold cross-fitting for the working classifier
# ============================================================
# Gan et al. (2024), "Prediction De-Correlated Inference," require the
# predictive model to be independent of the labeled/unlabeled data used
# for inference. When no separately pretrained model is available, they
# prescribe cross-fitting rather than fitting directly on the full
# labeled sample and predicting in-sample (which biases the correction
# term the PDC/PPI/PPI++/PSPA estimators rely on).
#
# mu_lab: computed via K-fold cross-fitting -- each labeled point is
#         predicted using a classifier trained on the *other* K-1 folds,
#         so no prediction is made using a model that saw that point.
# mu_unlab: computed using a classifier trained on the FULL labeled
#           sample. This is fine without cross-fitting because the
#           unlabeled sample x was never used in training -- it is
#           already genuinely held-out data.
cross_fit_mu_lab <- function(X, Y, K = 5) {
  n <- nrow(X)
  folds <- sample(rep(1:K, length.out = n))
  mu_lab <- numeric(n)
  for (k in seq_len(K)) {
    test_idx  <- which(folds == k)
    train_idx <- setdiff(seq_len(n), test_idx)
    Xk <- X[train_idx, , drop = FALSE]
    Yk <- Y[train_idx]
    clf_k <- glm(Yk ~ Xk, family = binomial)
    mu_lab[test_idx] <- predict(clf_k,
                                newdata = data.frame(Xk = I(X[test_idx, , drop = FALSE])),
                                type = "response")
  }
  mu_lab
}