library(mvnfast)

# ============================================================
# Design constants relevant to the true target parameters
# ============================================================
rho <- 0.2
p <- 10

theta0 <- c(-4, 1, 1, 0.5, 0.5, rep(0, p - 4))   # theta0[1] = intercept
Sigma <- matrix(3 * rho, p, p); diag(Sigma) <- 3

# ============================================================
# Data-generating mechanisms
# ============================================================
gen_x <- function(n) {
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
# One-time Monte Carlo "true" theta* per DGM
# (the pseudo-true parameter the working logistic model
# converges to under misspecification, e.g. DGM 2 / DGM 3)
# ============================================================
set.seed(20240101)
true_thetas <- lapply(names(dgm_list), function(dgm_name) {
  X_mc <- gen_x(50000)
  Y_mc <- dgm_list[[dgm_name]](X_mc)
  coef(glm(Y_mc ~ X_mc, family = binomial))
})
names(true_thetas) <- names(dgm_list)

saveRDS(true_thetas, "data/gronsbell-true-thetas.rds")