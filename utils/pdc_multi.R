# PDC one-step estimator for linear regression (Gan, Liang & Zou, 2024),
# implemented directly from Algorithm 1 / eq. (4), (6), (7), Corollary 1,
# and the variance formulas in the paper's Appendix ("Additional results").
# Supports K >= 1 predictive models via the stacked f(x, theta) of eq. (5).
#
# X_lab, X_unlab: design matrices WITH intercept column (n x p, N x p)
# mu_lab_list, mu_unlab_list: lists of K numeric vectors (model predictions)
pdc_ols_multi <- function(X_lab, Y_lab, mu_lab_list, X_unlab, mu_unlab_list, gamma = NULL) {
  
  n <- nrow(X_lab); N <- nrow(X_unlab)
  eta <- N / (n + N)
  if (is.null(gamma)) gamma <- -eta   # gamma_opt, Corollary 1
  
  theta0 <- as.vector(solve(t(X_lab) %*% X_lab, t(X_lab) %*% Y_lab))  # theta_sup, initial estimator
  H_hat  <- (t(X_lab) %*% X_lab) / n                                   # Example 2
  
  # s(y, x, theta) = (x'theta - y) x  ->  n x p
  s_of <- function(theta, X, Y) as.vector(X %*% theta - Y) * X
  
  # f(x, theta) = stack_k s(mu_k(x), x, theta)  ->  n x (K*p), eq. (5)
  f_of <- function(theta, X, mu_list) {
    do.call(cbind, lapply(mu_list, function(mu) as.vector(X %*% theta - mu) * X))
  }
  
  s_lab <- s_of(theta0, X_lab, Y_lab)              # n x p
  f_lab <- f_of(theta0, X_lab, mu_lab_list)        # n x (K*p)
  f_unlab <- f_of(theta0, X_unlab, mu_unlab_list)  # N x (K*p)
  
  s1 <- colMeans(s_lab)
  s2 <- colMeans(f_lab)
  s3 <- colMeans(f_unlab)
  
  T_hat <- cov(s_lab, f_lab) %*% solve(cov(f_lab))  # eq. (4), p x (K*p)
  
  S_hat <- s1 + gamma * as.vector(T_hat %*% (s2 - s3))          # eq. (6)
  theta_pdc <- theta0 - as.vector(solve(H_hat, S_hat))          # Algorithm 1
  
  # Gamma_hat(gamma), V_hat(gamma) -- Appendix "Additional results" formulas
  Gamma_hat <- cov(s_lab) + (gamma^2 / eta + 2 * gamma) * (T_hat %*% cov(f_lab, s_lab))
  H_inv <- solve(H_hat)
  V_hat <- H_inv %*% Gamma_hat %*% t(H_inv)
  se <- sqrt(diag(V_hat) / n)
  
  list(est = theta_pdc, se = se)
}