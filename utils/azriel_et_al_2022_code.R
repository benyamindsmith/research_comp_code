# Note: Code for implementing the method outlined in 
# "Semi-Supervised Linear Regression"
# By Azriel et al. (2022) is not available publicly. 
# Based on code from Song et al.

PI_se <- function(labelled_data, unlabelled_data) {
  
  n <- nrow(labelled_data)
  N <- nrow(unlabelled_data)
  p <- ncol(labelled_data) - 1
  nu <- n / (n + N)
  
  fit <- PI(labelled_data, unlabelled_data)   # {hat alpha, hat beta_1..hat beta_p}
  
  X_combined <- rbind(labelled_data[, -1], unlabelled_data)
  X_labelled <- labelled_data[, -1]
  
  delta_tilde <- matrix(0, n, p)              # check-tilde-delta_j^(i), Sec 6.1
  for (j in 1:p) {
    coef_negj    <- lm(X_combined[, j] ~ X_combined[, -j])$coefficients
    Xj_dot       <- X_labelled[, j] - cbind(1, X_labelled[, -j]) %*% coef_negj
    Xj_dot_total <- X_combined[, j] - cbind(1, X_combined[, -j]) %*% coef_negj
    E_Xjdot2     <- mean(Xj_dot_total^2)
    
    W_j <- labelled_data[, 1] * Xj_dot / E_Xjdot2
    U1  <- Xj_dot / E_Xjdot2
    U   <- sapply(1:p, function(jj) {
      if (jj == j) X_labelled[, jj] * Xj_dot / E_Xjdot2 - 1
      else         X_labelled[, jj] * Xj_dot / E_Xjdot2
    })
    
    delta_tilde[, j] <- residuals(lm(W_j ~ U1 + U))
  }
  Cov_delta <- cov(delta_tilde)               # empirical Cov(check-tilde-delta)
  
  # Parametric sandwich estimate of AV(beta_LSE), Section 6
  X_int       <- cbind(1, X_labelled)
  delta_LSE   <- residuals(lm(labelled_data[, 1] ~ X_labelled))
  XtX_inv     <- solve(t(X_int) %*% X_int)
  AV_LSE_full <- XtX_inv %*% (t(X_int) %*% (delta_LSE^2 * X_int)) %*% XtX_inv
  AV_LSE      <- AV_LSE_full[-1, -1, drop = FALSE]  # drop intercept row/col
  
  AV_PI <- Cov_delta + nu * (AV_LSE - Cov_delta)
  diag(AV_PI) <- diag(Cov_delta) + nu * pmax(diag(AV_LSE) - diag(Cov_delta), 0)
  
  se_beta <- sqrt(diag(AV_PI) / n)
  
  list(Hattheta = fit$Hattheta, se = c(NA, se_beta))  # se[1] (intercept) not covered by (28)
}

