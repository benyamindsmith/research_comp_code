# Note: Code for implementing the method outlined in
# "Semi-Supervised Linear Regression"
# By Azriel et al. (2022) is not available publicly.
# Based on code from Song et al.
#
# INPUT CONVENTION: labelled_data and unlabelled_data must be numeric MATRICES.
#   labelled_data   : column 1 = response, remaining columns = predictors
#   unlabelled_data : predictor columns only
#   NO intercept column -- PI()/PI_se() add their own internally.
#
# intercept_se = how to compute the standard error of the intercept, which
#   Azriel et al.'s eq. (28) does not cover (it applies to the slopes only):
#     "influence" (default) -- delta-method / influence-function variance
#     "bootstrap"           -- nonparametric bootstrap over the labelled rows
#     "none"                -- legacy behaviour, returns NA for the intercept

PI_se <- function(labelled_data, unlabelled_data,
                  intercept_se = c("influence", "bootstrap", "none"), B = 500) {
  
  intercept_se <- match.arg(intercept_se)
  
  n  <- nrow(labelled_data)
  N  <- nrow(unlabelled_data)
  p  <- ncol(labelled_data) - 1
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
  AV_LSE_full <- n * XtX_inv %*% (t(X_int) %*% (delta_LSE^2 * X_int)) %*% XtX_inv
  AV_LSE      <- AV_LSE_full[-1, -1, drop = FALSE]  # drop intercept row/col
  
  AV_PI <- Cov_delta + nu * (AV_LSE - Cov_delta)
  diag(AV_PI) <- diag(Cov_delta) + nu * pmax(diag(AV_LSE) - diag(Cov_delta), 0)
  
  se_beta <- sqrt(diag(AV_PI) / n)
  
  # ---- standard error of the intercept ---------------------------------------
  # PI() sets hat_alpha = mean(Y) - hat_beta' * colMeans(X_labelled), so
  #   hat_alpha - alpha ~= mean_i[(Y_i - muY) - (X_i - muX)'beta] - muX'(hat_beta - beta)
  # and hat_beta - beta is represented by the delta_tilde influence function
  # already computed above. Hence the per-observation influence function
  #   psi_i = (Y_i - Ybar) - (X_i - muX)'beta - muX' delta_tilde_i
  # NOTE: this is a delta-method extension, not part of Azriel et al.'s eq. (28).
  se_alpha <- NA_real_
  if (intercept_se == "influence") {
    muX  <- colMeans(X_labelled)
    beta <- fit$Hattheta[-1]
    psi  <- (labelled_data[, 1] - mean(labelled_data[, 1])) -
      as.vector(sweep(X_labelled, 2, muX) %*% beta) -
      as.vector(delta_tilde %*% muX)
    se_alpha <- sqrt(var(psi) / n)
    
  } else if (intercept_se == "bootstrap") {
    boot <- replicate(B, {
      idx <- sample.int(n, n, replace = TRUE)
      f   <- PI(labelled_data[idx, ], unlabelled_data)
      mean(labelled_data[idx, 1]) - sum(f$Hattheta[-1] * colMeans(labelled_data[idx, -1]))
    })
    se_alpha <- sd(boot)
  }
  
  list(Hattheta = fit$Hattheta, se = c(se_alpha, se_beta))
}