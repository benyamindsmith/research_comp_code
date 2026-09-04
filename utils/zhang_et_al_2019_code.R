# Note: Code for implementing the method outlined in 
# "Semi-Supervised Inference: General Theory and Estimation of Means"
# By Zhang et al. (2019) is not available publicly. 
# The code here is an implementation of the method. 

zhang_ss_mean <- function(X_lab, Y_lab, X_unlab, mu_hat=NULL, alpha = 0.05) {
  X_lab <- as.matrix(X_lab)
  X_unlab <- as.matrix(X_unlab)
  Y_lab <- as.numeric(Y_lab)
  
  n <- nrow(X_lab)
  m <- nrow(X_unlab)
  
  # Compute local and global means
  Y_bar <- mean(Y_lab)
  X_bar <- colMeans(X_lab)
  if(is.null(mu_hat)){
    mu_hat <- colMeans(X_unlab)
  }else{
    mu_hat <- mu_hat
  }
  
  # Estimate beta via OLS on the labeled data
  design_matrix <- cbind(1, X_lab)
  fit <- lm.fit(x = design_matrix, y = Y_lab)
  
  # Extract slopes (drop the intercept)  
  beta_hat <- coef(fit)[-1]

  # 1. Compute the Semi-Supervised Point Estimator
  theta_hat <- Y_bar - sum(beta_hat * (X_bar - mu_hat))
  
  # 2. Variance and Confidence Interval Estimation
  # Residual variance from labeled data
  residuals <- Y_lab - (design_matrix %*% coef(fit))
  sigma2_eps <- sum(residuals^2) / (n - length(coef(fit)))
  # Covariance matrix of X from unlabeled data
  Sigma_X <- cov(X_unlab)
  # Asymptotic variance calculation
  var_beta_X <- as.numeric(t(beta_hat) %*% Sigma_X %*% beta_hat)
  var_theta_hat <- (sigma2_eps / n) + (var_beta_X / m)
  
  se <- sqrt(var_theta_hat)
  z_crit <- qnorm(1 - alpha / 2)
  
  # 5. Return comparison against the naive estimator
  list(
    ss_estimate = theta_hat,
    ss_se = se,
    ss_ci = c(theta_hat - z_crit * se, theta_hat + z_crit * se),
    naive_estimate = Y_bar,
    naive_se = sd(Y_lab) / sqrt(n)
  )
}