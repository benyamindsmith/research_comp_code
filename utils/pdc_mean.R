pdc_mean <- function(Y_lab, mu_lab, mu_unlab, alpha = 0.05) {
  Y_lab    <- as.numeric(Y_lab)
  mu_lab   <- as.numeric(mu_lab)     # mu(X) evaluated on LABELED units
  mu_unlab <- as.numeric(mu_unlab)   # mu(X) evaluated on UNLABELED units
  
  n <- length(Y_lab)
  N <- length(mu_unlab)
  
  Y_bar        <- mean(Y_lab)
  mu_lab_bar   <- mean(mu_lab)
  mu_unlab_bar <- mean(mu_unlab)
  
  gamma_hat <- cov(Y_lab, mu_lab) / var(mu_lab)   # regress Y on mu(X), labeled data only
  
  theta_hat <- Y_bar - gamma_hat * (mu_lab_bar - mu_unlab_bar)
  
  # Variance: residual-based, using the mu(X)-regression residuals
  resid <- Y_lab - gamma_hat * (mu_lab - mu_lab_bar)
  sigma2_eps <- var(resid)
  var_mu <- var(mu_lab)
  var_correction <- gamma_hat^2 * var_mu / N
  
  se <- sqrt(sigma2_eps / n + var_correction)
  z_crit <- qnorm(1 - alpha / 2)
  
  list(
    ss_estimate = theta_hat,
    ss_se = se,
    ss_ci = c(theta_hat - z_crit * se, theta_hat + z_crit * se)
  )
}