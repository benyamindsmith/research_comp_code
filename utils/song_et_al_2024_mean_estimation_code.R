psse_ss_mean <- function(Y_lab, X_lab, X_unlab, alpha_poly = NULL, sd = FALSE) {
  
  # 1. Coerce to matrices
  X_lab <- as.matrix(X_lab)
  X_unlab <- as.matrix(X_unlab)
  Y_lab <- as.numeric(Y_lab)
  
  # 2. Approximate E[X] using the large unlabeled dataset
  mu_x_hat <- colMeans(X_unlab)
  
  # 3. Center the covariates 
  X_lab_centered <- sweep(X_lab, 2, mu_x_hat, FUN = "-")
  X_unlab_centered <- sweep(X_unlab, 2, mu_x_hat, FUN = "-")
  
  # 4. Format the data precisely as PSSE expects 
  # (First column is Y, subsequent columns are X)
  labelled_data_psse <- cbind(Y_lab, X_lab_centered)
  
  # 5. Fit the PSSE model using the linear working model
  fit_psse <- PSSE(
    labelled_data = labelled_data_psse,
    unlabelled_data = X_unlab_centered,
    type = "linear", 
    alpha = alpha_poly, # If NULL, GBIC_ppo selects the polynomial order
    sd = sd
  )
  
  # 6. Extract the intercept, which is now the marginal mean
  mean_estimate <- fit_psse$Hattheta[1]
  
  # 7. Format output
  result <- list(
    ss_estimate = mean_estimate,
    poly_order_selected = fit_psse$alpha
  )
  
  if (sd) {
    # The first standard deviation corresponds to the intercept
    result$ss_se <- fit_psse$sd.of.hattheta[1]
  }
  
  return(result)
}