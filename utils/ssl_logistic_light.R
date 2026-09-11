ssl_logistic_light <- function(X_labeled, X_unlabeled, y, num_knots = 3, num_folds = 3) {
  n_lab <- nrow(X_labeled); N_unlab <- nrow(X_unlabeled)
  S_labeled <- rep(1, n_lab); S_unlabeled <- rep(1, N_unlab)
  samp_prob <- rep(1, n_lab)
  my_basis <- stratifiedSSL:::NaturalSplineBasis(rbind(X_labeled, X_unlabeled),
                                 c(S_labeled, S_unlabeled), num_knots = num_knots)
  basis_labeled   <- my_basis[1:n_lab, ]
  basis_unlabeled <- my_basis[(n_lab + 1):nrow(my_basis), ]
  reg <- stratifiedSSL:::SemiSupervisedRegression(basis_labeled, basis_unlabeled,
                                  X_labeled, X_unlabeled, y, samp_prob, lambda = NULL)
  cvr <- stratifiedSSL:::CrossValResids(basis_labeled, basis_unlabeled, X_labeled,
                        X_unlabeled, y, samp_prob, num_folds)
  se_obj <- stratifiedSSL:::StdErrorEstimation(X_labeled, X_unlabeled, y, reg$beta_SSL, cvr$resids_gamma)
  list(est = reg$beta_SSL, se = se_obj$std_error)
}