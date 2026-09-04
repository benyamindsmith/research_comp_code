library(ipd)
source("utils/zhang_et_al_2019_code.R")
source("utils/song_et_al_2024_code/semi_supervised_methods.R")
source("utils/song_et_al_2024_code/SupervisedEstimation.R")


# If using a slurm job - need to run 1000 times.
# taskid <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
# set.seed(taskid)
n_sims <- 1000
alpha <- 0.1
z_crit <- qnorm(1 - alpha / 2)
n <- 1000
N <- 5000
p <- 4
beta_1 <- 9
beta_vec <-c(beta_1, -1, -2, -2)
epsilon <- seq(0,2, by=0.2)
# True Population Mean for Coverage Calculations
# (Based on Y [See for loop below])
true_theta <- 1

# Simulation

for(eps in epsilon){
  cat("Working on epsilon =", eps,"\n")
  
  # Matrices to store results for the current epsilon
  coverages <- matrix(0, nrow = n_sims, ncol = 6)
  widths    <- matrix(0, nrow = n_sims, ncol = 6)
  colnames(coverages) <- c("Naive", "Zhang","PDC", "PPI++", "PPI", "Song")
  colnames(widths)    <- c("Naive", "Zhang","PDC", "PPI++", "PPI", "Song")
  
  for(i in 1:n_sims){
    set.seed(i)
    cat("Working on sim", i,"/",n_sims,"\n")
    
    # Labelled Dataset
    zeta <- rnorm(n, mean=0, sd=1)
    X <- mvnfast::rmvn(n, mu=rep(0, p), sigma=diag(p))
    Y <- 1 + X%*%beta_vec + ((X^2 -1)%*%beta_vec) + zeta
    
    # Unlabelled Dataset
    x <- mvnfast::rmvn(N, mu=rep(0, p), sigma=diag(p))
    
    # Predictive Model
    # FIXED: Use uppercase 'X' for labeled
    zeta_prime_lab <- rnorm(n, mean=0, sd=1)
    mu_lab <- (1 + eps*zeta_prime_lab) * (X %*% beta_vec^2)
    
    # FIXED: Use lowercase 'x' for unlabeled
    zeta_prime_unlab <- rnorm(N, mean=0, sd=1)
    mu_unlab <- (1 + eps*zeta_prime_unlab) * (x %*% beta_vec^2) 
    
    # Semi-Supervised Estimator
    zhang_est <- zhang_ss_mean(X_lab=X, Y_lab=Y, X_unlab=x, alpha = alpha)
    
    # PDC Estimator 
    # FIXED: Replaced 'mu' with 'mu_unlab' (Note: Ensure your zhang function can accept this format)
    pdc_est <- zhang_ss_mean(X_lab=X, Y_lab=Y, X_unlab=x, mu_hat = mu_unlab, alpha = alpha)
    
    # Dataset set up for using ipd
    # FIXED: Replaced characters with 1 and 0 to prevent ipd bugs
    ipd_dat <- data.frame(
      Y   = c(Y, rep(NA, N)),                     
      f   = c(mu_lab, mu_unlab),                  
      set = c(rep(1, n), rep(0, N)) 
    )
    
    
    
    n  <- nrow(Y)
    N  <- nrow(mu_unlab)
    w_l <- rep(1, n)
    w_u <- rep(1, N)
    
    est0 <- mean(w_u * mu_unlab) + mean(w_l * (Y - mu_lab))
    grads <- w_l * (Y - est0)
    grads_hat <- w_l * (mu_lab - est0)
    grads_hat_unlabeled <- w_u * (mu_unlab - est0)
    
    lhat <- ipd:::calc_lhat_glm(grads, grads_hat, grads_hat_unlabeled,
                                diag(1), coord = NULL, clip = TRUE)
    
    ## PPI++
    ppi_pp_est <- ppi_plusplus_mean_est(Y_l = Y, f_l = mu_lab, f_u = mu_unlab, lhat = lhat)
    ppi_pp_ci  <- ppi_plusplus_mean(Y_l = Y, f_l = mu_lab, f_u = mu_unlab, alpha = alpha, lhat = lhat)
    ppi_pp_se  <- (ppi_pp_ci[, "upper"] - ppi_pp_ci[, "lower"]) / (2 * z_crit)
    
    ## PPI
    ppi_est <- ppi_plusplus_mean_est(Y_l = Y, f_l = mu_lab, f_u = mu_unlab, lhat = 1)
    ppi_ci  <- ppi_mean(Y_l = Y, f_l = mu_lab, f_u = mu_unlab, alpha = alpha)
    ppi_se  <- (ppi_ci[, "upper"] - ppi_ci[, "lower"]) / (2 * z_crit)
    
    # Naive (Supervised)
    naive_est <- zhang_est$naive_estimate
    naive_se  <- zhang_est$naive_se
    naive_ci  <- naive_est + c(-1, 1) * z_crit * naive_se
    
    # Zhang
    zhang_est_val <- zhang_est$ss_estimate
    zhang_se      <- zhang_est$ss_se
    zhang_ci      <- zhang_est$ss_ci
    
    # PDC 
    # FIXED: Uses ss_estimate and ss_se based on zhang_ss_mean output
    pdc_est_val <- pdc_est$ss_estimate
    pdc_se  <- pdc_est$ss_se
    pdc_ci  <- pdc_est$ss_ci
    
    # Song et al. (PSSE)
    song_est <- psse_ss_mean(
      Y_lab = Y, 
      X_lab = X, 
      X_unlab = x, 
      alpha_poly = 1,  
      sd = TRUE
    )
    
    song_mean <- song_est$ss_estimate
    song_se   <- song_est$ss_se
    song_ci   <- song_mean + c(-1, 1) * z_crit * song_se
    
    # Coverage logic
    coverages[i, "Naive"] <- (true_theta >= naive_ci[1]) & (true_theta <= naive_ci[2])
    coverages[i, "Zhang"] <- (true_theta >= zhang_ci[1]) & (true_theta <= zhang_ci[2])
    coverages[i, "PDC"]   <- (true_theta >= pdc_ci[1]) & (true_theta <= pdc_ci[2])
    coverages[i, "PPI++"] <- (true_theta >= ppi_pp_ci[1]) & (true_theta <= ppi_pp_ci[2])
    coverages[i, "PPI"]   <- (true_theta >= ppi_ci[1]) & (true_theta <= ppi_ci[2])
    coverages[i, "Song"]  <- (true_theta >= song_ci[1]) & (true_theta <= song_ci[2])
    
    # Width ratio logic
    widths[i, "Naive"] <- 1.0 
    widths[i, "Zhang"] <- zhang_se / naive_se
    widths[i, "PDC"]   <- pdc_se / naive_se
    widths[i, "PPI++"] <- ppi_pp_se / naive_se
    widths[i, "PPI"]   <- ppi_se / naive_se
    widths[i, "Song"]  <- song_se / naive_se
  }
  
  # Aggregate averages for the current epsilon
  avg_coverage <- colMeans(coverages)
  avg_width    <- colMeans(widths)
  
  # Append to final results
  res_df <- data.frame(
    epsilon      = eps,
    Estimator    = c("Naive", "Zhang","PDC", "PPI++", "PPI","Song"),
    Coverage     = avg_coverage,
    Width_Ratio  = avg_width
  )
  
  final_results <- rbind(final_results, res_df)
}
