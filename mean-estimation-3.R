library(foreach)
library(doParallel)
library(dplyr)

n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores,outfile="")
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(ipd); library(MASS); library(caret); library(quantreg)
  source("utils/zhang_et_al_2019_code.R")
  source("utils/song_et_al_2024_code/semi_supervised_methods.R")
  source("utils/song_et_al_2024_code/SupervisedEstimation.R")
  source("utils/song_et_al_2024_mean_estimation_code.R")
  source("utils/pdc_mean.R")
  
})

n_sims <- 1000
alpha <- 0.1
z_crit <- qnorm(1 - alpha / 2)
n <- 1000
Ns <- seq(2000, 22000, by = 2000)
p <- 4
beta_vec <- c(9, -1, -2, -2)
true_theta <- 1
grid <- expand.grid(N = Ns, sim = 1:n_sims)

results <- foreach(row = 1:nrow(grid), .combine = rbind,
                   .packages = c("ipd")) %dopar% {
                     
                     N <- grid$N[row]
                     i <- grid$sim[row]
                     set.seed(i)
                     cat(sprintf("\n--- Starting Config N=%d (i=%d/1000) ---\n", N, i))
                     
                     zeta <- rnorm(n, mean = 0, sd = 1)
                     X <- mvnfast::rmvn(n, mu = rep(0, p), sigma = diag(p))
                     Y <- 1 + X %*% beta_vec + ((X^2 - 1) %*% beta_vec) + zeta
                     
                     x <- mvnfast::rmvn(N, mu = rep(0, p), sigma = diag(p))
                     
                     zeta_prime_lab <- rnorm(n, mean = 0, sd = 1)
                     mu_lab <- (1 + zeta_prime_lab) * (X^2 %*% beta_vec)
                     
                     zeta_prime_unlab <- rnorm(N, mean = 0, sd = 1)
                     mu_unlab <- (1 + zeta_prime_unlab) * (x^2 %*% beta_vec)
                     
                     zhang_est <- zhang_ss_mean(X_lab = X, Y_lab = Y, X_unlab = x, alpha = alpha)
                     pdc_est   <- pdc_mean(Y_lab = Y, mu_lab = mu_lab, mu_unlab = mu_unlab, alpha = alpha)
                     pspa_est  <- pspa_mean(Y_l = Y, f_l = mu_lab, f_u = mu_unlab, alpha = alpha)
                     
                     n_loc <- nrow(Y)
                     N_loc <- nrow(mu_unlab)
                     w_l <- rep(1, n_loc)
                     w_u <- rep(1, N_loc)
                     
                     est0 <- mean(w_u * mu_unlab) + mean(w_l * (Y - mu_lab))
                     grads <- w_l * (Y - est0)
                     grads_hat <- w_l * (mu_lab - est0)
                     grads_hat_unlabeled <- w_u * (mu_unlab - est0)
                     
                     lhat <- ipd:::calc_lhat_glm(grads, grads_hat, grads_hat_unlabeled,
                                                 diag(1), coord = NULL, clip = TRUE)
                     
                     ppi_pp_ci <- ppi_plusplus_mean(Y_l = Y, f_l = mu_lab, f_u = mu_unlab, alpha = alpha, lhat = lhat)
                     ppi_pp_se <- (ppi_pp_ci[, "upper"] - ppi_pp_ci[, "lower"]) / (2 * z_crit)
                     
                     ppi_ci <- ppi_mean(Y_l = Y, f_l = mu_lab, f_u = mu_unlab, alpha = alpha)
                     ppi_se <- (ppi_ci[, "upper"] - ppi_ci[, "lower"]) / (2 * z_crit)
                     
                     naive_est <- zhang_est$naive_estimate
                     naive_se  <- zhang_est$naive_se
                     naive_ci  <- naive_est + c(-1, 1) * z_crit * naive_se
                     
                     zhang_se <- zhang_est$ss_se
                     zhang_ci <- zhang_est$ss_ci
                     
                     pdc_se <- pdc_est$ss_se
                     pdc_ci <- pdc_est$ss_ci
                     
                     song_est <- psse_ss_mean(Y_lab = Y, X_lab = X, X_unlab = x, alpha_poly = 1, sd = TRUE)
                     song_mean <- song_est$ss_estimate
                     song_se   <- song_est$ss_se
                     song_ci   <- song_mean + c(-1, 1) * z_crit * song_se
                     
                     
                     pspa_mean_val <- pspa_est$est
                     pspa_se <- pspa_est$se
                     pspa_ci <- pspa_mean_val + c(-1, 1) * z_crit * pspa_se   # <- fixed typo (was pspas_ci)
                     
                     data.frame(
                       N = N, sim = i,
                       Estimator = c("Naive", "Zhang", "PDC", "PPI++", "PPI", "Song", "PSPA"),
                       Covered = c(
                         (true_theta >= naive_ci[1])  & (true_theta <= naive_ci[2]),
                         (true_theta >= zhang_ci[1])  & (true_theta <= zhang_ci[2]),
                         (true_theta >= pdc_ci[1])    & (true_theta <= pdc_ci[2]),
                         (true_theta >= ppi_pp_ci[1]) & (true_theta <= ppi_pp_ci[2]),
                         (true_theta >= ppi_ci[1])    & (true_theta <= ppi_ci[2]),
                         (true_theta >= song_ci[1])   & (true_theta <= song_ci[2]),
                         (true_theta >= pspa_ci[1])   & (true_theta <= pspa_ci[2])
                       ),
                       Width = c(1.0, zhang_se/naive_se, pdc_se/naive_se,
                                 ppi_pp_se/naive_se, ppi_se/naive_se,
                                 song_se/naive_se, pspa_se/naive_se)
                     )
                     
                   }

stopCluster(cl)

final_results_mean3 <- results %>%
  group_by(N, Estimator) %>%
  summarise(Coverage = mean(Covered), Width_Ratio = mean(Width), .groups = "drop")

saveRDS(final_results_mean3, "data/mean-estimation-3-results.rds")