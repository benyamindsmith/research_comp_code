library(foreach)
library(doParallel)
library(dplyr)

n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(ipd); library(MASS); library(caret); library(quantreg)
  library(mvnfast); library(randomForest)
  source("utils/azriel_et_al_2022_code.R")
  source("utils/song_et_al_2024_code/semi_supervised_methods.R")
  source("utils/song_et_al_2024_code/SupervisedEstimation.R")
  source("utils/pdc_multi.R")
})

n_sims <- 1000
alpha  <- 0.1
z_crit <- qnorm(1 - alpha / 2)
n <- 1000; N <- 5000; p <- 4
beta_1s <- 0:10

grid <- expand.grid(beta_1 = beta_1s, sim = 1:n_sims)
log_file <- "logs/progress_beta3.log"
if (file.exists(log_file)) file.remove(log_file)

results <- foreach(row = 1:nrow(grid), .combine = rbind,
                   .packages = c("ipd", "randomForest")) %dopar% {
                     
                     beta_1 <- grid$beta_1[row]
                     i      <- grid$sim[row]
                     set.seed(row)
                     
                     if (row %% 50 == 0) {
                       cat(sprintf("[%d/%d] beta_1=%d sim=%d\n", row, nrow(grid), beta_1, i),
                           file = log_file, append = TRUE)
                     }
                     
                     beta_vec   <- c(beta_1, -1, -2, -2)
                     true_theta <- beta_1
                     
                     X_1 <- rmvn(n, mu = rep(0, p), sigma = diag(p))
                     zeta_1 <- rlnorm(n, meanlog = 0, sdlog = 1)
                     Y_1 <- X_1 %*% beta_vec + (X_1^3) %*% beta_vec + exp(X_1) %*% beta_vec + zeta_1
                     
                     X_2 <- rmvn(n, mu = rep(0, p), sigma = diag(p))
                     zeta_2 <- rt(n, df = 3)
                     Y_2 <- (X_2^2) %*% beta_vec + zeta_2
                     
                     rf1 <- randomForest(x = X_1, y = as.vector(Y_1))
                     rf2 <- randomForest(x = X_2, y = as.vector(Y_2))
                     
                     X <- rmvn(n, mu = rep(0, p), sigma = diag(p))
                     zeta <- rnorm(n, mean = 0, sd = 1)
                     Y <- 1 + X %*% beta_vec + (X^2 - 1) %*% beta_vec + zeta
                     x <- rmvn(N, mu = rep(0, p), sigma = diag(p))
                     
                     mu1_lab   <- as.vector(predict(rf1, newdata = X))
                     mu2_lab   <- as.vector(predict(rf2, newdata = X))
                     mu1_unlab <- as.vector(predict(rf1, newdata = x))
                     mu2_unlab <- as.vector(predict(rf2, newdata = x))
                     
                     labelled_data   <- cbind(Y, X)
                     unlabelled_data <- x
                     X_int <- cbind(1, X)
                     x_int <- cbind(1, x)
                     
                     naive_fit <- lm(Y ~ X)
                     theta_hat <- coef(naive_fit)
                     e <- residuals(naive_fit)
                     XtX_inv <- solve(crossprod(X_int))
                     meat <- crossprod(X_int * e) 
                     V <- XtX_inv %*% meat %*% XtX_inv
                     
                     naive_est <- theta_hat[2]
                     naive_se  <- sqrt(diag(V))[2]   
                     
                     azriel_fit <- PI_se(labelled_data, unlabelled_data, intercept_se = "none")
                     azriel_est <- azriel_fit$Hattheta[2]; azriel_se <- azriel_fit$se[2]
                     
                     pdc_fit  <- pdc_ols_multi(X_int, Y, list(mu1_lab, mu2_lab), x_int, list(mu1_unlab, mu2_unlab))
                     pdc_est  <- pdc_fit$est[2]; pdc_se <- pdc_fit$se[2]
                     
                     pdc1_fit <- pdc_ols_multi(X_int, Y, list(mu1_lab), x_int, list(mu1_unlab))
                     pdc1_est <- pdc1_fit$est[2]; pdc1_se <- pdc1_fit$se[2]
                     
                     pdc2_fit <- pdc_ols_multi(X_int, Y, list(mu2_lab), x_int, list(mu2_unlab))
                     pdc2_est <- pdc2_fit$est[2]; pdc2_se <- pdc2_fit$se[2]
                     
                     pp_fit  <- ipd::ppi_plusplus_ols(X_int, Y, matrix(mu1_lab, ncol = 1), x_int, matrix(mu1_unlab, ncol = 1))
                     pp_est  <- pp_fit$est[2]; pp_se <- pp_fit$se[2]
                     
                     ppi_fit <- ipd::ppi_ols(X_int, Y, matrix(mu1_lab, ncol = 1), x_int, matrix(mu1_unlab, ncol = 1))
                     ppi_est <- ppi_fit$est[2]; ppi_se <- ppi_fit$se[2]
                     
                     pspa_fit <- ipd::pspa_ols(X_int, Y, matrix(mu1_lab, ncol = 1), x_int, matrix(mu1_unlab, ncol = 1))
                     pspa_est <- pspa_fit$est[2]; pspa_se <- pspa_fit$se[2]
                     
                     song_fit <- PSSE(labelled_data, unlabelled_data, type = "linear", sd = TRUE)
                     song_est <- song_fit$Hattheta[2]; song_se <- song_fit$sd.of.hattheta[2]
                     
                     ests <- c(naive_est, azriel_est, pdc_est, pdc1_est, pdc2_est, pp_est, ppi_est, pspa_est, song_est)
                     ses  <- c(naive_se,  azriel_se,  pdc_se,  pdc1_se,  pdc2_se,  pp_se,  ppi_se,  pspa_se,  song_se)
                     los  <- ests - z_crit * ses
                     his  <- ests + z_crit * ses
                     
                     data.frame(
                       beta_1 = beta_1, sim = i,
                       Estimator = c("Naive", "Azriel", "PDC", "PDC1", "PDC2", "PPI++", "PPI", "PSPA", "Song"),
                       Covered = (true_theta >= los) & (true_theta <= his),
                       Width   = ses / naive_se
                     )
                   }

stopCluster(cl)

final_results_beta3 <- results %>%
  group_by(beta_1, Estimator) %>%
  summarise(Coverage = mean(Covered), Width_Ratio = mean(Width), .groups = "drop")

saveRDS(final_results_beta3, "data/linear-regression-3-results.rds")