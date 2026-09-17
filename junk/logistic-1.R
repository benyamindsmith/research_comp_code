library(foreach)
library(doParallel)
library(dplyr)
library(stratifiedSSL)

source("logistic-data-generation.R")

n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(ipd); library(MASS); library(mvnfast); library(stratifiedSSL); library(caret)
  source("utils/ssl_logistic_light.R")
  source("utils/song_et_al_2024_code/semi_supervised_methods.R")
  source("utils/song_et_al_2024_code/SupervisedEstimation.R")
})

# ---- Fixed DGM for this script ----
dgm_name <- "dgm1"
gen_y <- gen_y_dgm1

# ---- Fixed sample sizes (Gan et al. style: n, N fixed, sweep rho instead) ----
N <- 5000
n <- 1000
n_sims <- 1000
alpha <- 0.1
z_crit <- qnorm(1 - alpha / 2)
K_folds <- 5   # folds for cross-fitting the working classifier (see cross_fit_mu_lab)

# rho_grid comes from logistic-data-generation.R (0 to 0.9 by 0.1)
grid <- expand.grid(rho = rho_grid, sim = 1:n_sims, stringsAsFactors = FALSE)
W
log_file <- "logs/progress_logistic1.log"
if (file.exists(log_file)) file.remove(log_file)

results <- foreach(row = 1:nrow(grid), .combine = rbind,
                   .packages = c("ipd", "stratifiedSSL"),
                   .errorhandling = "remove") %dopar% {
                     
                     rho <- grid$rho[row]
                     i <- grid$sim[row]
                     set.seed(row)
                     
                     cat(sprintf("[%d/%d] dgm=%s rho=%.1f sim=%d\n", row, nrow(grid), dgm_name, rho, i),
                         file = log_file, append = TRUE)
                     
                     true_theta <- true_thetas[[dgm_name]][[paste0("rho_", rho)]][2]   # coefficient on X1
                     
                     tryCatch({
                       # Labeled + unlabeled target-population samples
                       X <- gen_x(n, rho)
                       Y <- gen_y(X)
                       x <- gen_x(N, rho)
                       
                       # mu_lab: K-fold cross-fitted predictions (see
                       # cross_fit_mu_lab() in logistic-data-generation.R)
                       # mu_unlab: classifier fit on the full labeled sample,
                       # predicted on the (already held-out) unlabeled sample
                       clf_full <- glm(Y ~ X, family = binomial)
                       
                       X_int <- cbind(1, X)
                       x_int <- cbind(1, x)
                       
                       mu_lab   <- matrix(cross_fit_mu_lab(X, Y, K = K_folds), ncol = 1)
                       mu_unlab <- matrix(predict(clf_full, newdata = data.frame(X = I(x)), type = "response"), ncol = 1)
                       
                       naive_fit <- summary(glm(Y ~ X, family = binomial))$coefficients
                       naive_est <- naive_fit[2, 1]; naive_se <- naive_fit[2, 2]
                       
                       ssl_fit <- ssl_logistic_light(X, x, Y)
                       ssl_est <- ssl_fit$est[2]; ssl_se <- ssl_fit$se[2]
                       
                       pdc_fit <- ipd::pdc_logistic(X_int, Y, mu_lab, x_int, mu_unlab, intercept = TRUE)
                       pdc_est <- pdc_fit$est[2]; pdc_se <- pdc_fit$se[2]
                       
                       pp_fit <- ipd::ppi_plusplus_logistic(X_int, Y, mu_lab, x_int, mu_unlab)
                       pp_est <- pp_fit$est[2]; pp_se <- pp_fit$se[2]
                       
                       ppi_fit <- ipd::ppi_logistic(X_int, Y, mu_lab, x_int, mu_unlab)
                       ppi_est <- ppi_fit$est[2]; ppi_se <- ppi_fit$se[2]
                       
                       pspa_fit <- ipd::pspa_logistic(X_int, Y, mu_lab, x_int, mu_unlab)
                       pspa_est <- pspa_fit$est[2]; pspa_se <- pspa_fit$se[2]
                       
                       labelled_data   <- cbind(Y, X)
                       unlabelled_data <- x
                       
                       song_fit <- PSSE(labelled_data, unlabelled_data, type = "logistic", sd = TRUE)
                       song_est <- song_fit$Hattheta[2]; song_se <- song_fit$sd.of.hattheta[2]
                       
                       ests <- c(naive_est, ssl_est, pdc_est, pp_est, ppi_est, pspa_est, song_est)
                       ses  <- c(naive_se,  ssl_se,  pdc_se,  pp_se,  ppi_se,  pspa_se,  song_se)
                       los  <- ests - z_crit * ses
                       his  <- ests + z_crit * ses
                       
                       data.frame(
                         dgm = dgm_name, rho = rho, sim = i,
                         Estimator = c("Naive", "SEMI", "PDC", "PPI++", "PPI", "PSPA", "Song"),
                         Covered = (true_theta >= los) & (true_theta <= his),
                         Width   = ses / naive_se
                       )
                     }, error = function(e) {
                       cat(sprintf("[FAILED] dgm=%s rho=%.1f sim=%d: %s\n", dgm_name, rho, i, conditionMessage(e)),
                           file = log_file, append = TRUE)
                       NULL
                     })
                   }

stopCluster(cl)

final_results_logistic1 <- results %>%
  group_by(dgm, rho, Estimator) %>%
  summarise(Coverage = mean(Covered), Width_Ratio = mean(Width), .groups = "drop")

saveRDS(final_results_logistic1, "data/logistic-1-results.rds")