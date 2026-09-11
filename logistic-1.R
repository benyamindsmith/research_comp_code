library(foreach)
library(doParallel)
library(dplyr)
library(stratifiedSSL)

source("logistic-data-generation.R")

n_cores <- 8
cl <- makeCluster(n_cores)
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(ipd); library(MASS); library(mvnfast); library(stratifiedSSL); library(caret)
  source("utils/ssl_logistic_light.R")
  source("utils/song_et_al_2024_code/semi_supervised_methods.R")
  source("utils/song_et_al_2024_code/SupervisedEstimation.R")
})

dgm_name <- "dgm1"
gen_y <- gen_y_dgm1
true_theta <- true_thetas[[dgm_name]][2]  

N <- 20000
n_train <- 10000
ns <- c(200, 400)
n_sims <- 1000
alpha <- 0.1
z_crit <- qnorm(1 - alpha / 2)

grid <- expand.grid(n = ns, sim = 1:n_sims, stringsAsFactors = FALSE)

log_file <- "logs/progress_logistic1.log"
if (file.exists(log_file)) file.remove(log_file)

results <- foreach(row = 1:nrow(grid), .combine = rbind,
                   .packages = c("ipd", "stratifiedSSL"),
                   .errorhandling = "stop") %dopar% {
                     
                     n <- grid$n[row]
                     i <- grid$sim[row]
                     set.seed(i)
                    
                    # if (row %% 200 == 0) {
                      cat(sprintf("[%d/%d] dgm=%s n=%d sim=%d\n", row, nrow(grid), dgm_name, n, i),
                           file = log_file, append = TRUE)
                    #}
                     
                     # Independent training sample -> fit the working classifier
                     X_train <- gen_x(n_train)
                     Y_train <- gen_y(X_train)
                     clf <- glm(Y_train ~ X_train, family = binomial)
                     
                     # Labeled + unlabeled target-population samples
                     X <- gen_x(n)
                     Y <- gen_y(X)
                     x <- gen_x(N)
                     
                     X_int <- cbind(1, X)
                     x_int <- cbind(1, x)
                     
                     mu_lab   <- matrix(predict(clf, newdata = data.frame(X_train = I(X)), type = "response"), ncol = 1)
                     mu_unlab <- matrix(predict(clf, newdata = data.frame(X_train = I(x)), type = "response"), ncol = 1)
                     
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
                       dgm = dgm_name, n = n, sim = i,
                       Estimator = c("Naive", "SEMI", "PDC", "PPI++", "PPI", "PSPA", "Song"),
                       Covered = (true_theta >= los) & (true_theta <= his),
                       Width   = ses / naive_se
                     )
                   }

stopCluster(cl)

final_results_logistic1 <- results %>%
  group_by(dgm, n, Estimator) %>%
  summarise(Coverage = mean(Covered), Width_Ratio = mean(Width), .groups = "drop")

saveRDS(final_results_logistic1, "data/logistic-1-results.rds")
