# =============================================================================
# Replication of Gan et al. (2024), Table 1 -- Los Angeles homeless dataset
#
# Compares a supervised OLS benchmark against six semi-supervised / inference-
# on-predicted-data estimators, and writes a publication-ready LaTeX table.
# =============================================================================

library(ipd)
library(MASS)
library(caret)
library(readr)
library(dplyr)
library(randomForest)

source("utils/azriel_et_al_2022_code.R")
source("utils/song_et_al_2024_code/semi_supervised_methods.R")
source("utils/song_et_al_2024_code/SupervisedEstimation.R")

# -----------------------------------------------------------------------------
# INPUT CONVENTION FOR THE SONG / AZRIEL CODE  (PI, PI_se, PSSE, EASE, DRESS)
#
# All of these functions require a plain numeric MATRIX:
#   labelled_data   : column 1 = response, remaining columns = predictors
#   unlabelled_data : predictor columns only
#   NO intercept column -- each function adds its own internally.
#
# Two failure modes if you deviate (both verified against this code):
#
#  (a) A matrix that INCLUDES an intercept column makes p one too large, so the
#      loop eventually runs lm(X_combined[, j] ~ X_combined[, -j]) with the
#      constant column among the predictors. That is collinear with lm()'s
#      implicit intercept, R drops the aliased term and returns an NA
#      coefficient, the NA propagates through the next matrix multiply to every
#      row, and the following lm() fails with "0 (non-NA) cases".
#
#  (b) A data.frame or tibble fails with "invalid type (list) for variable ...",
#      because these functions use the whole object as a single formula term
#      (e.g. lm(labelled_data[, 1] ~ X_labelled)). Only a matrix auto-expands
#      into separate regressors there. readr::read_csv() returns tibbles, so the
#      as.matrix() calls below are required, not cosmetic.
#
# The ipd::* functions use the opposite convention: they DO take an intercept
# column, which is why X_l / X_u are built separately from labelled_mat.
# -----------------------------------------------------------------------------


# =============================================================================
# PART I -- TABLE HELPERS
#
# Layout: methods down the ROWS, predictors across the TOP (transposed relative
# to Gan et al.'s Table 1). With 7 methods the original layout needs 1 + 7*3 =
# 22 columns; this needs 1 + 3*3 = 10, measures 368pt against a 470pt textwidth,
# and adding a method costs a row rather than three columns.
# Base R only. LaTeX output requires \usepackage{booktabs}.
# =============================================================================

# Assemble a tidy method x predictor frame from the fitted objects.
#   methods  : named list, each element list(est = <p+1 vector>, se = <p+1 vector>)
#   baseline : method whose SD defines WR (the supervised benchmark)
#   reference: method against which "performs better" is judged. A method beats
#              the reference on a given coefficient when its estimated SD is
#              strictly smaller (equivalently, its WR is smaller, since both are
#              scaled by the same baseline). Flagged in the `better` column and
#              rendered in bold by latex_results(). The reference itself is never
#              flagged; NA SDs are never flagged.
build_results <- function(methods, predictors, baseline = "SUP", reference = "PDC") {
  base_se <- methods[[baseline]]$se
  ref_se  <- if (!is.null(reference)) as.numeric(methods[[reference]]$se) else NULL
  do.call(rbind, lapply(names(methods), function(m) {
    est <- as.numeric(methods[[m]]$est)
    se  <- as.numeric(methods[[m]]$se)
    stopifnot(length(est) == length(predictors), length(se) == length(predictors))
    better <- if (is.null(ref_se) || m == reference) rep(FALSE, length(se))
    else !is.na(se) & !is.na(ref_se) & se < ref_se
    data.frame(method = m, predictor = predictors, est = est, sd = se,
               wr = if (m == baseline) NA_real_ else se / base_se,
               better = better, stringsAsFactors = FALSE)
  }))
}

fmt <- function(x, d = 3, dash = "---") {
  ifelse(is.na(x), dash, formatC(x, format = "f", digits = d))
}

# Wrap in \textbf{} where flag is TRUE (never bolds a dash).
bold_tex <- function(txt, flag) ifelse(flag & txt != "---", paste0("\\textbf{", txt, "}"), txt)

# Console view: one block per predictor, methods as rows. Methods beating the
# reference are marked with "*" (bold does not survive piping to a log file).
print_results <- function(res, d = 3, reference = "PDC") {
  for (p in unique(res$predictor)) {
    s <- res[res$predictor == p, ]
    cat("\n", p, "\n", strrep("-", 40), "\n", sep = "")
    print(data.frame(Method = paste0(s$method, ifelse(s$better, " *", "")),
                     Est = fmt(s$est, d), SD = fmt(s$sd, d), WR = fmt(s$wr, d)),
          row.names = FALSE)
  }
  if (any(res$better)) cat("\n* smaller estimated SD than ", reference, "\n", sep = "")
  invisible(res)
}

# LaTeX (booktabs). file = NULL returns the string instead of writing.
latex_results <- function(res, d = 3, file = NULL,
                          caption = "Results from applying different methods to the Los Angeles homeless dataset described in Section 4.2.",
                          label = "tab:la-homeless",
                          reference = "PDC",
                          notes = NULL) {
  
  if (is.null(notes))
    notes <- sprintf(paste("$\\hat{\\theta}$, the point estimator; $\\widehat{\\mathrm{SD}}$, the estimated standard deviation;",
                           "WR, the ratio of the CI's width to that of the supervised counterpart. WR is 1 by definition for",
                           "the supervised estimator and is omitted. \\textbf{Bold} indicates a smaller estimated SD than %s."),
                     reference)
  
  preds <- unique(res$predictor)
  meths <- unique(res$method)
  
  grp  <- paste(sprintf("\\multicolumn{3}{c}{%s}", preds), collapse = " & ")
  cmid <- paste(sprintf("\\cmidrule(lr){%d-%d}",
                        seq(2, by = 3, length.out = length(preds)),
                        seq(4, by = 3, length.out = length(preds))), collapse = "")
  hdr  <- paste(rep("$\\hat{\\theta}$ & $\\widehat{\\mathrm{SD}}$ & WR", length(preds)),
                collapse = " & ")
  
  body <- vapply(meths, function(m) {
    cells <- unlist(lapply(preds, function(p) {
      r <- res[res$method == m & res$predictor == p, ]
      c(fmt(r$est, d),                              # estimate is not a
        bold_tex(fmt(r$sd, d), r$better),           # performance measure, so
        bold_tex(fmt(r$wr, d), r$better))           # only SD and WR are bolded
    }))
    paste0(m, " & ", paste(cells, collapse = " & "), " \\\\")
  }, character(1))
  
  tex <- c(
    "\\begin{table}[tb]", "\\centering",
    sprintf("\\caption{%s}", caption), sprintf("\\label{%s}", label),
    sprintf("\\begin{tabular}{l%s}", paste(rep("rrr", length(preds)), collapse = "")),
    "\\toprule", paste0(" & ", grp, " \\\\"), cmid,
    paste0("Method & ", hdr, " \\\\"), "\\midrule",
    body, "\\bottomrule", "\\end{tabular}",
    "\\begin{minipage}{\\linewidth}\\vspace{2pt}\\footnotesize",
    sprintf("\\textit{Notes:} %s", notes), "\\end{minipage}", "\\end{table}")
  
  if (is.null(file)) paste(tex, collapse = "\n") else {
    writeLines(tex, file)
    invisible(tex)
  }
}


# =============================================================================
# PART II -- ANALYSIS
# =============================================================================

# 1. Load Data ----------------------------------------------------------------
census_data <- readr::read_csv("utils/song_et_al_2024_code/WorkingExampleMatrix.csv")

# 2. Partition Data according to Table counts (244 / 265 / 1545) --------------
data_hot <- census_data %>% filter(selection == 2)                  # n = 244
data_dl  <- census_data %>% filter(selection == 1)                  # n = 265
data_du  <- census_data %>% filter(selection == 0 | is.na(StTotal)) # N = 1545

predictors <- c("Perc.Vacant", "Perc.Minority")

# Guard: the data_du filter keys on StTotal, not on the predictors, so it can
# admit rows with missing covariates. randomForest::predict() drops those rows
# silently, which would leave pred_u shorter than nrow(X_u) and produce a
# confusing dimension error deep inside the ipd calls.
stopifnot(
  !anyNA(data_dl[,  c("StTotal", predictors)]),
  !anyNA(data_du[,  predictors]),
  !anyNA(data_hot[, c("StTotal", predictors)])
)

# 3. Train Predictive Random Forest on the 244 Hot Tracts ---------------------
set.seed(20220122)
rf_hot <- randomForest(StTotal ~ Perc.Vacant + Perc.Minority, data = data_hot)

# 4. Generate Predictions & Design Matrices -----------------------------------
# For the ipd package (these DO take an intercept column):
X_l <- cbind(1, as.matrix(data_dl[, predictors]))
X_u <- cbind(1, as.matrix(data_du[, predictors]))
Y_l <- matrix(data_dl$StTotal, ncol = 1)

pred_l <- matrix(predict(rf_hot, newdata = data_dl), ncol = 1)   # mu(x)
pred_u <- matrix(predict(rf_hot, newdata = data_du), ncol = 1)

stopifnot(nrow(pred_l) == nrow(X_l), nrow(pred_u) == nrow(X_u))

# For PI_se() / PSSE() (matrix, response first, NO intercept column):
labelled_mat   <- as.matrix(data.frame(StTotal = data_dl$StTotal,
                                       data_dl[, predictors]))
unlabelled_mat <- as.matrix(data_du[, predictors])

# 5. Supervised Estimator (SUP) Benchmark -------------------------------------
sup_mod  <- lm(StTotal ~ Perc.Vacant + Perc.Minority, data = data_dl)
sup_coef <- coef(sup_mod)
sup_se   <- summary(sup_mod)$coefficients[, "Std. Error"]

# 6. Azriel et al. (2022) PI --------------------------------------------------
# intercept_se = "influence" fills in the intercept SE, which Azriel et al.'s
# eq. (28) does not cover. Use "bootstrap" for an assumption-lighter (slower,
# noisier) alternative, or "none" for the original NA. Slope SEs are identical
# in all three cases.
azriel_fit <- PI_se(labelled_mat, unlabelled_mat, intercept_se = "influence")
azriel_est <- azriel_fit$Hattheta
azriel_se  <- azriel_fit$se

# 7. PDC (Gan et al. 2024, Algorithm 1), PPI++, PPI, PSPA ---------------------
fit_pdc   <- ipd::pdc_ols(X_l = X_l, Y_l = Y_l, f_l = pred_l,
                          X_u = X_u, f_u = pred_u)

fit_ppipp <- ipd::ppi_plusplus_ols(X_l = X_l, Y_l = Y_l, f_l = pred_l,
                                   X_u = X_u, f_u = pred_u)

fit_ppi   <- ipd::ppi_ols(X_l = X_l, Y_l = Y_l, f_l = pred_l,
                          X_u = X_u, f_u = pred_u)

pspa_fit  <- ipd::pspa_ols(X_l = X_l, Y_l = Y_l, f_l = pred_l,
                           X_u = X_u, f_u = pred_u)

# 8. Song et al. (2024) PSSE --------------------------------------------------
song_fit <- PSSE(labelled_data   = labelled_mat,
                 unlabelled_data = unlabelled_mat,
                 type = "linear", sd = TRUE)

song_est <- song_fit$Hattheta
song_se  <- song_fit$sd.of.hattheta

# 9. Summary Table (Replicating Table 1) --------------------------------------
table_predictors <- c("Intercept", "Perc. vacant", "Perc. minority")

methods <- list(
  SUP     = list(est = sup_coef,      se = sup_se),
  Azriel  = list(est = azriel_est,    se = azriel_se),
  PDC     = list(est = fit_pdc$est,   se = fit_pdc$se),
  `PPI++` = list(est = fit_ppipp$est, se = fit_ppipp$se),
  PPI     = list(est = fit_ppi$est,   se = fit_ppi$se),
  PSPA    = list(est = pspa_fit$est,  se = pspa_fit$se),
  Song    = list(est = song_est,      se = song_se)
)

# baseline = denominator for WR; reference = what "performs better" is judged
# against (bolded when the estimated SD is strictly smaller).
results <- build_results(methods, table_predictors,
                         baseline = "SUP", reference = "PDC")

print_results(results, reference = "PDC")                       # console view
latex_results(results, reference = "PDC", file = "tables/data-analysis.tex")  # \input{} this