R
library(ipd)

# =====================================================================
# PATCH 1: Fix zconfint_generic to sanitize the corrupted internal variables
# =====================================================================
patched_zconf <- function(mean, std_mean, alpha, alternative) {
  
  # 1. Safeguard 'alternative': If it's numeric, null, or not a string, force "two-sided"
  if (missing(alternative) || is.null(alternative) || !is.character(alternative) || length(alternative) == 0) {
    alt <- "two-sided"
  } else {
    alt <- as.character(alternative[1])
    if (!(alt %in% c("two-sided", "2-sided", "2s", "two.sided", "larger", "l", "greater", "smaller", "s", "less"))) {
      alt <- "two-sided"
    }
  }
  
  # 2. Safeguard 'alpha': If it's a vector (like N=5000) or out of bounds, fallback to 0.05
  if (missing(alpha) || is.null(alpha) || !is.numeric(alpha) || length(alpha) > 1 || alpha <= 0 || alpha >= 1) {
    alpha <- 0.05 
  }
  
  # 3. Standard Interval Calculations
  if (alt %in% c("two-sided", "2-sided", "2s", "two.sided")) {
    zcrit <- qnorm(1 - alpha / 2)
    lower <- mean - zcrit * std_mean
    upper <- mean + zcrit * std_mean
  } else if (alt %in% c("larger", "l", "greater")) {
    zcrit <- qnorm(alpha)
    lower <- mean + zcrit * std_mean
    upper <- Inf
  } else if (alt %in% c("smaller", "s", "less")) {
    zcrit <- qnorm(1 - alpha)
    lower <- -Inf
    upper <- mean + zcrit * std_mean
  } else {
    zcrit <- qnorm(1 - alpha / 2)
    lower <- mean - zcrit * std_mean
    upper <- mean + zcrit * std_mean
  }
  
  return(cbind(lower = lower, upper = upper))
}

assignInNamespace("zconfint_generic", patched_zconf, ns = "ipd")
# =====================================================================
# PATCH 2: Fix the wrapper to support atomic vectors using [[]] instead of $
# =====================================================================
patched_ipd <- function(
    formula, method, model, data, label = NULL, unlabeled_data = NULL,
    intercept = TRUE, alpha = 0.05, alternative = "two-sided", na_action = "na.fail", ...
) {
  valid_methods <- c("chen", "pdc", "postpi_analytic", "postpi_boot", "ppi", "ppi_a", "ppi_plusplus", "pspa")
  valid_models <- c("mean", "quantile", "ols", "logistic", "poisson")
  
  all_vars    <- all.vars(formula)
  preds       <- all_vars[-c(1,2)]
  factor_vars <- intersect(preds, names(Filter(is.factor, data)))
  
  if (!is.null(label) && is.null(unlabeled_data)) {
    data <- .drop_unused_levels(data, factor_vars)
  }
  
  inp <- .parse_inputs(data, label, unlabeled_data, na_action)
  .warn_differing_levels(inp$data_l, inp$data_u, factor_vars)
  mats <- .build_design(formula, inp$data_l, inp$data_u, intercept, na_action)
  
  method <- match.arg(method, valid_methods)
  model  <- match.arg(model,  valid_models)
  
  helper <- get(paste(method, model, sep = "_"))
  fit <- helper(mats$X_l, mats$Y_l, mats$f_l, mats$X_u, mats$f_u, ...)
  
  # --- THE CRITICAL FIX ---
  # Replaced $ with [[ to prevent atomic vector crashes
  est <- as.numeric(fit[["est"]])
  se  <- as.numeric(fit[["se"]])
  # ------------------------
  
  nm  <- colnames(mats$X_u)
  names(est) <- names(se) <- nm
  
  ci_mat <- zconfint_generic(est, se, alpha, alternative)
  rownames(ci_mat) <- nm
  colnames(ci_mat) <- c("lower", "upper")
  
  zval <- est / se
  pval <- 2 * pnorm(-abs(zval))
  
  coef_tab <- data.frame(
    Estimate     = est,
    `Std. Error` = se,
    `z value`    = zval,
    `Pr(>|z|)`   = pval,
    row.names    = nm,
    check.names  = FALSE
  )
  
  new("ipd",
      coefficients = est,
      se           = se,
      ci           = ci_mat,
      coefTable    = coef_tab,
      fit          = as.list(fit), # Enforce list structure for S4 object
      formula      = formula,
      data_l       = inp$data_l,
      data_u       = inp$data_u,
      method       = method,
      model        = model,
      intercept    = intercept
  )
}
# Bind our patched wrapper directly to the internal package environment so it can see all the hidden .functions
environment(patched_ipd) <- asNamespace("ipd")
assignInNamespace("ipd", patched_ipd, ns = "ipd")