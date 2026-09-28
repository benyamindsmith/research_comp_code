
# Table Functions
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

