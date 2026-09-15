library(dplyr)
library(tidyr)
library(kableExtra)

dir.create("tables", showWarnings = FALSE)

canonical_order <- c("Naive", "SEMI", "Zhang", "Azriel",
                     "PDC", "PDC1", "PDC2",
                     "PPI", "PPI++", "PSPA", "Song")

order_estimators <- function(estimator_values) {
  present <- unique(as.character(estimator_values))
  c(canonical_order[canonical_order %in% present],
    sort(setdiff(present, canonical_order)))
}

fmt_cell <- function(coverage, width, digits = 3,
                     nominal = 0.90, n_sims = 1000, flag_mult = 2) {
  # Bold any cell whose coverage deviates from the nominal level by more
  # than `flag_mult` Monte Carlo standard errors, i.e. a statistically
  # meaningful departure from nominal coverage given n_sims replications
  # -- not an arbitrary round-number cutoff.
  threshold <- flag_mult * sqrt(nominal * (1 - nominal) / n_sims)
  txt <- sprintf("%.*f (%.*f)", digits, coverage, digits, width)
  flagged <- abs(coverage - nominal) > threshold
  ifelse(flagged, sprintf("\\textbf{%s}", txt), txt)
}

# ============================================================
# Converts a kableExtra booktabs table (no vertical rules, sparse
# horizontal rules) into a fully boxed grid: outer border, a
# vertical rule after the first (sweep-variable) column, and a
# horizontal rule separating each row-group.
#   n_est: number of estimator columns (excludes the sweep column)
# ============================================================
box_table <- function(tab, n_est) {
  lines <- strsplit(as.character(tab), "\n")[[1]]
  out <- character(0)
  seen_group <- FALSE
  
  for (ln in lines) {
    if (grepl("^\\\\begin\\{tabular\\}", ln)) {
      new_spec <- sprintf("{|c|%s|}", paste(rep("c", n_est), collapse = ""))
      ln <- sub("\\{[clr]+\\}\\s*$", new_spec, ln)
      out <- c(out, ln)
    } else if (grepl("^\\\\(toprule|midrule|bottomrule)", ln)) {
      out <- c(out, "\\hline")
    } else if (grepl("^\\\\addlinespace", ln)) {
      next   # drop; replaced by \hline before each group below
    } else if (grepl("^\\\\multicolumn\\{", ln)) {
      if (seen_group) out <- c(out, "\\hline")
      seen_group <- TRUE
      ln <- sub("\\\\multicolumn\\{(\\d+)\\}\\{l\\}",
                "\\\\multicolumn{\\1}{|l|}", ln)
      out <- c(out, ln)
    } else {
      out <- c(out, ln)
    }
  }
  paste(out, collapse = "\n")
}

# ============================================================
# 1. LOGISTIC REGRESSION -- one table, DGM1/2/3 as row groups
# ============================================================
dgm_labels <- c(
  dgm1 = "DGM1 (correctly specified)",
  dgm2 = "DGM2 (link misspecification)",
  dgm3 = "DGM3 (omitted interaction)"
)

logistic_all <- bind_rows(lapply(1:3, function(i)
  readRDS(sprintf("data/logistic-%d-results.rds", i))
))

est_order <- order_estimators(logistic_all$Estimator)

logistic_wide <- logistic_all %>%
  mutate(cell = fmt_cell(Coverage, Width_Ratio),
         Estimator = factor(Estimator, levels = est_order),
         dgm = factor(dgm, levels = names(dgm_labels))) %>%
  select(dgm, rho, Estimator, cell) %>%
  pivot_wider(names_from = Estimator, values_from = cell) %>%
  arrange(dgm, rho) %>%
  select(dgm, rho, all_of(est_order))   # pivot_wider does NOT respect factor
# level order for column placement --
# must reorder explicitly or kbl's
# col.names will mislabel columns

group_sizes <- table(logistic_wide$dgm)[names(dgm_labels)]
names(group_sizes) <- dgm_labels[names(group_sizes)]

logistic_tab <- logistic_wide %>%
  select(-dgm) %>%
  kbl(format = "latex", booktabs = TRUE,
      align = c("c", rep("c", length(est_order))),
      col.names = c("$\\rho$", est_order),
      caption = "Empirical coverage and relative width (in parentheses) for logistic regression across data-generating mechanisms. Each cell reports 90\\% CI coverage and the ratio of the estimator's SE to the Naive estimator's SE, averaged over 1{,}000 replications. \\textbf{Bold} entries indicate coverage differing from the nominal 90\\% level by more than two Monte Carlo standard errors ($\\pm 0.019$).",
      label = "logistic-results", escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  pack_rows(index = group_sizes)

writeLines(box_table(logistic_tab, length(est_order)), "tables/logistic-table.tex")
cat("Saved tables/logistic-table.tex\n")

# ============================================================
# 2. LINEAR REGRESSION -- one table, three scenarios as row
#    groups. Scenario 3 has extra PDC1/PDC2 columns; missing
#    cells in scenarios 1-2 are filled with "--".
#    ASSUMPTION: replace [DESCRIBE] below with the actual
#    distinguishing description for each of the three files.
# ============================================================
linear_scenario_labels <- c(
  "1" = "Scenario 1 [DESCRIBE]",
  "2" = "Scenario 2 [DESCRIBE]",
  "3" = "Scenario 3 [DESCRIBE]"
)

linear_all <- bind_rows(lapply(1:3, function(i) {
  readRDS(sprintf("data/linear-regression-%d-results.rds", i)) %>%
    mutate(scenario = as.character(i))
}))

est_order_lin <- order_estimators(linear_all$Estimator)

linear_wide <- linear_all %>%
  mutate(cell = fmt_cell(Coverage, Width_Ratio),
         Estimator = factor(Estimator, levels = est_order_lin),
         scenario = factor(scenario, levels = names(linear_scenario_labels))) %>%
  select(scenario, beta_1, Estimator, cell) %>%
  pivot_wider(names_from = Estimator, values_from = cell,
              values_fill = "--") %>%
  arrange(scenario, beta_1) %>%
  select(scenario, beta_1, all_of(est_order_lin))   # explicit reorder -- see note above

group_sizes_lin <- table(linear_wide$scenario)[names(linear_scenario_labels)]
names(group_sizes_lin) <- linear_scenario_labels[names(group_sizes_lin)]

linear_tab <- linear_wide %>%
  select(-scenario) %>%
  kbl(format = "latex", booktabs = TRUE,
      align = c("c", rep("c", length(est_order_lin))),
      col.names = c("$\\beta_1$", est_order_lin),
      caption = "Empirical coverage and relative width (in parentheses) for linear regression across scenarios. `--' indicates an estimator variant not evaluated in that scenario. Averaged over 1{,}000 replications. \\textbf{Bold} entries indicate coverage differing from the nominal 90\\% level by more than two Monte Carlo standard errors ($\\pm 0.019$).",
      label = "linear-results", escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  pack_rows(index = group_sizes_lin)

writeLines(box_table(linear_tab, length(est_order_lin)), "tables/linear-regression-table.tex")
cat("Saved tables/linear-regression-table.tex\n")

# ============================================================
# 3. MEAN ESTIMATION -- one table, epsilon/n/N sweeps as row
#    groups. Column header is generic ("Value") since the units
#    differ by group; the row-group label states which
#    parameter and its units.
#    ASSUMPTION: epsilon = contamination/misspecification level,
#    n = labeled sample size, N = unlabeled sample size --
#    confirm this matches your actual design.
# ============================================================
mean_specs <- list(
  list(file = "data/mean-estimation-1-results.rds", sweep_var = "epsilon",
       label = "Varying $\\epsilon$ (contamination level)", digits = 1),
  list(file = "data/mean-estimation-2-results.rds", sweep_var = "n",
       label = "Varying $n$ (labeled sample size)", digits = 0),
  list(file = "data/mean-estimation-3-results.rds", sweep_var = "N",
       label = "Varying $N$ (unlabeled sample size)", digits = 0)
)

mean_all <- bind_rows(lapply(mean_specs, function(spec) {
  readRDS(spec$file) %>%
    rename(value = all_of(spec$sweep_var)) %>%
    mutate(group = spec$label)
}))

est_order_mean <- order_estimators(mean_all$Estimator)
group_levels <- vapply(mean_specs, function(s) s$label, character(1))
group_digits <- setNames(vapply(mean_specs, function(s) s$digits, numeric(1)), group_levels)

mean_wide <- mean_all %>%
  mutate(cell = fmt_cell(Coverage, Width_Ratio),
         Estimator = factor(Estimator, levels = est_order_mean),
         group = factor(group, levels = group_levels)) %>%
  select(group, value, Estimator, cell) %>%
  pivot_wider(names_from = Estimator, values_from = cell) %>%
  arrange(group, value) %>%
  mutate(value = sprintf(paste0("%.", group_digits[as.character(group)], "f"), value)) %>%
  select(group, value, all_of(est_order_mean))   # explicit reorder -- see note above

group_sizes_mean <- table(mean_wide$group)[group_levels]

mean_tab <- mean_wide %>%
  select(-group) %>%
  kbl(format = "latex", booktabs = TRUE,
      align = c("c", rep("c", length(est_order_mean))),
      col.names = c("Value", est_order_mean),
      caption = "Empirical coverage and relative width (in parentheses) for mean estimation. Averaged over 1{,}000 replications. \\textbf{Bold} entries indicate coverage differing from the nominal 90\\% level by more than two Monte Carlo standard errors ($\\pm 0.019$).",
      label = "mean-results", escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  pack_rows(index = group_sizes_mean)

writeLines(box_table(mean_tab, length(est_order_mean)), "tables/mean-estimation-table.tex")
cat("Saved tables/mean-estimation-table.tex\n")

cat("\nDone -- 3 tables total in tables/\n")