plot_results <- function(df, x_title = " ") {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Pivot longer
  plot_df <- df %>%
    pivot_longer(
      cols = c(mean_ci_width, cp),
      names_to = "metric",
      values_to = "value"
    ) %>%
    filter(!Method %in% c("naive", "true")) %>%
    mutate(
      Method = factor(Method, levels = c("classical", "ppi", "pdc", "chen-chen")),
      param = factor(param)
    )
  
  # Facet labels
  facet_labels <- c(
    mean_ci_width = "95% Confidence Interval Width",
    cp            = "Coverage Probability"
  )
  
  method_colors <- c(
    "classical" = "#d62728", # red
    "ppi"       = "#ff7f0e",
    "pdc"       = "#2ca02c",
    "chen-chen" = "#1f77b4"  # blue
  )
  
  # Plot
  ggplot(plot_df, aes(x = param, y = value, group = Method)) +
    # CI width: bars
    geom_col(
      data = subset(plot_df, metric == "mean_ci_width"),
      aes(fill = Method),
      position = position_dodge(width = 0.8),
      width = 0.7,
      color = "black"
    ) +
    # CP: line + points
    geom_line(
      data = subset(plot_df, metric == "cp"),
      aes(color = Method),
      linewidth = 1
    ) +
    geom_point(
      data = subset(plot_df, metric == "cp"),
      aes(color = Method),
      size = 3
    ) +
    geom_hline(
      data = subset(plot_df, metric == "cp"),
      aes(yintercept = 0.95),
      linetype = "dashed",
      linewidth = 0.8,
      color = "black"
    ) +
    # Facets
    facet_wrap(~ metric, scales = "free_y", labeller = labeller(metric = facet_labels)) +
    facetted_pos_scales(
      y = list(
        metric == "cp" ~ scale_y_continuous(limits = c(0.9, 1)),
        metric == "mean_ci_width" ~ scale_y_continuous()
      )) +
    # Colors
    scale_fill_manual(values = method_colors,
                      labels = c("Classical", "PPI", "PDC", "CC")) +
    scale_color_manual(values = method_colors,
                       labels = c("Classical", "PPI", "PDC", "CC")) +
    # Labels
    labs(x = x_title, y = NULL, fill = "Method", color = "Method") +
    # Theme
    theme_bw() +
    theme(
      axis.title.x = element_text(size = 16),
      axis.text.x  = element_text(size = 14),
      axis.text.y  = element_text(size = 14),
      strip.text   = element_text(size = 16, face = "bold"),
      legend.position = "right"
    )
}
