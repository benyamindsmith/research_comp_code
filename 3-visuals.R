library(ggplot2)
library(patchwork)

# Helper Functions

theme_paper <- theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 12, color = "black"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.background = element_rect(fill = "white", color = "white", linewidth = 0.3),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key = element_rect(fill = "white"),
    plot.title = element_blank()
  )

make_panel <- function(data, x, y, xlab, shape_values, ylab = NULL) {
  if (is.null(ylab)) ylab <- gsub("_", " ", y)  # e.g. "Width_Ratio" -> "Width Ratio"
  
  ggplot(data = data, mapping = aes(x = .data[[x]], y = .data[[y]],
                                    color = Estimator, shape = Estimator)) +
    geom_hline(yintercept = 0.9, linetype = "dashed", color = "black", linewidth = 0.6) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    scale_color_brewer(palette = "Set1") +
    scale_shape_manual(values = shape_values) +
    labs(x = xlab, y = ylab, color = "Method", shape = "Method") +
    theme_paper
}

# Visuals


############
# Figure 1 #
############
p1 <- make_panel(mean_estimation_1, "epsilon", "Coverage", expression(epsilon), c(15, 16, 17, 18, 4, 2))
p2 <- make_panel(mean_estimation_1, "epsilon", "Width_Ratio", expression(epsilon), c(15, 16, 17, 18, 4, 2))

# one legend for the whole figure, pulled to the bottom
p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

############
# Figure 2 #
############
p3 <- make_panel(mean_estimation_2, "n", "Coverage", expression(n), c(15, 16, 17, 18, 4, 2, 8))
p4 <- make_panel(mean_estimation_2, "n", "Width_Ratio", expression(n), c(15, 16, 17, 18, 4, 2, 8))

p3 + p4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

############
# Figure 3 #
############
p5 <- make_panel(mean_estimation_3, "N", "Coverage", expression(N), c(15, 16, 17, 18, 4, 2, 8))
p6 <- make_panel(mean_estimation_3, "N", "Width_Ratio", expression(N), c(15, 16, 17, 18, 4, 2, 8))

p5 + p6 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

############
# Figure 4 #
############
p7 <- make_panel(linear_regression_1, "beta_1", "Coverage", expression(beta[1]), c(15, 16, 17, 18, 4, 2, 8))
p8 <- make_panel(linear_regression_1, "beta_1", "Width_Ratio", expression(beta[1]), c(15, 16, 17, 18, 4, 2, 8))

p7 + p8 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

############
# Figure 5 #
############
p9  <- make_panel(linear_regression_2, "beta_1", "Coverage", expression(beta[1]), c(15, 16, 17, 18, 4, 2, 8))
p10 <- make_panel(linear_regression_2, "beta_1", "Width_Ratio", expression(beta[1]), c(15, 16, 17, 18, 4, 2, 8))

p9 + p10 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

############
# Figure 6 #
############
p11 <- make_panel(linear_regression_3, "beta_1", "Coverage", expression(beta[1]), c(15, 16, 17, 18, 4, 2, 8,7,14))
p12 <- make_panel(linear_regression_3, "beta_1", "Width_Ratio", expression(beta[1]), c(15, 16, 17, 18, 4, 2, 8,7,14))

p11 + p12 + plot_layout(guides = "collect") & theme(legend.position = "bottom")