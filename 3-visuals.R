library(ggplot2)
library(patchwork)

# Fixed colour and shape per estimator (same in every figure)
estimator_colors <- c(
  "PDC"       = "#E41A1C",  # red
  "PDC1"     = "#FB6A4A",  # light red
  "PDC2"     = "#99000D",  # dark red
  "SEMI"      = "#377EB8",  # blue
  "PPI"       = "#4DAF4A",  # green
  "SUP"       = "#984EA3",  # purple
  "PPI++"     = "#FF7F00",  # orange
  "SONG"      = "#A65628",  # brown
  "POP-Inf"   = "#666666",  # grey
  "CC" = "#17BECF"   # teal
)

estimator_shapes <- c(
  "PDC"       = 15,
  "PDC1"     = 0,
  "PDC2"     = 22,
  "SEMI"      = 16,
  "PPI"       = 17,
  "SUP"       = 18,
  "PPI++"     = 4,
  "SONG"      = 2,
  "POP-Inf"   = 8,
  "CC" = 3
)

stopifnot(
  setequal(names(estimator_colors), estimator_levels),
  setequal(names(estimator_shapes), estimator_levels)
)

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

make_panel <- function(data, x, y, xlab, ylab = NULL, sub_caption = NULL) {
  if (is.null(ylab)) ylab <- gsub("_", " ", y)
  
  ggplot(data = data, mapping = aes(x = .data[[x]], y = .data[[y]],
                                    color = Estimator, shape = Estimator)) +
    geom_hline(yintercept = 0.9, linetype = "dashed", color = "black", linewidth = 0.6) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    # Named values: each estimator keeps its colour/shape; unused ones are dropped
    scale_color_manual(values = estimator_colors) +
    scale_shape_manual(values = estimator_shapes) +
    labs(x = xlab, y = ylab, color = "Method", shape = "Method", title = sub_caption) +
    theme_paper +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, margin = ggplot2::margin(b = 10))
    )
}

# Visuals

############
# Figure 1 #
############
p1 <- make_panel(
  data = mean_estimation_1, x = "epsilon", y = "Coverage",
  xlab = expression(epsilon), ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for EY"))
)

p2 <- make_panel(
  data = mean_estimation_1, x = "epsilon", y = "Width_Ratio",
  xlab = expression(epsilon), ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig1 <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
fig1

############
# Figure 2 #
############
p3 <- make_panel(
  data = mean_estimation_2, x = "n", y = "Coverage",
  xlab = expression(n), ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for EY"))
)

p4 <- make_panel(
  data = mean_estimation_2, x = "n", y = "Width_Ratio",
  xlab = expression(n), ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig2 <- p3 + p4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
fig2

############
# Figure 3 #
############
p5 <- make_panel(
  data = mean_estimation_3, x = "N", y = "Coverage",
  xlab = expression(N), ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for EY"))
)

p6 <- make_panel(
  data = mean_estimation_3, x = "N", y = "Width_Ratio",
  xlab = expression(N), ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig3 <- p5 + p6 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
fig3

############
# Figure 4 #
############
p7 <- make_panel(
  data = linear_regression_1, x = "beta_1", y = "Coverage",
  xlab = expression(beta[1]), ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for ", theta[(1)]))
)

p8 <- make_panel(
  data = linear_regression_1, x = "beta_1", y = "Width_Ratio",
  xlab = expression(beta[1]), ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig4 <- p7 + p8 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
fig4

############
# Figure 5 #
############
p9 <- make_panel(
  data = linear_regression_2, x = "beta_1", y = "Coverage",
  xlab = expression(beta[1]), ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for ", theta[(1)]))
)

p10 <- make_panel(
  data = linear_regression_2, x = "beta_1", y = "Width_Ratio",
  xlab = expression(beta[1]), ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig5 <- p9 + p10 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
fig5

############
# Figure 6 #
############
p11 <- make_panel(
  data = linear_regression_3, x = "beta_1", y = "Coverage",
  xlab = expression(beta[1]), ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for ", theta[(1)]))
)

p12 <- make_panel(
  data = linear_regression_3, x = "beta_1", y = "Width_Ratio",
  xlab = expression(beta[1]), ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig6 <- p11 + p12 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
fig6

# Save plots
plot_list <- list(fig1, fig2, fig3, fig4, fig5, fig6)
for (i in seq_along(plot_list)) {
  ggsave(
    filename = paste0("figs/fig", i, ".png"),
    plot = plot_list[[i]],
    width = 1231 * 3,
    height = 597 * 3,
    units = "px",
    dpi = 300
  )
}