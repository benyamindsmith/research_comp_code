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

make_panel <- function(data, x, y, xlab, shape_values, ylab = NULL, sub_caption = NULL) {
  if (is.null(ylab)) ylab <- gsub("_", " ", y)  
  
  ggplot(data = data, mapping = aes(x = .data[[x]], y = .data[[y]],
                                    color = Estimator, shape = Estimator)) +
    geom_hline(yintercept = 0.9, linetype = "dashed", color = "black", linewidth = 0.6) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 3) +
    scale_color_brewer(palette = "Set1") +
    scale_shape_manual(values = shape_values) +
    # Use 'title' so it goes at the top of the plot
    labs(x = xlab, y = ylab, color = "Method", shape = "Method", title = sub_caption) +
    theme_paper +
    theme(
      # Overwrite element_blank() to show the title centered at the top
      plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 10))
    )
}
# Visuals

############
# Figure 1 #
############
p1 <- make_panel(
  data = mean_estimation_1, 
  x = "epsilon", 
  y = "Coverage", 
  xlab = expression(epsilon), 
  shape_values = c(15, 16, 17, 18, 4, 2),
  ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for EY")) 
)

p2 <- make_panel(
  data = mean_estimation_1, 
  x = "epsilon", 
  y = "Width_Ratio", 
  xlab = expression(epsilon), 
  shape_values = c(15, 16, 17, 18, 4, 2),
  ylab = "Width Ratio", 
  sub_caption = expression(paste(bold("(b) "), "Width ratio")) 
)

fig1 <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

fig1

############
# Figure 2 #
############
p3 <- make_panel(
  data = mean_estimation_2, 
  x = "n", 
  y = "Coverage", 
  xlab = expression(n), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8),
  ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for EY"))
)

p4 <- make_panel(
  data = mean_estimation_2, 
  x = "n", 
  y = "Width_Ratio", 
  xlab = expression(n), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8),
  ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig2 <- p3 + p4 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

fig2

############
# Figure 3 #
############
p5 <- make_panel(
  data = mean_estimation_3, 
  x = "N", 
  y = "Coverage", 
  xlab = expression(N), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8),
  ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for EY"))
)

p6 <- make_panel(
  data = mean_estimation_3, 
  x = "N", 
  y = "Width_Ratio", 
  xlab = expression(N), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8),
  ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig3 <- p5 + p6 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

fig3

############
# Figure 4 #
############
p7 <- make_panel(
  data = linear_regression_1, 
  x = "beta_1", 
  y = "Coverage", 
  xlab = expression(beta[1]), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8),
  ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for ", theta[(1)]))
)

p8 <- make_panel(
  data = linear_regression_1, 
  x = "beta_1", 
  y = "Width_Ratio", 
  xlab = expression(beta[1]), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8),
  ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig4 <- p7 + p8 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

fig4

############
# Figure 5 #
############
p9 <- make_panel(
  data = linear_regression_2, 
  x = "beta_1", 
  y = "Coverage", 
  xlab = expression(beta[1]), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8),
  ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for ", theta[(1)]))
)

p10 <- make_panel(
  data = linear_regression_2, 
  x = "beta_1", 
  y = "Width_Ratio", 
  xlab = expression(beta[1]), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8),
  ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig5 <- p9 + p10 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

fig5

############
# Figure 6 #
############
p11 <- make_panel(
  data = linear_regression_3, 
  x = "beta_1", 
  y = "Coverage", 
  xlab = expression(beta[1]), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8, 7, 14),
  ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for ", theta[(1)]))
)

p12 <- make_panel(
  data = linear_regression_3, 
  x = "beta_1", 
  y = "Width_Ratio", 
  xlab = expression(beta[1]), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8, 7, 14),
  ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig6 <- p11 + p12 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

fig6

############
# Figure 7 #
############
p13 <- make_panel(
  data = logistic_1, 
  x = "rho", 
  y = "Coverage", 
  xlab = expression(rho), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8, 7, 14),
  ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for ", theta[(1)]))
)

p14 <- make_panel(
  data = logistic_1, 
  x = "rho", 
  y = "Width_Ratio", 
  xlab = expression(rho), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8, 7, 14),
  ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig7 <- p13 + p14 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

fig7

############
# Figure 8 #
############
p15 <- make_panel(
  data = logistic_2, 
  x = "rho", 
  y = "Coverage", 
  xlab = expression(rho), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8, 7, 14),
  ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for ", theta[(1)]))
)

p16 <- make_panel(
  data = logistic_2, 
  x = "rho", 
  y = "Width_Ratio", 
  xlab = expression(rho), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8, 7, 14),
  ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig8 <- p15 + p16 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

fig8

############
# Figure 9 #
############
p17 <- make_panel(
  data = logistic_3, 
  x = "rho", 
  y = "Coverage", 
  xlab = expression(rho), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8, 7, 14),
  ylab = "Coverage",
  sub_caption = expression(paste(bold("(a) "), "Coverage probability for ", theta[(1)]))
)

p18 <- make_panel(
  data = logistic_3, 
  x = "rho", 
  y = "Width_Ratio", 
  xlab = expression(rho), 
  shape_values = c(15, 16, 17, 18, 4, 2, 8, 7, 14),
  ylab = "Width Ratio",
  sub_caption = expression(paste(bold("(b) "), "Width ratio"))
)

fig9 <- p17 + p18 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

fig9

# Save plots
plot_list <- list(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, fig9)
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