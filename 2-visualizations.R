library(ggplot2)
library(dplyr)
library(patchwork)
# Load Data
mean_estimation_1 <- readRDS("data/mean-estimation-1-results.rds")
mean_estimation_2 <- readRDS("data/mean-estimation-2-results.rds")
mean_estimation_3 <- readRDS("data/mean-estimation-3-results.rds")

linear_regression_1 <- readRDS("data/linear-regression-1-results.rds")
linear_regression_2 <- readRDS("data/linear-regression-2-results.rds")
linear_regression_3 <- readRDS("data/linear-regression-3-results.rds")

theme_paper_p1 <- theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 12, color = "black"),
    legend.position = c(0.82, 0.31),          # inside the panel, like the reference
    legend.background = element_rect(fill = "white", color = "white", linewidth = 0.3),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key = element_rect(fill = "white"),
    plot.title = element_blank()
  )


theme_paper_p2 <- theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 12, color = "black"),
    legend.position = c(0.12, 0.77),          # inside the panel, like the reference
    legend.background = element_rect(fill = "white", color = "white", linewidth = 0.3),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.key = element_rect(fill = "white"),
    plot.title = element_blank()
  )

# Replicating the visuals 

# Mean Estimation Visuals
# NOTE: PDC is the same as PPI++ in this setting
# Need to rename the data. 
# "PDC" in the original data is a modification of Zhang
# As stated by the authors. 

mean_estimation_1 <- mean_estimation_1|>
  dplyr::filter(Estimator %in% c("Naive", "PPI", "PPI++", "Song","Zhang"))|>
  dplyr::mutate(Estimator = dplyr::case_match(Estimator,
                                               "PPI++" ~ "PDC", 
                                               "Naive" ~ "SUP", 
                                               "Zhang" ~ "SEMI",
                                               "Song" ~ "SONG", 
                                              "PSPA" ~ "POP-Inf"
                                               .default = Estimator)|>
                  factor(x=_, levels = c("PDC", "SEMI", "PPI", "SUP", "SONG","POP-Inf")))
# Figure 1

p1 <- ggplot(data = mean_estimation_1, mapping = aes(x = epsilon, y = Coverage, color = Estimator, shape = Estimator)) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black", linewidth = 0.6) +  
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) + 
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(15, 16, 17, 18, 4)) + 
  labs(
    x = expression(epsilon),
    y = "Coverage",
    color = "Method",
    shape = "Method"
  ) +
  theme_paper_p1

p2 <- ggplot(data = mean_estimation_1, mapping = aes(x = epsilon, y = Width_Ratio, color = Estimator, shape = Estimator)) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black", linewidth = 0.6) +  
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) + 
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(15, 16, 17, 18, 4)) + 
  labs(
    x = expression(epsilon),
    y = "Width Ratio",
    color = "Method",
    shape = "Method"
  ) +
  theme_paper_p2

# Figure 1
p1+p2