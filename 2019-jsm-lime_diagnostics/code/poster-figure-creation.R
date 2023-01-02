## 2019 JSM Poster
## Purpose: create figures for the poster
## Date Created: 2019/06/24
## Date Last Updated: 2019/07/11

#### Set Up ----------------------------------------------------------------------

# Load packages
library(cowplot)
library(gower)
library(gretchenalbrecht)
library(magick)
library(tidyverse)

#### Conceptual Depiction of LIME ------------------------------------------------

# Generate data
set.seed(20190624)
lime_data <- data.frame(feature1 = sort(runif(250, 0, 1)),
                      feature2 = sample(x = 1:250, size = 250, replace = FALSE)) %>%
  mutate(prediction = if_else(feature1 >= 0 & feature1 < 0.1, (0.3 * feature1) + rnorm(n(), 0, 0.01), 
                      if_else(feature1 >= 0.1 & feature1 < 0.3, rbeta(n(), 1, 0.5), 
                      if_else(feature1 >= 0.3 & feature1 < 0.5, sin(pi* feature1) + rnorm(n(), 0, 0.5),
                      if_else(feature1 >= 0.5 & feature1 < 0.8, -(sin(pi* feature1) + rnorm(n(), 0, 0.1)) + 1,
                      if_else(feature1 >= 0.8 & feature1 < 0.9, 0.5 + runif(n(), -0.5, 0.5), 
                              0.5 + rnorm(n(), 0, 0.3)))))))

# Specify a prediction of interest
prediction_of_interest <- data.frame(feature1 = 0.07,
                                feature2 = 200,
                                prediction = 0.05, 
                                color = factor("Prediction of Interest"))

# Compute the distance between the prediction of interest 
lime_data$distance <- (1 - gower_dist(x = prediction_of_interest, y = lime_data))^10

# Fit the interpretable explainer model
lime_explainer <- lm(prediction ~ feature1 + feature2, data = lime_data, weights = distance)

# Create a title for the lime figure
lime_title <- ggdraw() + 
  draw_label(label = "Conceptual Depiction of LIME",
             hjust = -0.01,
             x = 0,
             fontfamily = "Avenir",
             size = 36)

# Predictions versus feature 1
plot_feature1 <- ggplot(lime_data, aes(x = feature1, y = prediction, size = distance)) + 
  geom_point() + 
  geom_point(data = prediction_of_interest,
             mapping = aes(x = feature1, y = prediction),
             color = "#EFB084",
             size = 5) +
  geom_abline(intercept = coef(lime_explainer)[[1]] + coef(lime_explainer)[[3]]*prediction_of_interest$feature2, 
              slope = coef(lime_explainer)[[2]], 
              size = 1) +
  theme_linedraw(base_family = "Avenir",
                 base_size = 22) +
  labs(x = "Feature 1", 
       y = "Black-Box Prediction") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

# Predictions versus feature 2
plot_feature2 <- ggplot(lime_data, aes(x = feature2, y = prediction, size = distance)) + 
  geom_point() + 
  geom_point(data = prediction_of_interest,
             mapping = aes(x = feature2, y = prediction),
             color = "#EFB084",
             size = 5) +
  geom_abline(intercept = coef(lime_explainer)[[1]] + coef(lime_explainer)[[2]]*prediction_of_interest$feature1, 
              slope = coef(lime_explainer)[[3]], 
              size = 1) +
  theme_linedraw(base_family = "Avenir",
                 base_size = 22) +
  labs(x = "Feature 2", 
       y = "Black-Box Prediction") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

# Join the two feature plots
lime_plots <- plot_grid(plot_feature1, plot_feature2)

# Create a plot to extract a legend for the prediction of interest
lime_legend_plot <- ggplot(prediction_of_interest, aes(x = feature1, y = prediction, color = color)) + 
  geom_point(size = 5) +
  scale_color_manual(values = c("#EFB084")) +
  theme_classic(base_family = "Avenir", 
                base_size = 32) +
  labs(color = "") +
  theme(legend.position = "bottom")

# Extract the legend
lime_legend <- get_legend(lime_legend_plot)

# Create a caption for the lime figure
lime_caption <- ggdraw() + 
  draw_label(
label = 
"Linear regression models weighted by proximity
to prediction of interest",
             hjust = -0.01,
             x = 0,
             fontfamily = "Avenir", 
             size = 30)

# Join the title, plots, legend, and caption into one figure
lime_figure <- plot_grid(lime_title, lime_plots, lime_legend, lime_caption, 
                         ncol = 1, 
                         rel_heights = c(0.1, 0.6, 0.1, 0.2),
                         scale = 0.98)

# View the figure
lime_figure

# Save the figure
ggsave(filename = "./posters/2019-jsm-lime_diagnostics/figures/lime_concept.png",
       plot = lime_figure, 
       units = "in", 
       width = 9.5, 
       height = 5.5, 
       dpi = 300)

#### Diagnostic Plots Templates --------------------------------------------------

# Specify the number of cases and LIME input options
ncases = 10
ninputs = 5

# Create title for diagnostics plots
diagnostic_title <- ggdraw() + 
  draw_label(label = "Templates of Diagnostic Plots",
             hjust = -0.01,
             x = 0,
             fontfamily = "Avenir",
             size = 38)

# Create mse example data
set.seed(20190627)
mse_data <- data.frame(input = factor(c("A", "B", "C", "D", "E")),
                       mse = runif(ninputs, 0.01, 0.1))

# Create the plot of the MSEs
mse_plot <- ggplot(mse_data, aes(x = input, y = mse)) + 
  geom_point(size = 3) + 
  labs(x = "Method", 
       y = "MSE") + 
  theme_linedraw(base_family = "Avenir",
                 base_size = 20) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none") +
  scale_color_gretchenalbrecht(palette = "last_rays", discrete = TRUE)

# Create a caption for the lime figure
mse_caption <- ggdraw() + 
  draw_label(
    label = 
"MSE Comparison: Lowest 
MSE suggests best 
approximation of simple 
model to complex model", 
    hjust = -0.01,
    x = 0,
    fontfamily = "Avenir",
    size = 28)

# Join the mse plot and caption
mse_figure <- plot_grid(mse_plot, mse_caption, 
                        ncol = 2, 
                        rel_widths = c(0.45, 0.55),
                        scale = 0.95)

# Create chosen feature example data
set.seed(20190627)
heatmap_data <- tibble(case = factor(rep(1:ncases, ninputs), levels = ncases:1),
                       input = factor(rep(1:ninputs, each = ncases)),
                       feature = factor(c(rep(1, ncases),
                                          c(rep(2, ncases * 0.9), rep(3, ncases *0.1)),
                                          c(rep(1, ncases * 0.3), rep(2, ncases * 0.2), rep(3, ncases * 0.3), rep(4, ncases * 0.2)),
                                          c(rep(3, ncases * 0.2), rep(2, ncases * 0.3), rep(4, ncases *0.5)),
                                          sort(sample(1:4, ncases, replace = TRUE))))) %>%
  mutate(input = forcats::fct_recode(input, "A" = "1", "B" = "2", "C" = "3", "D" = "4", "E" = "5"))

# Create the conceptual heatmap for assessing "localness" of explanations
feature_plot <- ggplot(data = heatmap_data, mapping = aes(x = input, y = case, fill = feature)) + 
  geom_tile() + 
  labs(x = "Method", 
       y = "Case", 
       fill = "Feature") + 
  theme_linedraw(base_family = "Avenir",
                 base_size = 20) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_fill_grey()

# Create a caption for the lime figure
feature_caption <- ggdraw() + 
  draw_label(
    label = 
"Top Features Comparison:
Horizontal stripes suggest
consistency across 
methods and vertical 
stripes suggest 
non-local explanations", 
    hjust = -0.01,
    x = 0,
    fontfamily = "Avenir",
    size = 28)

# Join the mse plot and caption
feature_figure <- plot_grid(feature_plot, feature_caption, 
                        ncol = 2, 
                        rel_widths = c(0.45, 0.55),
                        scale = 0.95)

# Join the title, plots, legend, and caption into one figure
diagnostic_figure <- plot_grid(diagnostic_title, mse_figure, feature_figure,
                             ncol = 1, 
                             rel_heights = c(0.1, 0.35, 0.55),
                             scale = 0.98)

# View the heatmap
diagnostic_figure

# Save the figure
ggsave(filename = "./posters/2019-jsm-lime_diagnostics/figures/diagnostic_template.png",
       plot = diagnostic_figure, 
       units = "in", 
       width = 9.5, 
       height = 7, 
       dpi = 300)

#### Bullet Figures --------------------------------------------------------------

# Create a title for the heatmap figure
bullet_title <- ggdraw() + 
  draw_label(label = "Bullet Comparisons",
             hjust = -0.01,
             x = 0,
             fontfamily = "Avenir",
             size = 46)

# Load image of the bullet
bullet <- ggdraw() +
  draw_image('posters/2019-jsm-lime_diagnostics/figures/bullet.png')

# Load plot of the signatures
signatures <- ggdraw() +
  draw_image('posters/2019-jsm-lime_diagnostics/figures/signatures.png')

# Join bullet and signatures
bullet_plot <- plot_grid(bullet, signatures,
          ncol = 2, 
          rel_widths = c(0.3, 0.7),
          scale = 0.98)

# Create a caption for the lime figure
bullet_caption <- ggdraw() + 
  draw_label(
    label =
"Grooves created when bullet is fired 
extracted through high definition scans and 
used to create 'signatures' to identify bullets 
fired from the same gun",
    hjust = -0.01,
    x = 0,
    fontfamily = "Avenir", 
    size = 36)

# Join the text and images
bullet_figure <- plot_grid(bullet_title, bullet_plot, bullet_caption,
                        ncol = 1, 
                        rel_heights = c(0.1, 0.55, 0.35),
                        scale = 0.98)

# Print the bullet figure
bullet_figure

# Save the figure
ggsave(filename = "./posters/2019-jsm-lime_diagnostics/figures/bullet_comparisons.png",
       plot = bullet_figure,
       units = "in", 
       width = 10.5, 
       height = 7.5, 
       dpi = 300)

#### Bullet RF MSE Comparisons ---------------------------------------------------

# Create a title for the bullet mse comparison plot
bullet_mse_title <- ggdraw() + 
  draw_label(label = "MSE Implementation Comparisons",
             hjust = -0.01,
             x = 0,
             fontfamily = "Avenir",
             size = 44)

# Read in the lime comparison data
# Version copied from lime-to-bullets project on April 7, 2019
hamby224_lime_comparisons <- readRDS("./posters/2019-jsm-lime_diagnostics/data/hamby224_lime_comparisons.rds")

# Summarizing lime results
hamby224_lime_results <- hamby224_lime_comparisons %>%
  filter(!is.na(rfscore)) %>%
  group_by(situation, bin_situation, bin_continuous, quantile_bins, nbins, use_density, 
           bin_method, response, set) %>%
  summarise(mse = (sum(diff^2)) / length(diff)) %>%
  arrange(set) %>%
  ungroup()

# Create plot of mse
bullet_mse_plot <- hamby224_lime_results %>%
  filter(set == "Set 1") %>%
  mutate(bins = ifelse(nbins == 4 & bin_continuous == FALSE, "other", nbins)) %>%
  select(bin_situation, bins, mse) %>%
  rename("MSE" = "mse") %>%
  mutate(new_bins = if_else(bin_situation == "kernel density", "Kernel",
                            if_else(bin_situation == "normal approximation", "Normal",
                                    bins)),
         bin_situation = fct_recode(bin_situation, "Equal Bins" = "equally spaced", 
                                    "Quantile Bins" = "quantile", 
                                    "Other" = "kernel density",
                                    "Match Tree" = "samesource tree",
                                    "RF Prob Tree" = "rfscore tree",
                                    "Other" = "normal approximation"),
         bin_situation = factor(bin_situation, 
                                levels = c("Equal Bins", "Quantile Bins",
                                           "RF Prob Tree", "Match Tree", 
                                           "Other"))) %>%
  ggplot(aes(x = new_bins, y = MSE, color = bin_situation)) +
  geom_point(size = 3) + 
  facet_grid(. ~ bin_situation, scales = "free") + 
  theme_bw() + 
  theme(legend.position = "none",
        text = element_text(size = 20, family = "Avenir")) +
  labs(x = "Number of Bins", y = "MSE", color = "") + 
  scale_color_gretchenalbrecht(palette = "last_rays", discrete = TRUE)

# Create a caption for the lime figure
bullet_mse_caption <- ggdraw() + 
  draw_label(
    label =
"No one obvious best implementation 
method and quantile bins do not decrease
as the number of bins increases as expected",
    hjust = -0.01,
    x = 0,
    fontfamily = "Avenir", 
    size = 38)

# Join the text and images
bullet_mse_figure <- plot_grid(bullet_mse_title, bullet_mse_plot, bullet_mse_caption,
                           ncol = 1, 
                           rel_heights = c(0.075, 0.725, 0.2),
                           scale = 0.98)

# Print the figure
bullet_mse_figure 

# Save the plot
ggsave(filename = "./posters/2019-jsm-lime_diagnostics/figures/bullet_mses.png",
       plot = bullet_mse_figure, 
       width = 11, 
       height = 10,  
       units = c("in"))

#### Bullet RF Features Comparisons ----------------------------------------------

# Create a title for the bullet mse comparison plot
bullet_feature_title <- ggdraw() + 
  draw_label(label = "Top Features Selected by LIME",
             hjust = -0.01,
             x = 0,
             fontfamily = "Avenir",
             size = 44)

# Read in the test_explain data
hamby224_test_explain <- readRDS("./posters/2019-jsm-lime_diagnostics/data/hamby224_test_explain.rds")

# Create the chosen features dataset
chosen_features <- hamby224_test_explain %>%
  filter(!(bin_situation %in% c("kernel density", "normal approximation"))) %>%
  filter(!is.na(rfscore)) %>%
  select(set, situation, bin_situation, nbins, case, samesource, feature, feature_weight) %>%
  mutate(feature_weight_abs = abs(feature_weight)) %>%
  arrange(situation, case, desc(feature_weight_abs)) %>%
  mutate(explainer = rep(c("first", "second", "third"), length(situation) / 3)) %>%
  select(-feature_weight, -feature_weight_abs) %>%
  spread(explainer, feature)

# Plot of the third feature to get the nine colors from the gretchenalbrecht package
plot_to_get_nine_colors <- chosen_features %>%
  group_by(bin_situation, case) %>%
  mutate(nlevels = length(levels(factor(third)))) %>%
  ungroup() %>%
  arrange(bin_situation, set, samesource, desc(nlevels), case) %>%
  mutate(order = rep(rep(1:length(unique(case)), each = 5, 4))) %>%
  ggplot(aes(x = situation, y = order, fill = third)) + 
  geom_tile() + 
  facet_grid(set + samesource ~ bin_situation, scales = "free", space = "free_y") + 
  scale_fill_gretchenalbrecht(palette = "last_rays", discrete = TRUE)

# Grab the data from the plot and get the color palette
plot_data <- ggplot_build(plot_to_get_nine_colors)
palette <- unique(plot_data$data[[1]]["fill"])

# Order the colors correctly for the plot
colors <- c("ccf" = palette$fill[9], 
            "cms" = palette$fill[6], 
            "D" = palette$fill[3], 
            "matches" = palette$fill[5],
            "mismatches" = palette$fill[8], 
            "non_cms" = palette$fill[1],
            "rough_cor" = palette$fill[7], 
            "sd_D" = palette$fill[4],
            "sum_peaks" = palette$fill[2])

# Create the plot of the first chosen feature
bullet_feature_plot <- chosen_features %>%
  filter(set == "Set 1") %>%
  group_by(bin_situation, case) %>%
  mutate(nlevels = length(levels(factor(first)))) %>%
  ungroup() %>%
  mutate(bin_situation = fct_recode(bin_situation, "Equal Bins" = "equally spaced", 
                                    "Quantile Bins" = "quantile", 
                                    "Kernel Density" = "kernel density",
                                    "Match Tree" = "samesource tree",
                                    "RF Prob Tree" = "rfscore tree",
                                    "Normal Approximation" = "normal approximation")) %>%
  ggplot(aes(x = nbins, y = case, fill = first)) + 
  geom_tile() + 
  facet_grid(samesource ~ bin_situation, scales = "free", space = "free_y") + 
  theme_bw() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 20, family = "Avenir")) + 
  scale_fill_manual(values = colors, drop = FALSE) +
  labs(x = "Number of Bins", y = "Case", fill = "Feature")

# Create a caption for the lime figure
bullet_feature_caption <- ggdraw() + 
  draw_label(
    label =
"Clear vertical stripes for many of the methods 
suggesting LIME has produced global explanations 
that vary based on the method used",
    hjust = -0.01,
    x = 0,
    fontfamily = "Avenir", 
    size = 38)

# Join the text and images
bullet_feature_figure <- plot_grid(bullet_feature_title, bullet_feature_plot, bullet_feature_caption,
                               ncol = 1, 
                               rel_heights = c(0.075, 0.725, 0.2),
                               scale = 0.98)

# Print the figure
bullet_feature_figure 

# Save the plot
ggsave(filename = "./posters/2019-jsm-lime_diagnostics/figures/bullet_features.png",
       plot = bullet_feature_figure, 
       width = 13.5, 
       height = 10,  
       units = c("in"))

