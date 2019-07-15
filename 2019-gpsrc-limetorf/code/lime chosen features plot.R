# Create heatmap plot of first chosen feature by LIME to be included on poster
# Last updated: 2019/04/05

# Load libraries
library(gretchenalbrecht)
library(tidyverse)

# Read in the test_explain data
hamby224_test_explain <- readRDS("./2019-gpsrc-limetorf/data/hamby224_test_explain.rds")

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
lime_first_chosen <- chosen_features %>%
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
        text = element_text(size = 25, family = "Helvetica")) + 
  scale_fill_manual(values = colors, drop = FALSE) +
  labs(x = "Number of Bins", y = "Case", fill = "Feature", title = "First Feature Chosen by LIME")

# Look at the plot        
lime_first_chosen

# Save the plot
ggsave("./2019-gpsrc-limetorf/figures/lime_first_chosen.png", 
       plot = lime_first_chosen, 
       width = 15, 
       height = 9, 
       units = c("in"))
