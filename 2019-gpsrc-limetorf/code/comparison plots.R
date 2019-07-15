
library(gretchenalbrecht)
library(tidyverse)

# Read in the lime comparison data
# Version copied from lime-to-bullets project on April 7, 2019
hamby224_lime_comparisons <- readRDS("2019-gpsrc-limetorf/data/hamby224_lime_comparisons.rds")

# Summarizing lime results
hamby224_lime_results <- hamby224_lime_comparisons %>%
  filter(!is.na(rfscore)) %>%
  group_by(situation, bin_situation, bin_continuous, quantile_bins, nbins, use_density, 
           bin_method, response, set) %>%
  summarise(mse = (sum(diff^2)) / length(diff),
            ave_r2 = mean(model_r2)) %>%
  arrange(set) %>%
  ungroup()


metrics <- hamby224_lime_results %>%
  filter(set == "Set 1") %>%
  mutate(bins = ifelse(nbins == 4 & bin_continuous == FALSE, "other", nbins)) %>%
  select(bin_situation, bins, mse, ave_r2) %>%
  rename("Average R2" = "ave_r2", "MSE" = "mse") %>%
  mutate(bins = fct_recode(bins, "2 Bins" = "2", "3 Bins" = "3", "4 Bins" = "4",
                           "5 Bins" = "5", "6 Bins" = "6", "Other" = "other"),
         bin_situation = fct_recode(bin_situation, "Equal Bins" = "equally spaced", 
                                    "Quantile Bins" = "quantile", 
                                    "Kernel Density" = "kernel density",
                                    "Match Tree" = "samesource tree",
                                    "RF Prob Tree" = "rfscore tree",
                                    "Normal Approximation" = "normal approximation")) %>%
  gather(metric, value, 3:4) %>%
  mutate(metric = factor(metric, levels = c("MSE", "Average R2"))) %>%
  ggplot(aes(x = bin_situation, y = value, color = bin_situation)) +
  geom_point(size = 3) + 
  facet_grid(metric ~ bins, scales = "free") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none",
        text = element_text(size = 25, family = "Helvetica")) + 
  labs(x = "", y = "", color = "") + 
  scale_color_gretchenalbrecht(palette = "last_rays", discrete = TRUE)

metrics

# Save the plot
ggsave("./2019-gpsrc-limetorf/figures/metrics.pdf", 
       plot = metrics, 
       width = 15, 
       height = 7, 
       units = c("in"))

