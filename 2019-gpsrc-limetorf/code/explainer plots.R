
# Load libraries
library(cowplot)
library(tidyverse)

# Read in the test_explain data
# Version copied from lime-to-bullets project on April 6, 2019
hamby224_test_explain <- readRDS("./2019-gpsrc-limetorf/data/hamby224_test_explain.rds")

# Subset to the case of interest
caseofinterest <- hamby224_test_explain %>%
  filter(set == "Set 1",
         bullet1 == "Known 1", bullet2 == "Known 2", 
         land1 == "Land 2", land2 == "Land 2") 

xlimit <- caseofinterest %>%
  filter(situation %in% c("3 equally spaced", "4 quantile", "kernel density")) %>%
  summarise(max(abs(feature_weight))) %>%
  as.double()

equal <- caseofinterest %>%
  filter(situation == "3 equally spaced") %>%
  mutate(feature_desc = reorder(feature_desc, abs(as.numeric(feature_weight))),
         evidence = factor(if_else(feature_weight >= 0, 
                                   "Supports a Match", "Supports a Non-Match"), 
                           levels = c("Supports a Non-Match", "Supports a Match"))) %>%
  ggplot() + 
  geom_col(aes(y = abs(feature_weight), x = feature_desc, fill = evidence)) + 
  ylim(0, xlimit) +
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 20, family = "Helvetica")) +
  labs(y = "Feature Effect Size", x = "", fill = "", 
       title = "Three Equal Bins") +
  scale_fill_manual(values = c("Supports a Match" = "darkorange", 
                               "Supports a Non-Match" = "darkgrey"),
                    drop = FALSE)
 
quantile <- caseofinterest %>%
  filter(situation == "4 quantile") %>%
  mutate(feature_desc = reorder(feature_desc, abs(as.numeric(feature_weight))),
         evidence = factor(if_else(feature_weight >= 0, 
                                   "Supports a Match", "Supports a Non-Match"), 
                           levels = c("Supports a Non-Match", "Supports a Match"))) %>%
  ggplot() + 
  geom_col(aes(y = abs(feature_weight), x = feature_desc, fill = evidence)) + 
  ylim(0, xlimit) +
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none", 
        text = element_text(size = 20, family = "Helvetica")) +
  labs(y = "Feature Effect Size", x = "", fill = "",
       title = "Four Quantile Bins") +
  scale_fill_manual(values = c("Supports a Match" = "darkorange", 
                               "Supports a Non-Match" = "darkgrey"),
                    drop = FALSE)

kernel <- caseofinterest %>%
  filter(situation == "kernel density") %>%
  mutate(feature_desc = reorder(feature_desc, abs(as.numeric(feature_weight))),
         evidence = factor(if_else(feature_weight >= 0, 
                                   "Supports a Match", "Supports a Non-Match"), 
                           levels = c("Supports a Non-Match", "Supports a Match"))) %>%
  mutate(feature_desc = fct_recode(feature_desc, "                 rough_cor" = "rough_cor")) %>%
  ggplot() + 
  geom_col(aes(y = abs(feature_weight), x = feature_desc, fill = evidence)) + 
  ylim(0, xlimit) +
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 20, family = "Helvetica")) +
  labs(y = "Feature Effect Size", x = "", fill = "",
       title = "Kernel Density") +
  scale_fill_manual(values = c("Supports a Match" = "darkorange", 
                               "Supports a Non-Match" = "darkgrey"),
                    drop = FALSE)

normal <- caseofinterest %>%
  filter(situation == "normal approximation") %>%
  mutate(feature_desc = reorder(feature_desc, abs(as.numeric(feature_weight))),
         evidence = factor(if_else(feature_weight >= 0, 
                                   "Supports a Match", "Supports a Non-Match"), 
                           levels = c("Supports a Non-Match", "Supports a Match"))) %>%
  mutate(feature_desc = fct_recode(feature_desc, "                 rough_cor" = "rough_cor")) %>%
  ggplot() + 
  geom_col(aes(y = abs(feature_weight), x = feature_desc, fill = evidence)) + 
  ylim(0, xlimit) +
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 20, family = "Helvetica")) +
  labs(y = "Feature Effect Size", x = "", fill = "",
       title = "Normal Approximation") +
  scale_fill_manual(values = c("Supports a Match" = "darkorange", 
                               "Supports a Non-Match" = "darkgrey"),
                    drop = FALSE)

legend <- get_legend(equal + 
                       theme(legend.position = "bottom",
                             text = element_text(size = 25, family = "Helvetica")))

plots <- plot_grid(quantile, equal, kernel, normal,
                   ncol = 2)

explainers <- plot_grid(plots, legend, 
          ncol = 1, 
          rel_heights = c(0.6, 0.1))

explainers

# Save the plot
ggsave("./2019-gpsrc-limetorf/figures/explainers.pdf", 
       plot = explainers, 
       width = 12, 
       height = 8, 
       units = c("in"))

