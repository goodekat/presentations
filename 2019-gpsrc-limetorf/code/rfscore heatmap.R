# Create heatmap plot of random forest scores to be included on poster
# Last updated: 2019/04/05

# Load libraries
library(tidyverse)

# Load in the testing data (Hamby Data 224 Sets 1 and 11)
# Version copied from lime-to-bullets project on April 5, 2019
hamby224_test <- read_csv("./2019-gpsrc-limetorf/data/hamby224_test.csv")

# Heatmap of rfscore for each comparison in set 1
heatmap <- hamby224_test %>%
  filter(set == "Set 1", bullet1 != "Questioned", bullet2 != "Questioned") %>%
  mutate(bullet1 = fct_recode(bullet1, "Bullet 1" = "Known 1", "Bullet 2" = "Known 2"),
         bullet2 = fct_recode(bullet2, "Bullet 1" = "Known 1", "Bullet 2" = "Known 2"),
         land1 = fct_recode(land1, "1" = "Land 1", "2" = "Land 2", "3" = "Land 3",
                            "4" = "Land 4", "5" = "Land 5", "6" = "Land 6"),
         land2 = fct_recode(land2, "1" = "Land 1", "2" = "Land 2", "3" = "Land 3",
                           "4" = "Land 4", "5" = "Land 5", "6" = "Land 6")) %>%
  ggplot(aes(x = land1, y = land2, label = bullet1, label2 = bullet2,
             text = paste('Bullets Compared: ', bullet1, "-", land1, 
                          "vs", bullet2, "-", land2,
                          '\nRandom Forest Score: ', 
                          ifelse(is.na(rfscore), "Missing due to tank rash", rfscore)))) +
  geom_tile(aes(fill = rfscore)) +
  facet_grid(bullet2 ~ bullet1, scales = "free") +
  theme_bw() +
  scale_fill_gradient2(low = "darkgrey", high = "darkorange", midpoint = 0.5, na.value = "white") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 25, family = "Helvetica")) +
  labs(x = "Land Number", y = "Land Number", fill = "Random Forest Probability of a Match") + 
  guides(fill = guide_colourbar(barwidth = 20, title.position = "top"))

# Look at the plot        
heatmap

# Save the plot
ggsave("./2019-gpsrc-limetorf/figures/heatmap.pdf", plot = heatmap, width = 7.5, height = 8, units = c("in"))

