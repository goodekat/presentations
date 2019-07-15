
library(cowplot)
hamby173and252_train <- read.csv("./data/hamby173and252_train.csv")

full <- ggplot(hamby173and252_train, aes(x = non_cms, y = rfscore)) + 
  geom_point() + 
  # geom_point(data = data.frame(non_cms = 0.5, rfscore = 0.9), 
  #            aes(x = non_cms, y = rfscore), color = "darkorange", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") + 
  labs(y = "Random Forest Prbability of a Match", 
       title = "Global Relationship") +
  theme_bw() + 
  theme(text = element_text(size = 25, family = "Helvetica"))

sub_linear <- hamby173and252_train %>%
  filter(non_cms <= 3) %>%
  ggplot(aes(x = non_cms, y = rfscore)) + 
  geom_point() + 
  # geom_point(data = data.frame(non_cms = 0.5, rfscore = 0.9), 
  #            aes(x = non_cms, y = rfscore), color = "darkorange", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") + 
  labs(y = "",
       title = "Local Relationship") + 
  theme_bw() + 
  theme(text = element_text(size = 25, family = "Helvetica"))

sub_bins <- hamby173and252_train %>%
  filter(non_cms <= 3) %>%
  mutate(binary_non_cms = ifelse(non_cms < 1, "0 <= non_cms < 1", " 1 <= non_cms < 3") %>%
           factor(levels = c("0 <= non_cms < 1", " 1 <= non_cms < 3"))) %>%
  ggplot(aes(x = binary_non_cms, y = rfscore)) + 
  geom_point() + 
  # geom_point(data = data.frame(non_cms = "0 <= non_cms < 1", rfscore = 0.9), 
  #            aes(x = non_cms, y = rfscore), color = "darkorange", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") + 
  labs(x = "Binned non_cms", 
       y = "",
       title = "Local Binned Relationship") + 
  theme_bw() + 
  theme(text = element_text(size = 25, family = "Helvetica"))

lime_example <- plot_grid(full, sub_linear, sub_bins, nrow = 1)

# Save the plot
ggsave("./figures/lime_example.png", 
       plot = lime_example, 
       width = 25, 
       height = 6, 
       units = c("in"))
