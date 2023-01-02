
# Load in the training data
# Version copied from lime-to-bullets project on April 6, 2019
hamby173and252_train <- read.csv("./2019-gpsrc-limetorf/data/hamby173and252_train.csv")

# Obtain features used when fitting the rtrees random forest
rf_features <- rownames(bulletr::rtrees$importance)

features <- hamby173and252_train %>% 
  select(rf_features, samesource) %>%
  gather(key = feature, value = value, 1:9) %>%
  select(feature, value, samesource) %>%
  ggplot(aes(x = value, fill = samesource)) + 
  geom_histogram(position = "fill", bins = 30, size = 0) + 
  facet_wrap( ~ feature, scales = "free", nrow = 2) +
  labs(x = "Variable Value", y = "Proportion", fill = "Match?",
       title = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),  
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        strip.text.x = element_text(size = 25),
        text = element_text(family = "Helvetica")) + 
  scale_fill_manual(values = c("TRUE" = "darkorange", 
                               "FALSE" = "darkgrey"))

features 

# Save the plot
ggsave("./2019-gpsrc-limetorf/figures/features.png", 
       plot = features, 
       width = 15, 
       height = 6, 
       units = c("in"))

