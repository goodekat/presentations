# Create example signature plots to include on poster for gpsrc
# Last updated: 2019/04/04

# Load libraries
library(tidyverse)

# Set a random seed that produces the desired type of plot
set.seed(20190402)

# Set the number of observations and number of "signatures"
n = 500
ns = 2

# Simulate the data
wiggles <- tibble(t = rep(1:n, ns),
                  group = factor(rep(c("Signature 1", "Signature 2"), each = n)),
                  yt = arima.sim(n = n*ns, 
                                list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
                                sd = sqrt(0.1796)))

# Create the plot of the signatures
sig_plot <- ggplot(wiggles, aes(x = t, y = yt)) + 
  geom_smooth(span = 0.1, color = "black", se = FALSE) + 
  facet_grid(group ~ ., switch = "y") +
  theme_bw() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.y = element_blank())

# Look at the plot        
sig_plot

# Save the plot
ggsave("./2019-gpsrc-limetorf/figures/sig_plot.pdf", plot = sig_plot, width = 14, height = 5, units = c("in"))
