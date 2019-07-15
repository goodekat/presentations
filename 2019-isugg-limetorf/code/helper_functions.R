## Helper Functions for the Paper

## ---------------------------------------------------------------
## Create Bins using a Tree
## ---------------------------------------------------------------

# Function to get from a tree to k bins:
# Inputs: x = feature, y = response variable, k = number of bins
# Outputs: k-1 values for breakpoints (does not include minimum and maximum)

library(tidyverse)
library(tree)

y = hamby173and252_train$samesource
x = hamby173and252_train$ccf
k = 5

treebink <- function (y, x, k, minsize = 10) {
  
  # Set the mindev: "The within-node deviance must be at least this times 
  # that of the root node for the node to be split." 0.01 is the default
  # value in the tree package.
  dev <- 0.01
  
  # Fit a tree with starting specifications
  tree <- tree::tree(y ~ x, control = tree::tree.control(nobs = length(y), 
                                                         mindev = dev, 
                                                         minsize = minsize))
  while ((length(grep("<leaf>", tree$frame$var)) < k) & (dev > 1e-9)) {
    dev <- dev/10
    tree <- tree::tree(y ~ x, control = tree::tree.control(nobs = length(y), mindev=dev,  minsize = minsize))
  }
  
  tree <- tree::prune.tree(tree, best = k)
  breaks <- na.omit(unique(parse_number(tree$frame$splits[,1])))
  if (length(breaks) == k-1) return(sort(breaks)) # and we're happy because we got all the values we need
  
  
  if (length(breaks) > k-1) {
    # we have too many breaks, now reduce the number
    tree$frame$id <- as.numeric(row.names(tree$frame))
    # tree$frame %>%  ggplot(aes(x = id, y = dev, colour=var)) + geom_point()
    
    tree$frame$splits <- tree$frame$splits[,1]
    subtree <- tree$frame %>% arrange(id) %>% filter(var=="x")
    breaks <- sort(parse_number(subtree$splits[1:(k-1)]))
  }
  if (length(breaks) < k-1) {
    warning("Not enough intervals found, consider decreasing minsize (default is 10)")
  }
  breaks
  
}

# Examples:
# treebink(car.test.frame$Mileage, car.test.frame$Weight, k = 5)
# 
# # gives warning
# treebink(car.test.frame$Mileage, car.test.frame$Weight, k = 10)
# treebink(car.test.frame$Mileage, car.test.frame$Weight, k = 10, minsplit = 5)
# 
# # gives warning
# treebink(car.test.frame$Mileage, car.test.frame$Weight, k = 5, minsplit = 40)
