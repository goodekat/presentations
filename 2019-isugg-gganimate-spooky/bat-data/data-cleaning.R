# DATA BAT CLEANING

# Pacakges
library(dplyr)
library(lubridate)

# Raw data
bats_raw <- read.csv("./2019-isugg-gganimate-spooky/bat-data/raw/Movement coordination in trawling bats (data from Giuggioli et al. 2015).csv")

# Select a few individuals to use in the data
bats_cleaned <- bats_raw %>%
  filter(tag.local.identifier %in% c(1:5)) %>%
  select(tag.local.identifier, timestamp, location.long, location.lat) %>%
  rename(id = tag.local.identifier, 
         date_time = timestamp, 
         longitude = location.long, 
         latitude = location.lat) %>% 
  mutate(time = format(ymd_hms(date_time), "%OS3")) %>%
  select(id, time, latitude, longitude) %>%
  mutate(time = as.numeric(time))

# Save the cleaned data
write.csv(x = bats_cleaned, 
          file = "./2019-isugg-gganimate-spooky/bat-data/bats-subset.csv", 
          row.names = FALSE)


