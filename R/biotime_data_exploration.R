### data exploration of full data


library(tidyverse)

bt_f <- read_csv("Data/biotime_full.csv")
bt_f <- janitor::clean_names(bt_f)

summary(bt_f)

# every species for every plot - binary count for each year

bt_s <- bt_f %>% select(c('year','plot','genus')) %>% distinct()


length(unique(bt_s$genus))

# create data frame with sightings per genus per year per plot -- long format creates over 300 billion rows
# presence <- data.frame(plot = rep(sort(unique(bt_s$plot)), length(unique(bt_s$genus))*length(unique(bt_s$year)) ),
#                       genus = rep(rep(sort(unique(bt_s$genus)),  length(unique(bt_s$plot))), length(unique(bt_s$year))),
#                       year = rep(sort(unique(bt_s$year))),
#                       presence = rep(NA, length(unique(bt_s$year))*length(unique(bt_s$plot))*length(unique(bt_s$genus)) ))









