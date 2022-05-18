### data exploration of full data


library(tidyverse)

bt_f <- read_csv("Data/biotime_full.csv")
bt_f <- janitor::clean_names(bt_f)

summary(bt_f)

# every species for every plot - binary count for each year

