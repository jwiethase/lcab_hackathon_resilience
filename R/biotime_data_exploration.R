### data exploration of full data


library(tidyverse)

bt_f <- read_csv("Data/biotime_full.csv")
bt_f <- janitor::clean_names(bt_f)

bt_meta <- read_csv("Data/biotime_metadata.csv")
bt_meta <- janitor::clean_names(bt_meta)

summary(bt_f)

# every species for every plot - binary count for each year

bt_f_summary <- bt_f %>%
  group_by(plot, year, genus) %>%
  tally()

bt_f_summary <- bt_f_summary%>%
  group_by(plot, genus) %>%
  tally()

bt_f_s_50 <- bt_f_summary %>%
  filter(n >= 50)

bt_f_s_50 %>%
  group_by(plot) %>%
  tally()


