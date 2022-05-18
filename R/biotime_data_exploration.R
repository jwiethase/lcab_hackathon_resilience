### data exploration of full data


library(tidyverse)

bt_f <- read_csv("Data/biotime_full.csv")
bt_f <- janitor::clean_names(bt_f)

bt_meta <- read_csv("Data/biotime_metadata.csv")
bt_meta <- janitor::clean_names(bt_meta)

summary(bt_f)

# every species for every plot - binary count for each year
bt_s <- bt_f %>% select(c('year','plot','genus')) %>% distinct()


length(unique(bt_s$genus))

# create data frame with sightings per genus per year per plot -- long format creates over 300 billion rows
# presence <- data.frame(plot = rep(sort(unique(bt_s$plot)), length(unique(bt_s$genus))*length(unique(bt_s$year)) ),
#                       genus = rep(rep(sort(unique(bt_s$genus)),  length(unique(bt_s$plot))), length(unique(bt_s$year))),
#                       year = rep(sort(unique(bt_s$year))),
#                       presence = rep(NA, length(unique(bt_s$year))*length(unique(bt_s$plot))*length(unique(bt_s$genus)) ))


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



