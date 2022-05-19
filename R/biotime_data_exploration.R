### data exploration of full data
library(tidyverse)

bt_f <- read_csv("Data/biotime_full.csv") %>% 
  janitor::clean_names()

bt_meta <- read_csv("Data/biotime_metadata.csv") %>% 
  janitor::clean_names()

####### old stuff ########
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

bt_f$latitude <- round(bt_f$latitude, 2)
bt_f$longitude <- round(bt_f$longitude, 2)
bt_f$location <- paste(bt_f$latitude, bt_f$longitude, sep = '_')

bt_f_summary <- bt_f %>%
  group_by(location, year, genus) %>%
  tally()

bt_f_summary <- bt_f_summary %>%
  group_by(location, genus) %>%
  tally()

bt_f_s_50 <- bt_f_summary %>%
  filter(n >= 50)

bt_f_s_50 %>%
  group_by(plot) %>%
  tally()

#plot(bt_f$longitude ~ bt_f$latitude, pch = 16, cex = 0.2, col = rgb(0,0,1,0.2)) # map sightings, but takes a huge amount of memory and time

bt_ordered$gen_yr <- paste(bt_ordered$genus, bt_ordered$year, '_')
bt_ordered <- bt_ordered[order(bt_ordered$gen_yr),]
bt_o_s <- bt_ordered %>% select(c('year','genus','location'))

counts <- aggregate(rep(1, NROW(bt_o_s)), by = list(genus = bt_o_s$genus, location = bt_o_s$location), sum)
counts

continuous <- function(years){
  years <- years[!is.na(years)]
  start_year <- min(years)
  end_year <- max(years)
  range_year <- range(years)
  max_gap <- max(years[-1] - years[-nrow(years)])
  
  sightings
}

  
  

year_diff <- aggregate(bt_o_s$year, by = list(genus = bt_o_s$genus, location = bt_o_s$location), range)
year_diff



bt_o_s$year_count <- NA
bt_o_s$year_gap <- NA

for(i in 2:10){
  genus <- bt_o_s[bt_o_s$genus == bt_o_s$genus[i] & bt_o_s$location == bt_o_s$location[i],]
  genus <- genus[!is.na(genus$genus),]
  bt_o_s$year_count[i] <- nrow(genus)
  
  if(bt_o_s$genus[i] == bt_o_s$genus[i-1] & bt_o_s$location[i] == bt_o_s$location[i-1]){
    bt_o_s$year_gap[i] <- bt_o_s$year[i] - bt_o_s$year[i-1]
  }
  else(bt_o_s$year_gap[i] <- 9999)  
}

####### start over #########
terr_animals <- bt_meta[bt_meta$realm == 'Terrestrial' & 
                          bt_meta$taxa != 'Fungi' & 
                          bt_meta$taxa != 'Terrestrial plants',]

bt_ta <- bt_f[bt_f$study_id %in% terr_animals$study_id,]
rm(bt_f, bt_meta)

bt_ta$loc <- paste0(round(bt_ta$latitude,2), '_', round(bt_ta$longitude,2))
bt_ta_s <- bt_ta[,c('study_id','year','genus','loc')]

bt_ta_summary <- bt_ta_s %>%
  group_by(loc, year, genus) %>%
  tally()

bt_ta_summary <- bt_ta_summary%>%
  group_by(loc, genus) %>%
  tally()

hist(bt_ta_summary$n)
gen_loc <- bt_ta_summary %>%
  group_by(loc) %>%
  tally()


bt_ta_s_10 <- bt_ta_summary %>%
  filter(n >= 10)

gen_loc <- bt_ta_s_10 %>%
  group_by(loc) %>%
  tally()

bt_ta_s_20 <- bt_ta_summary %>%
  filter(n >= 20)

gen_loc <- bt_ta_s_20 %>%
  group_by(loc) %>%
  tally()

gen_loc_sep <- separate(gen_loc, loc, into = c('lat','long'), sep = '_', remove = F)

plot(gen_loc_sep$long ~ gen_loc_sep$lat, las = 1, pch = 16, col = rgb(0,0,1,0.2),
     xlab = 'latitude', ylab = 'longitude')
abline(lty = 2, v = 0, h = 0)
text('equator', x = 20, y = 5)
text('Greenwich', x = -3, y = 50, srt = 90)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = 'medium', returnclass = 'sf')
class(world)

#ggplot()+
#  geom_sf(data = world)+
#  coord_sf(ylim = c(-40, 80), xlim = c(-150,150))+
#  geom_point(data = gen_loc_sep, aes(y = lat, x = long))

maps::map('world')
points(gen_loc_sep$long, gen_loc_sep$lat, cex = 0.5, pch = 16, col = 'blue')

####### everything everything ########
bt_f
bt_meta

bt_f$loc <- paste0(round(bt_f$latitude,2), '_', round(bt_f$longitude,2))

bt_f <- bt_f %>%
  filter(!(year<1999))

terr_animals <- bt_meta[bt_meta$realm == 'Terrestrial' & 
                          bt_meta$taxa != 'Fungi' & 
                          bt_meta$taxa != 'Terrestrial plants',]

bt_f_s <- bt_f %>%
  group_by(study_id,year,loc) %>%
  tally()

bt_f_s <- bt_f_s[ with(bt_f_s, order(loc,year)),]

bt_f_s$id <- seq(1:length(bt_f_s$loc))

long_list <- aggregate(bt_f_s$year,
                       by= list(loc=bt_f_s$loc),
                       FUN=function(x){
                         x<- sort(x)
                         timediff <- x[NROW(x)] - x[1]
                         return(c(time = timediff, 
                                     end_year = x[NROW(x)], 
                                     start_year = x[1]))
                       })

str(long_list)

long_df <- data.frame(loc = long_list$loc,
                      time = long_list$x[,'time'],
                      end_year = long_list$x[,'end_year'],
                      start_year = long_list$x[,'start_year'])

long_df <- long_df %>%
  filter(time >= 3)

long_df <- separate(long_df, loc, into = c('lat','long'), sep = '_', remove = F)

write_csv(long_df, file="location_long_list.csv")


## [1] TRUE

bt_f_summary <- bt_f_s %>%
  group_by(study_id, loc, year, genus) %>%
  tally()

bt_f_summary <- bt_f_summary%>%
  group_by(study_id, loc, genus) %>%
  tally()

bt_f_summary <- bt_f_summary %>%
  filter(n >= 3)

bt_f <- bt_f %>%
  filter(study_id %in% bt_f_summary$study_id) %>%
  filter(study_id %in% terr_animals$study_id)




gen_loc <- bt_f %>%
  group_by(loc) %>%
  tally()
gen_loc_sep <- separate(gen_loc, loc, into = c('lat','long'), sep = '_', remove = F)

plot(gen_loc_sep$long ~ gen_loc_sep$lat, las = 1, pch = 16, col = rgb(0,0,1,0.2),
     xlab = 'latitude', ylab = 'longitude')
abline(lty = 2, v = 0, h = 0)
text('equator', x = 20, y = 5)
text('Greenwich', x = -3, y = 50, srt = 90)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = 'medium', returnclass = 'sf')
class(world)

maps::map('world')
points(gen_loc_sep$long, gen_loc_sep$lat, cex = 0.5, pch = 16, col = 'blue')







####### birds only #####
bt_f <- read_csv("Data/biotime_full.csv") %>% janitor::clean_names()
bt_meta <- read_csv("Data/biotime_metadata.csv") %>% janitor::clean_names()

terr_birds <- bt_meta[bt_meta$realm == 'Terrestrial' & bt_meta$taxa == 'Birds',]

birds <- bt_f[bt_f$study_id %in% terr_birds$study_id,]
rm(bt_f, bt_meta)

birds$loc <- paste0(round(birds$latitude,2), '_', round(birds$longitude,2))

bt_ta_summary <- birds %>%
  group_by(loc, year, genus) %>%
  tally()

bt_ta_summary <- bt_ta_summary%>%
  group_by(loc, genus) %>%
  tally()

bt_ta_s_10 <- bt_ta_summary %>% filter(n >= 3)
gen_loc_10 <- bt_ta_s_10 %>%
  group_by(loc) %>%
  tally()
gen_loc10_sep <- separate(gen_loc_10, loc, into = c('lat','long'), sep = '_', remove = F)

bt_ta_s_20 <- bt_ta_summary %>% filter(n >= 20)
gen_loc_20 <- bt_ta_s_20 %>%
  group_by(loc) %>%
  tally()
gen_loc20_sep <- separate(gen_loc_20, loc, into = c('lat','long'), sep = '_', remove = F)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = 'medium', returnclass = 'sf')
class(world)

maps::map('world')
points(gen_loc10_sep$long, gen_loc10_sep$lat, cex = 0.5, pch = 16, col = 'blue')
points(gen_loc20_sep$long, gen_loc20_sep$lat, cex = 0.5, pch = 16, col = 'red')

######
bt_ta_s_10 <- separate(bt_ta_s_10, loc, into = c('lat','lon'), sep = '_', remove = F)

length(unique(bt_ta_s_10$genus))
length(unique(bt_ta_s_10$loc))

birds$loc <- paste0(round(birds$latitude,2),'_',round(birds$longitude,2))
sites <- birds[birds$loc %in% bt_ta_s_10$loc,]

birds_gen <- birds %>%
  group_by(genus, loc, year) %>%
  tally()

birds_gen <- birds_gen %>%
  group_by(loc, genus) %>%
  mutate(mean_per_year = mean(n)) %>%
  tally()

birds_gen_2 <- birds_gen %>%
  group_by(loc) %>%
  tally()

example_site <- "39.08_-96.58"

birds_sample <- birds %>%
  filter(loc == example_site)

ggplot(birds_sample, aes(x=year, y=sum_allrawdata_abundance, fill=genus)) +
  geom_col()+
  theme(legend.position=("none"))

length(unique(birds$study_id))





