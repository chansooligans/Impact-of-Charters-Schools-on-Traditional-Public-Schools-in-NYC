# Dependencies
library(dplyr)
library(plyr)
library(tidyr)
library(openxlsx)
library(reshape2)

# File Locations
rm(list=ls())
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')

##### Read Data
##############################
master = read.csv('data/master.csv', stringsAsFactors = F)

##### Get Longitude / Latitude
##############################

# "Location.1" column contains Longitude and Latitude
master$LongLat = gsub("[\\(\\)]", "", regmatches(master$Location.1, gregexpr("\\(.*?\\)", master$Location.1)))

# Schools Missing Long Lat
missing_long_lat = which(nchar(master$LongLat)<17)

# Create Lat and Lon Columns
master = master %>%
  separate(LongLat, c("lat","lon"), ", ") %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

master = master %>% filter(abs(lon) > 0) #filter out schools missing long/lat


##### Elementary Schools (MATH) (Schools with Grade = 4), Year > 2013
##############################

elem_math = master %>% 
  filter(Grade == '4', 
         math == 1, 
         Year>2013)

years = unique(elem_math$Year)

for(i in 1:length(years)){
  export_file = elem_math %>% filter(Year == years[i])
  write.csv(export_file,paste('data/merged_scores_and_longlat/elem_math_allgrades_',years[i],'.csv',sep=''), row.names = F)
}


