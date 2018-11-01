# Dependencies
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)

# File Locations
rm(list=ls())
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')
locations_slug = 'data/locations'

##### Read Data
##############################
master = read.csv('data/all_schools_master.csv', stringsAsFactors = F)

# Locations 2012-2018
files_locations = list.files(locations_slug)

locations = list()

for(i in 1:length(files_locations)){
  locations[[i]] = read.csv(paste(locations_slug,files_locations[i],sep='/'), stringsAsFactors = F)  
}

loc_cols_to_keep = c('FISCAL_YEAR',
                     'ATS.SYSTEM.CODE',
                     'CENSUS_TRACT',
                     'Location.1')

# Subset each data frame in list to the columns listed above
locations = lapply(locations, function(x) x[,loc_cols_to_keep]) 
# Then combine list of data frames into single dataframe
locations = rbind.fill(locations)
# Fix DBN Column
locations$ATS.SYSTEM.CODE = trimws(locations$ATS.SYSTEM.CODE)


##### Join with Master
##############################

# Join with Master File
master_loc = locations %>%
  left_join(master, by = c('ATS.SYSTEM.CODE' = 'DBN', 'FISCAL_YEAR' = 'Year')) %>%
  dplyr::rename(DBN = ATS.SYSTEM.CODE, Year = FISCAL_YEAR) %>%
  arrange(DBN)

##### Create and Export Inventory File 
##############################

# Create Inventory File
inventory = master_loc %>%
  dplyr::group_by(Year, DBN) %>%
  dplyr::summarize(geo = n_distinct(Location.1)) %>%
  left_join(master %>% select(DBN,math,ela,charter,Grade,Year), by = c('Year','DBN')) %>%
  arrange(DBN)

# Export
write.csv(inventory,'data/inventory_locations_master.csv', row.names = F)

##### Get Longitude / Latitude
##############################

# "Location.1" column contains Longitude and Latitude
master_loc$LongLat = gsub("[\\(\\)]", "", regmatches(master_loc$Location.1, gregexpr("\\(.*?\\)", master_loc$Location.1)))

# Schools Missing Long Lat
missing_long_lat = which(nchar(master_loc$LongLat)<17)
master_loc[missing_long_lat,]

# Create Lat and Lon Columns
master_2 = master_loc %>%
  separate(LongLat, c("lat","lon"), ", ") %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

##### Export by Year
##############################

years = unique(master_2$Year)

export_file = master_2 %>% filter(Year == years[1])
write.csv(export_file,paste('data/master_',years[i],sep=''), row.names = F)







