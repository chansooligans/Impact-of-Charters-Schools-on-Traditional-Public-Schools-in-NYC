# Dependencies
library(dplyr)
library(tidyr)
library(openxlsx)

# File Locations
rm(list=ls())
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')
locations_slug = 'data/locations'

##### Read Data
##############################
master = read.csv('data/all_schools_master.csv')

# Locations 2012-2018
files_locations = list.files(locations_slug)
locations = list()
for(i in 1:length(files_locations)){
  locations[[i]] = read.csv(paste(locations_slug,files_locations[i],sep='/'), stringsAsFactors = F)  
}
names(locations) = files_locations

##### Check Schools Against Locations
##############################

# School Location Inventory
get_location_inventory = function(dbn, locations){
  schools = unique(dbn)
  
  # Load DBN's in Location Files. Location_2012 is for year starting 2012
  locations_2012 = unique(trimws(locations[[1]]$ATS.SYSTEM.CODE))
  locations_2013 = unique(trimws(locations[[2]]$ATS.SYSTEM.CODE))
  locations_2014 = unique(trimws(locations[[3]]$ATS.SYSTEM.CODE))
  locations_2015 = unique(trimws(locations[[4]]$ATS.SYSTEM.CODE))
  locations_2016 = unique(trimws(locations[[5]]$ATS.SYSTEM.CODE))
  locations_2017 = unique(trimws(locations[[6]]$ATS.SYSTEM.CODE))
  
  # Join Indicator Columns of Whether Location Data is available for schools by year
  inv_loc = as.data.frame(cbind(schools %in% locations_2012,
                                schools %in% locations_2013,
                                schools %in% locations_2014,
                                schools %in% locations_2015,
                                schools %in% locations_2016,
                                schools %in% locations_2017))
  
  # Clean
  inv_loc$all = apply(inv_loc,1,sum)
  inv_loc = cbind(as.character(schools),inv_loc)
  colnames(inv_loc) = c('DBN','year2012','year2013','year2014','year2015','year2016','year2017','all')
  return(inv_loc)
}

loc_inventory_all = get_location_inventory(master$DBN, locations)
loc_inventory_all$DBN

master_location_inventory = loc_inventory_all %>% 
  left_join(master %>% select(DBN,math,ela,charter,Grade,Year), by = 'DBN') %>%
  arrange(DBN)

# Export
write.csv(master_location_inventory,'data/inventory_locations_master.csv', row.names = F)












