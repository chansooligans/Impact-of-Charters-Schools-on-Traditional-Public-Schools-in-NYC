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
master_loc = master %>%
  left_join(locations, by = c('DBN' = 'ATS.SYSTEM.CODE', 'Year' = 'FISCAL_YEAR')) %>%
  arrange(DBN)

# Export
write.csv(master_loc,'data/master_loc.csv', row.names = F)

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



head(inventory)
inventory %>%
  group_by(Year,charter) %>%
  summarize(count = n_distinct(DBN))
