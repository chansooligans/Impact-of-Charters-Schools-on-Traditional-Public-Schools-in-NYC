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
locations = lapply(paste(locations_slug,files_locations,sep='/'), read.csv, stringsAsFactors = F)

colnames(locations[[1]])
loc_cols_to_keep = c('FISCAL_YEAR',
                     'ATS.SYSTEM.CODE',
                     'COMMUNITY_DISTRICT',
                     'COUNCIL_DISTRICT',
                     'NTA',
                     'NTA_NAME',
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

# Inventory by NTA
colnames(master_loc)

nta = master_loc %>%
  filter(Grade == '4', math == 1) %>%
  group_by(Year, NTA) %>%
  dplyr::summarize(charter = sum(charter),
                   total = n())

nta.charter.inv = dcast(nta, NTA ~ Year, value.var = c('charter'))
nta.tps.inv = dcast(nta, NTA ~ Year, value.var = c('total'))

apply(nta.tps.inv[,-1],2,sum)


# Export
write.csv(inventory,'data/inventory_locations_master.csv', row.names = F)
