# Dependencies
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)

# File Locations
rm(list=ls())
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')
qgis_slug = 'data/qgis_exports/'

##############################
##### Read Data
##############################

# Data Exported from 'compile_schools_data.R'
##############################
master_loc = read.csv('data/master_loc.csv', stringsAsFactors = F)

# Data Exported from QGIS (schools merged with zones)
##############################
qgis_exports = list.files(qgis_slug)
qgis_list = list()
qgis_list = lapply(paste(qgis_slug,qgis_exports,sep='/'), read.csv, stringsAsFactors = F)

# Then combine list of data frames into single dataframe
qgis_df = rbind.fill(qgis_list)
cols_to_keep = c('DBN','esid_no','Year','charter','math','ela','CENSUS_TRACT','lat','lon','shannon')

# Subset Dataframe
qgis_df = qgis_df[qgis_df$math==1,cols_to_keep]

##############################
##### Create and Export Inventory File 
##############################

# Inventory by NTA
##############################

nta = master_loc %>%
  filter(Grade == '4', math == 1) %>%
  group_by(Year, NTA) %>%
  dplyr::summarize(charter = sum(charter),
                   total = n())

nta.charter.inv = dcast(nta, NTA ~ Year, value.var = c('charter'))
nta.total.inv = dcast(nta, NTA ~ Year, value.var = c('total'))

# Inventory by Community District
##############################

com_dist = master_loc %>%
  filter(Grade == '4', math == 1) %>%
  group_by(Year, COMMUNITY_DISTRICT) %>%
  dplyr::summarize(charter = sum(charter),
                   total = n())

com_dist.charter.inv = dcast(com_dist, COMMUNITY_DISTRICT ~ Year, value.var = c('charter'))
com_dist.total.inv = dcast(com_dist, COMMUNITY_DISTRICT ~ Year, value.var = c('total'))

# Inventory by Community District
##############################

c_tract = master_loc %>%
  filter(Grade == '4', math == 1) %>%
  group_by(Year, CENSUS_TRACT) %>%
  dplyr::summarize(charter = sum(charter),
                   total = n())

c_tract.charter.inv = dcast(c_tract, CENSUS_TRACT ~ Year, value.var = c('charter'))
c_tract.total.inv = dcast(c_tract, CENSUS_TRACT ~ Year, value.var = c('total'))

# Inventory by QGIS: Elementary School Zone
##############################

qgis = qgis_df %>%
  group_by(Year, esid_no) %>%
  dplyr::summarize(charter = sum(charter),
                   total = n())

qgis.charter.inv = dcast(qgis, esid_no ~ Year, value.var = c('charter'))
qgis.total.inv = dcast(qgis, esid_no ~ Year, value.var = c('total'))




# Export
write.csv(nta.charter.inv,'data/data_inventory/nta.charter.inv.csv', row.names = F)
write.csv(nta.total.inv,'data/data_inventory/nta.total.inv.csv', row.names = F)
write.csv(com_dist.charter.inv,'data/data_inventory/com_dist.charter.inv.csv', row.names = F)
write.csv(com_dist.total.inv,'data/data_inventory/com_dist.total.inv.csv', row.names = F)
write.csv(c_tract.charter.inv,'data/data_inventory/c_tract.charter.inv.csv', row.names = F)
write.csv(c_tract.total.inv,'data/data_inventory/c_tract.total.inv.csv', row.names = F)
write.csv(qgis.charter.inv,'data/data_inventory/qgis.charter.inv.csv', row.names = F)
write.csv(qgis.total.inv,'data/data_inventory/qgis.total.inv.csv', row.names = F)
