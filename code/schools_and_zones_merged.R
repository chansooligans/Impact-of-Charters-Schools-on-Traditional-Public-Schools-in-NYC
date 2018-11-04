# Dependencies
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

# File Locations
rm(list=ls())
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')
qgis_slug = 'data/qgis_exports'

##### Read Data Exported from QGIS (schools merged with zones)
##############################
qgis_exports = list.files('data/qgis_exports/')
qgis_list = list()
qgis_list = lapply(paste(qgis_slug,qgis_exports,sep='/'), read.csv, stringsAsFactors = F)

# Then combine list of data frames into single dataframe
qgis_df = rbind.fill(qgis_list)
cols_to_keep = c('DBN','esid_no','Year','charter','math','ela','CENSUS_TRACT','lat','lon','shannon')

# Subset Dataframe
qgis_df = qgis_df[qgis_df$math==1,cols_to_keep]

qgis = qgis_df %>%
  group_by(Year, esid_no) %>%
  dplyr::summarize(charter = sum(charter),
                   total = n())


qgis.charter.inv = dcast(qgis, esid_no ~ Year, value.var = c('charter'))
qgis.tps.inv = dcast(qgis, esid_no ~ Year, value.var = c('total'))


