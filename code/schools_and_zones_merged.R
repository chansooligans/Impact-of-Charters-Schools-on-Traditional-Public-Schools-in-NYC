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
cols_to_keep = c('DBN','zoned_dist','Year','charter','math','ela','CENSUS_TRACT','lat','lon','shannon')
qgis_df = qgis_df[qgis_df$math==1,cols_to_keep]

# Reshape data from long to wide (creates new columns for each year)
qgis_by_year = dcast(qgis_df, DBN + zoned_dist + CENSUS_TRACT ~ Year, value.var = 'charter') %>% arrange(DBN)

# "Pivot"
qgis_inventory = qgis_by_year %>%
  replace_na(list(`2014`=-1,`2015`=-1,`2016`=-1,`2017`=-1,`2018`=-1)) %>%
  group_by(zoned_dist) %>%
  dplyr::summarize(charter14 = sum(`2014`==1),
                   charter15 = sum(`2015`==1),
                   charter16 = sum(`2016`==1),
                   charter17 = sum(`2017`==1),
                   charter18 = sum(`2018`==1),
                   tot14 = sum(`2014`>=0),
                   tot15 = sum(`2015`>=0),
                   tot16 = sum(`2016`>=0),
                   tot17 = sum(`2017`>=0),
                   tot18 = sum(`2018`>=0))

# Check Totals
apply(qgis_inventory,2,sum)

qgis_inventory




