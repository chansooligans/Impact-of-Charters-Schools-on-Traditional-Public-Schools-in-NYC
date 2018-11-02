# Dependencies
library(plyr)
library(dplyr)
library(tidyr)

# File Locations
rm(list=ls())
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')
qgis_slug = 'data/qgis_exports'

##### Read Data
##############################
qgis_exports = list.files('data/qgis_exports/')
qgis_list = list()
qgis_list = lapply(paste(qgis_slug,qgis_exports,sep='/'), read.csv, stringsAsFactors = F)
qgis_list[[1]]


