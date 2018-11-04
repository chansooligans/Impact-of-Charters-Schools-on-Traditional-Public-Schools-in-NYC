# Dependencies
library(dplyr)
library(tidyr)
library(openxlsx)

# File Locations
rm(list=ls())
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')
nyc_open06_slug = 'data/academic_2006-2012'
nyc_open13_slug = 'data/academic_2013-2018'
nyc_doe_slug = 'data/nyc_doe'
locations_slug = 'data/locations'

##############################
# Load Data
##############################

# NYC TEST SCORES FROM OPEN DATA
##############################

# NYC Open Data 2006-2013
files_nyc_open06 = list.files(nyc_open06_slug)
nyc_open06 = list()
nyc_open06 = lapply(paste(nyc_open06_slug,files_nyc_open06,sep='/'),read.csv)
names(nyc_open06) = files_nyc_open06

# NYC Open Data 2013-2017
files_nyc_open13 = list.files(nyc_open13_slug)
nyc_open13 = list()
nyc_open13 = lapply(paste(nyc_open13_slug,files_nyc_open13,sep='/'),read.csv)
names(nyc_open13) = files_nyc_open13

# NYC TEST SCORES FROM NYC DOE (2013-2018)
##############################

# NYC DOE Data: Charter Math 2013-2018
files_nyc_doe = list.files(nyc_doe_slug)
charter_math = read.xlsx(paste(nyc_doe_slug,files_nyc_doe[4],sep='/'), 
                        sheet = 1, 
                        startRow = 8,
                        colNames = TRUE)

# NYC DOE Data: Charter ELA 2013-2018
charter_ela = read.xlsx(paste(nyc_doe_slug,files_nyc_doe[4],sep='/'), 
                        sheet = 2, 
                        startRow = 8,
                        colNames = TRUE)

# NYC DOE Data: All Math 2013-2018
all_math_sheets = getSheetNames(paste(nyc_doe_slug,files_nyc_doe[10],sep='/'))
all_math = read.xlsx(paste(nyc_doe_slug,files_nyc_doe[10],sep='/'), 
                       sheet = 2, 
                       startRow = 8,
                       colNames = TRUE)
all_math = all_math[,-1] # Don't need first column, it's simply cocatenation of several other columns existing in dataset

# NYC DOE Data: All ELA 2013-2018
all_ela_sheets = getSheetNames(paste(nyc_doe_slug,files_nyc_doe[9],sep='/'))
all_ela = read.xlsx(paste(nyc_doe_slug,files_nyc_doe[9],sep='/'), 
                   sheet = 2,
                   startRow = 8,
                   colNames = TRUE)
all_ela = all_ela[,-1] # Don't need first column, it's simply cocatenation of several other columns existing in dataset

# Locations 2012-2018 (NYC OPEN DATA)
##############################

files_locations = list.files(locations_slug)
locations = list()
locations = lapply(paste(locations_slug,files_locations,sep='/'), read.csv, stringsAsFactors = F)

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

# Demographics 2013-2018 (NYC OPEN DATA)
##############################

demographics = read.csv('data/demographics/2013_-_2018_Demographic_Snapshot_School.csv', stringsAsFactors = F)

# Fix Column Names
colnames_to_fix = colnames(demographics)[19:38]
colnames_to_fix = substr(colnames_to_fix,4,nchar(colnames_to_fix))
colnames_to_fix = gsub(".1",'Percent',colnames_to_fix)
colnames(demographics)[19:38] = colnames_to_fix

# Fix Year Column
demographics$Year = as.integer(paste('20',substr(demographics$Year,6,7),sep=''))




##############################
##### Master: Aggregate NYC DOE Test Scores from NYC DOE Data
##############################

# Fix NYC DOE Column Names
nyc_doe_columns = c('DBN','School.Name','Grade','Year','Category','Number.Tested','Mean.Scale.Score',
                    paste(rep(c("Lvl1","Lvl2","Lvl3","Lvl4","Lvl3_4"),each=2),
                          rep(c('_cnt','_per'),5),
                          sep=''))

colnames(charter_math) = colnames(charter_ela) = colnames(all_math) = colnames(all_ela) = nyc_doe_columns

# Merge Sheets for "All Grades" in the year 2015 and Math
df = rbind(charter_math %>% 
             # filter(Grade == 'All Grades') %>% 
             mutate(charter = 1,
                    math = 1,
                    ela = 0),
           charter_ela %>% 
             # filter(Grade == 'All Grades') %>% 
             mutate(charter = 1,
                    math = 0,
                    ela = 1),
           all_math %>% 
             # filter(Grade == 'All Grades') %>% 
             mutate(charter = 0,
                    math = 1,
                    ela = 0),
           all_ela %>% 
             # filter(Grade == 'All Grades') %>% 
             mutate(charter = 0,
                    math = 0,
                    ela = 1))

##############################
##### Add Location
##############################

# Join Location with Master File
master = df %>%
  left_join(locations, by = c('DBN' = 'ATS.SYSTEM.CODE', 'Year' = 'FISCAL_YEAR'))

##############################
##### Diversity Index
##############################

# Join Demographics with Master File
# master = master %>%
#   left_join(demographics %>% select(-School.Name), by = c('DBN'='DBN', 'Year' = 'Year'))
# 
# colnames(demographics)

diversity = demographics %>%
  select(DBN, Year, Total.Enrollment, Asian, Black, Hispanic, Multiple.Race.Categories.Not.Represented, White)

for(i in 3:8){
  diversity[,i] = as.numeric(gsub(',','',diversity[,i]))
}

# percents
percents = diversity[,c(4:8)]/ diversity$Total.Enrollment

# shannon entropy 
shannon = -apply(percents * log(percents+.001),1,sum)
diversity = cbind(diversity,shannon)

# Merge with Master
master = master %>% 
  left_join(diversity, by = c('DBN', 'Year'))

# Export
write.csv(master,'data/master.csv', row.names = F)



