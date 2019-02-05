# Dependencies
library(dplyr)
library(plyr)
library(tidyr)
library(openxlsx)
library(reshape2)
library(data.table)

# File Locations
rm(list=ls())
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')
nyc_open06_slug = 'data/academic_2006-2012'
nyc_open13_slug = 'data/academic_2013-2018'
nyc_doe_slug = 'data/nyc_doe'
locations_slug = 'data/locations'

########################################################################################################################
                                                    # Load Data                                                      
########################################################################################################################


# NYC TEST SCORES FROM NY OPEN DATA
# https://opendata.cityofnewyork.us/
##############################

# NYC Open Data 2006-2013
files_nyc_open06 = list.files(nyc_open06_slug)
nyc_open06 = list()
nyc_open06 = lapply(paste(nyc_open06_slug,files_nyc_open06,sep='/'),read.csv, stringsAsFactors = F)
names(nyc_open06) = files_nyc_open06

# NYC Open Data 2013-2017
files_nyc_open13 = list.files(nyc_open13_slug)
nyc_open13 = list()
nyc_open13 = lapply(paste(nyc_open13_slug,files_nyc_open13,sep='/'),read.csv, stringsAsFactors = F)
names(nyc_open13) = files_nyc_open13

# NYC TEST SCORES FROM NYC DOE WEBSITE (2013-2018)
# https://infohub.nyced.org/reports-and-policies/citywide-information-and-data/test-results
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
                     'GEOGRAPHICAL_DISTRICT_CODE',
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


# Demographics 2006-2012 (NYC OPEN DATA)
##############################

demographics_1 = read.csv('data/demographics/2006_-_2012_School_Demographics_and_Accountability_Snapshot.csv', stringsAsFactors = F)

# Fix Column Names
colnames(demographics_1)[c(1:6,21:38)] = c('DBN',
                                           'School.Name',
                                           'Year',
                                           'PovertyPercent',
                                           'frl_percent',
                                           'Total.Enrollment',
                                           'English.Language.Learners',
                                           'English.Language.LearnersPercent',
                                           'Students.with.Disabilities',
                                           'Students.with.DisabilitiesPercent',
                                           'ctt',
                                           'selfcontained',
                                           'Asian',
                                           'AsianPercent',
                                           'Black',
                                           'BlackPercent',
                                           'Hispanic',
                                           'HispanicPercent',
                                           'White',
                                           'WhitePercent',
                                           'Male',
                                           'MalePercent',
                                           'Female',
                                           'FemalePercent')


# Fix Year Column
demographics_1$Year = as.integer(substr(demographics_1$Year,5,8))

# Demographics 2013-2018 (NYC OPEN DATA)
##############################

demographics_2 = read.csv('data/demographics/2013_-_2018_Demographic_Snapshot_School.csv', stringsAsFactors = F)

# Fix Column Names
colnames_to_fix = colnames(demographics_2)[19:38]
colnames_to_fix = substr(colnames_to_fix,4,nchar(colnames_to_fix))
colnames_to_fix = gsub(".1",'Percent',colnames_to_fix)
colnames(demographics_2)[19:38] = colnames_to_fix

# Fix Year Column (If 2013-14, keep 2014)
demographics_2$Year = as.integer(paste('20',substr(demographics_2$Year,6,7),sep=''))


# Remove "Multiple Race Categories Not Represented" for consistency across Demographics Data files
demographics_2$Total.Enrollment = as.integer(gsub(',','',demographics_2$Total.Enrollment))
demographics_2$Total.Enrollment = demographics_2$Total.Enrollment - demographics_2$Multiple.Race.Categories.Not.Represented

# Merge Demographics 
##############################
cols_to_keep = c('DBN',
                 'School.Name',
                 'Year',
                 'PovertyPercent',
                 'Total.Enrollment',
                 'English.Language.Learners',
                 'Students.with.Disabilities',
                 'Asian',
                 'Black',
                 'Hispanic',
                 'White',
                 'Male',
                 'Female')

# Fix formatting from char to int
for(i in cols_to_keep[!cols_to_keep %in% c('DBN','School.Name','PovertyPercent')]){demographics_1[,i] = as.numeric(gsub(",", "", demographics_1[,i]))}
for(i in cols_to_keep[!cols_to_keep %in% c('DBN','School.Name','PovertyPercent')]){demographics_2[,i] = as.numeric(gsub(",", "", demographics_2[,i]))}
demographics_2$PovertyPercent = gsub("%","",demographics_2$PovertyPercent)

# Bind
demographics = bind_rows(demographics_1[,cols_to_keep],
                         demographics_2[,cols_to_keep])
demographics$PovertyPercent = as.numeric(gsub(" ","",demographics$PovertyPercent))

# Data Exported from QGIS (schools merged with zones)
##############################
qgis_exports = list.files('data/qgis_exports/')
qgis_list = list()
qgis_list = lapply(paste('data/qgis_exports/',qgis_exports,sep='/'), read.csv, stringsAsFactors = F)

# Then combine list of data frames into single dataframe
qgis_df = rbind.fill(qgis_list)
cols_to_keep = c('DBN','esid_no')

#### Using Only Latest Zones
# Subset Dataframe
qgis_df = qgis_df[qgis_df$math==1 & qgis_df$Year == 2018,cols_to_keep]
qgis_df = unique(qgis_df)


########################################################################################################################
                                                  # Aggregate Data                                                      
########################################################################################################################

##############################
##### Data from NYC DOE (2013-2018)
##############################

# There are four DOE files: charter_math, charter_ela, all_math, and all_ela.
# I need to make sure that they all have consistent column names, then merge them.

# Fix NYC DOE Column Names
nyc_doe_columns = c('DBN','School.Name','Grade','Year','Category','Number.Tested','Mean.Scale.Score',
                    paste(rep(c("Lvl1","Lvl2","Lvl3","Lvl4","Lvl3_4"),each=2),
                          rep(c('_cnt','_per'),5),
                          sep=''))
colnames(charter_math) = colnames(charter_ela) = colnames(all_math) = colnames(all_ela) = nyc_doe_columns

# Bind the four files. Create columns called charter, math, and ela for each file.
df = rbind(charter_math %>%
             mutate(charter = TRUE,
                    math = TRUE,
                    ela = FALSE),
           charter_ela %>%
             mutate(charter = TRUE,
                    math = FALSE,
                    ela = TRUE),
           all_math %>%
             mutate(charter = FALSE,
                    math = TRUE,
                    ela = FALSE),
           all_ela %>%
             mutate(charter = FALSE,
                    math = FALSE,
                    ela = TRUE))

# Subset data to include only these columns
df = df %>% select('DBN','Grade','Year','Number.Tested','Mean.Scale.Score','charter','math')

# Change scores from character to numeric
df$Mean.Scale.Score = as.numeric(df$Mean.Scale.Score)
# df = df[which(!is.na(df$Mean.Scale.Score)),]

##############################
##### Data from NYC Open Data (2006-2012) and (2013-2018)
##############################

#### 2006 - 2012
##############################

# Before merging, I need to create indicators in the data letting me know 
# whether the data is from Math vs ELA or Charter vs TPS
math_files_06 = sapply('Math',grepl,files_nyc_open06,fixed='True')
charter_files_06 = sapply('Charter',grepl,files_nyc_open06,fixed='True')
for(i in 1:4){
  nyc_open06[[i]]$math = math_files_06[i]
  nyc_open06[[i]]$charter = charter_files_06[i]
}

# Bind Datasets, subset to selected columns, convert scores to numeric
nyc_open06_full = rbind.fill(nyc_open06)
cols_to_keep_06 = c('DBN','Grade','Year','Number.Tested','Mean.Scale.Score','charter','math')
nyc_open06_full = nyc_open06_full[,cols_to_keep_06]
nyc_open06_full$Mean.Scale.Score = as.numeric(as.character(nyc_open06_full$Mean.Scale.Score))

#### 2013 - 2018
##############################
math_files_13 = sapply('Math',grepl,files_nyc_open13,fixed='True')
charter_files_13 = sapply('Charter',grepl,files_nyc_open13,fixed='True')

for(i in 1:4){
  nyc_open13[[i]]$math = math_files_13[i]
  nyc_open13[[i]]$charter = charter_files_13[i]
}

# Bind datasets
nyc_open13_full = rbind.fill(nyc_open13)

# Category Column only in 2013-2018 that provides scores for groups such as ELL. 
# Exclude these observations by only keeping data where Category == 'All Students'
nyc_open13_full = nyc_open13_full %>%
  filter(Category == 'All Students')

# Subset to selected columns, convert scores to numeric
cols_to_keep_13 = c('DBN','Grade','Year','Number.Tested','Mean.Scale.Score','charter','math')
nyc_open13_full = nyc_open13_full[,cols_to_keep_13]
nyc_open13_full$Mean.Scale.Score = as.numeric(as.character(nyc_open13_full$Mean.Scale.Score))
nyc_open13_full$Number.Tested = as.numeric(as.character(nyc_open13_full$Number.Tested))

##############################
# For years 2013-2018, I have both Open Data AND DOE data.
# Open Data contains data that is not included in the DOE Data.
# So I want to identify the rows that are contained in Open Data 
# but NOT in DOE data, then merge the two files, excluding duplicates.
##############################

# Quick Inventory of DOE data
# DOE Data
df$math = as.logical(df$math)
doe_inventory = df %>% 
  group_by(DBN, Year, Grade, math) %>%
  dplyr::summarise(n())

# Create ID column
doe_inventory = doe_inventory %>%
  mutate(id = paste(DBN, Year, Grade, math))

# NYC Open Data
open_inventory = nyc_open13_full %>% 
  group_by(DBN, Year, Grade, math) %>%
  dplyr::summarise(n())

# Create ID Column
open_inventory = open_inventory %>%
  mutate(id = paste(DBN, Year, Grade, math))

# Cross Check IDs
# Subset NYC Open Data to Append to Master Dataset
open_data_to_include = open_inventory[!open_inventory$id %in% doe_inventory$id,]

##############################
# Bind Datasets
##############################
df = df %>%
  bind_rows(nyc_open06_full) %>% 
  bind_rows(nyc_open13_full %>%
              mutate(id = paste(DBN, Year, Grade, math)) %>%
              filter(id %in% open_data_to_include$id) %>%
              select(-id))

##############################
##### Add Location and selected Demographics (will add race-related variables next)
##############################

# Join Location with Master File (Location only contains 2013-2018)
master = df %>%
  left_join(locations, by = c('DBN' = 'ATS.SYSTEM.CODE', 'Year' = 'FISCAL_YEAR'))

# Join Locations for 2006-2012
master[master$Year<2013,] = df[df$Year<2013,] %>%
  left_join(locations[locations$FISCAL_YEAR==2013,], by = c('DBN' = 'ATS.SYSTEM.CODE')) %>%
  select(-FISCAL_YEAR)

# Join Demographics with Master File
master = master %>%
  left_join(demographics[,c('DBN','Year','Students.with.Disabilities','English.Language.Learners','PovertyPercent')], by = c('DBN','Year')) %>%
  mutate(Disabled = as.numeric(Students.with.Disabilities),
         Ell = as.numeric(English.Language.Learners),
         PovertyPercent = as.numeric(PovertyPercent)) %>%
  dplyr::select(-Students.with.Disabilities,-English.Language.Learners)

# Join School Zone ID from QGIS
master = master %>% 
  left_join(qgis_df, by=c('DBN'))

########################################################################################################################
                                                  # Diversity Index                                                     
########################################################################################################################

##############################
##### Diversity Index 
##############################

# Shannon Entropy by School
##############################
diversity = demographics %>%
  dplyr::select(DBN, Year, Total.Enrollment, Asian, Black, Hispanic, White)

# percents
percents = diversity[,c(4:7)]/ diversity$Total.Enrollment
colnames(percents) = paste(colnames(percents),'Percent',sep='')

# shannon entropy 
shannon = -apply(percents * log(percents+.001),1,sum)
diversity = cbind(diversity,shannon,percents)

# Merge with Master
master = master %>% 
  left_join(diversity, by = c('DBN', 'Year'))

# Shannon Entropy of TPS by NTA
##############################

diversity = master %>%
  filter(charter == 0) %>%
  replace(., is.na(.), 0) %>%
  group_by(NTA,Year) %>%
  dplyr::summarise(Total.Enrollment = sum(Total.Enrollment),
         Asian = sum(Asian),
         Black = sum(Black),
         Hispanic = sum(Hispanic),
         White = sum(White))
diversity = as.data.frame(diversity)

# percents
percents = diversity[,c(4:7)]/ diversity$Total.Enrollment

# shannon entropy 
shannon_nta = -apply(percents * log(percents+.001),1,sum)
diversity = cbind(diversity[,c('NTA','Year')],shannon_nta)

# Merge with Master
master = master %>% 
  left_join(diversity, by = c('NTA', 'Year'))

# Shannon Entropy of TPS by Community School District
##############################

diversity = master %>%
  filter(charter == 0) %>%
  replace(., is.na(.), 0) %>%
  group_by(GEOGRAPHICAL_DISTRICT_CODE,Year) %>%
  dplyr::summarise(Total.Enrollment = sum(Total.Enrollment),
                   Asian = sum(Asian),
                   Black = sum(Black),
                   Hispanic = sum(Hispanic),
                   White = sum(White))
diversity = as.data.frame(diversity)

# percents
percents = diversity[,c(4:7)]/ diversity$Total.Enrollment

# shannon entropy 
shannon_cd = -apply(percents * log(percents+.001),1,sum)
diversity = cbind(diversity[,c('GEOGRAPHICAL_DISTRICT_CODE','Year')],shannon_cd)

# Merge with Master
master = master %>% 
  left_join(diversity, by = c('GEOGRAPHICAL_DISTRICT_CODE', 'Year'))

master_raw=master
save(master_raw,file='data/raw_merged_master.RDATA')


########################################################################################################################
                                            # Prepare Data for Modeling
########################################################################################################################

# Filter / Subset Dataset
##############################
master = master %>% 
  filter(GEOGRAPHICAL_DISTRICT_CODE != 0) %>%
  filter(Grade != 'All Grades') %>%
  mutate(year_sch = paste(DBN,Year,sep='_'))

# Generate Year/Grade Column
##############################
master$GradeYear = paste(master$math,master$Grade,master$Year,sep='_')

# Remove Missing Scores
##############################
z = which(is.na(master$Mean.Scale.Score))
master = master[-z,]

# Standardize Scores for Math by Grade and Year
##############################
mean_scores_grade_year = tapply(master$Mean.Scale.Score,master$GradeYear,mean)
grade_sd_year = tapply(master$Mean.Scale.Score,master$GradeYear,sd)
grade_years = names(mean_scores_grade_year)

for(i in 1:length(grade_years)){
  master$Mean.Scale.Score[master$GradeYear==grade_years[i]] = (master$Mean.Scale.Score[master$GradeYear==grade_years[i]] - mean_scores_grade_year[i]) / grade_sd_year[i]  
}

# Charter Count (Zone)
#########################
temp = master %>%
  group_by(esid_no, Year) %>%
  dplyr::summarize(charter_count = uniqueN(DBN[charter == 1]),
                   charter_share = uniqueN(DBN[charter == 1])/n_distinct(DBN))
master = master %>% 
  left_join(temp, by=c('Year','esid_no'))

# Charter Count (District)
#########################
temp = master %>%
  group_by(GEOGRAPHICAL_DISTRICT_CODE, Year) %>%
  dplyr::summarize(charter_count_district = uniqueN(DBN[charter == 1]),
                   charter_share_district = uniqueN(DBN[charter == 1])/n_distinct(DBN))

master = master %>% 
  left_join(temp, by=c('Year','GEOGRAPHICAL_DISTRICT_CODE'))

# Charter Performance (Zone)
#########################
temp = master %>%
  filter(charter == 1) %>% 
  group_by(esid_no, Year) %>%
  dplyr::summarize(charter_score = mean(Mean.Scale.Score))
master = master %>% 
  left_join(temp, by=c('Year','esid_no'))

# Cohorts
#########################
master$cohort = master$Year-(as.numeric(master$Grade)-1)

# New Charters in Zone
#########################
new_charters = master %>% 
  group_by(esid_no,Year) %>%
  dplyr::summarize(n = mean(charter_count))

new_charters = new_charters %>%
  mutate(Year2 = Year - 1) %>%
  left_join(new_charters, by = c('Year2' = 'Year', 'esid_no' = 'esid_no')) %>%
  mutate(new_charts = n.x - n.y) %>%
  select(Year, esid_no, new_charts)

new_charters$new_charts[is.na(new_charters$new_charts)] = 0

master = master %>%
  left_join(new_charters, by = c('Year', 'esid_no'))

# Rename some columns for convenience
colnames(master)[8] = 'district'
colnames(master) = tolower(colnames(master))

########################################################################################################################
                                                      # Export
########################################################################################################################

# Export
write.csv(master,'data/master.csv', row.names = F)














