# Dependencies
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)

# File Locations
rm(list=ls())
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')

##### Read Data
##############################
master_loc = read.csv('data/master_loc.csv', stringsAsFactors = F)

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

master_2 = master_2 %>% filter(abs(lon) > 0) #filter out schools missing long/lat

##### Get Demographics
##############################
demographics = read.csv('data/demographics/2013_-_2018_Demographic_Snapshot_School.csv', stringsAsFactors = F)

# Fix Column Names
colnames_to_fix = colnames(demographics)[19:38]
colnames_to_fix = substr(colnames_to_fix,4,nchar(colnames_to_fix))
colnames_to_fix = gsub(".1",'Percent',colnames_to_fix)
colnames(demographics)[19:38] = colnames_to_fix

# Fix Year Column
demographics$Year = as.integer(paste('20',substr(demographics$Year,6,7),sep=''))

# Check matching DBN
table(master_2$DBN %in% demographics$DBN)

# Merge with master; Set Grade to All Grades, Set Year > 2013
master_demo = master_2 %>%
  filter(Grade == 'All Grades', Year > 2013) %>%
  left_join(demographics %>% select(-School.Name), by = c('DBN'='DBN', 'Year' = 'Year'))

# Check for NAs
apply(is.na(master_demo),2,sum)

##### Get Diversity Index
##############################
diversity = master_demo %>%
  filter(Grade == 'All Grades', math == 1) %>%
  select(DBN, Year, Total.Enrollment, Asian, Black, Hispanic, Multiple.Race.Categories.Not.Represented, White)

for(i in 3:8){
  diversity[,i] = as.numeric(gsub(',','',diversity[,i]))
}

# check
diversity$Total.Enrollment -  apply(diversity[,c(4:8)],1,sum)

# percents
percents = diversity[,c(4:8)]/ diversity$Total.Enrollment

# shannon entropy 
shannon = -apply(percents * log(percents+.001),1,sum)
diversity = cbind(diversity,shannon)

# Merge with Master
master_2 = master_2 %>% 
  left_join(diversity, by = c('DBN', 'Year'))

##### Elementary Schools (MATH) (Schools with Max Grade = 6), Year > 2013
##############################

elem = master_2 %>% 
  filter(Grade >= 3, Grade <= 6) %>%
  group_by(DBN) %>%
  dplyr::summarize(min = min(Grade), max = max(Grade))

elem_math = master_2 %>% filter(DBN %in% elem$DBN, 
                                Grade == 'All Grades', 
                                math == 1, 
                                Year>2013)


years = unique(elem_math$Year)
for(i in 1:length(years)){
  export_file = elem_math %>% filter(Year == years[i])
  write.csv(export_file,paste('data/merged_scores_and_longlat/elem_math_allgrades_',years[i],'.csv',sep=''), row.names = F)
}
