rm(list=ls())
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')
nyc_open06_slug = 'data/academic_2006-2012'
nyc_open13_slug = 'data/academic_2013-2018'
nyc_doe_slug = 'data/nyc_doe'

##### Raw Data Sources:

#####
# NYC Open Data

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

#####
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



##############################
##### Cross Check DOE data and Open Data
##############################

##### Master: Aggregate NYC DOE Test Scores from NYC DOE Data (2013-2018)
##############################

# Fix NYC DOE Column Names
nyc_doe_columns = c('DBN','School.Name','Grade','Year','Category','Number.Tested','Mean.Scale.Score',
                    paste(rep(c("Lvl1","Lvl2","Lvl3","Lvl4","Lvl3_4"),each=2),
                          rep(c('_cnt','_per'),5),
                          sep=''))

colnames(charter_math) = colnames(charter_ela) = colnames(all_math) = colnames(all_ela) = nyc_doe_columns


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

df = df %>% select('DBN','Grade','Year','Number.Tested','Mean.Scale.Score','charter','math')

df$Mean.Scale.Score = as.numeric(df$Mean.Scale.Score)

# Remove Missing Scores ## !!!!!!!!!!!!!!!!!!!!
df = df[which(!is.na(df$Mean.Scale.Score)),]


##### Master: Aggregate NYC DOE Test Scores from NYC Open Data 2
##############################

math_files_13 = sapply('Math',grepl,files_nyc_open13,fixed='True')
charter_files_13 = sapply('Charter',grepl,files_nyc_open13,fixed='True')

for(i in 1:4){
  nyc_open13[[i]]$math = math_files_13[i]
  nyc_open13[[i]]$charter = charter_files_13[i]
}

nyc_open13_full = rbind.fill(nyc_open13)

cols_to_keep_13 = c('DBN','Grade','Year','Number.Tested','Mean.Scale.Score','charter','math')

nyc_open13_full = nyc_open13_full[,cols_to_keep_13]

unique(all_math %>% arrange(DBN) %>% select(DBN))

df2 = nyc_open13_full

#####

# DOE Data
df$math = as.logical(df$math)
check1 = df %>% 
  group_by(DBN, Year, Grade, math) %>%
  dplyr::summarise(n())

check1$id = paste(check1$DBN, check1$Year, check1$Grade, check1$math)

# NYC Open Data
check2 = df2 %>% 
  group_by(DBN, Year, Grade, math) %>%
  dplyr::summarise(n())

check2$id = paste(check2$DBN, check2$Year, check2$Grade, check2$math)

open_data_to_include = check2[!check2$id %in% check1$id,]

df2 %>%
  mutate(id = paste(DBN, Year, Grade, math)) %>%
  filter(id %in% open_data_to_include$id) %>%
  select(-id)


