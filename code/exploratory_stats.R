library(dplyr)
library(plyr)
library(lme4)
library(ggplot2)
setwd('/Users/Chansoo/Desktop/Charter_School_Project/data/')

# Read Data
master = read.csv('master.csv')
master = master %>%
  filter(math == 1)

##############################
# School Zones
##############################

# # of school zones
length(unique(master$esid_no))

# # of zones with >1 TPS elem schools
master %>% 
  filter(charter == 0) %>% 
  group_by(esid_no) %>%
  dplyr::summarise(n_s = n_distinct(DBN)) %>%
  filter(n_s > 1)

# # of zones with >7 TPS elem schools
master %>% 
  filter(charter == 0) %>%
  group_by(esid_no) %>%
  dplyr::summarise(n_s = n_distinct(DBN)) %>%
  filter(n_s > 7)

# # of zones over time
master %>%
  group_by(Year) %>%
  dplyr::summarise(n_s = n_distinct(esid_no))

##############################
# Districts
##############################

# # of school zones
length(unique(master$GEOGRAPHICAL_DISTRICT_CODE))

# # of zones with >1 TPS elem schools
master %>%
  filter(charter == 0) %>%
  group_by(GEOGRAPHICAL_DISTRICT_CODE) %>%
  dplyr::summarise(n_s = n_distinct(DBN)) %>%
  filter(n_s > 1)

# # of zones over time
master %>%
  group_by(Year) %>%
  dplyr::summarise(n_s = n_distinct(GEOGRAPHICAL_DISTRICT_CODE))

# Schools Missing Districts
length(unique(master$DBN[is.na(master$GEOGRAPHICAL_DISTRICT_CODE)]))


##############################
# Charter Schools
##############################

# # of charters over time
master %>% 
  filter(charter == 1) %>% 
  group_by(Year) %>%
  dplyr::summarise(n_s = n_distinct(DBN))

# # of zones with >1 charter schools
master %>% 
  filter(charter == 1) %>%
  group_by(esid_no) %>%
  dplyr::summarise(n_s = n_distinct(DBN)) %>%
  filter(n_s > 7)

# # of charter schools over time
master %>%
  group_by(Year) %>%
  dplyr::summarise(n_s = n_distinct(esid_no))


##############################
# Missing Data
##############################

missing = as.data.frame( round( apply(is.na(master),
                                      2,
                                      sum) / nrow(master),
                                2)
                         )
colnames(missing) = 'percent_missing'
missing

###############################################################################################################










