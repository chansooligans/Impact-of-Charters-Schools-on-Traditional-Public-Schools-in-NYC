# Dependencies
library(dplyr)
library(tidyr)
library(openxlsx)
library(data.table)

# File Locations
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')

#########################
##### Load Data
#########################

nyc_puma_NTA = read.xlsx('data/census/nyc2010census_tabulation_equiv.xlsx', 
                        sheet = 2, 
                        startRow = 7,
                        colNames = TRUE)
nyc_pumas = as.integer(nyc_puma_NTA$PUMA.Code)

nyc = fread('data/census/ss16pny.csv')

#########################
##### Subset Data
#########################

nyc = nyc[nyc$PUMA %in% nyc_pumas,]
colnames(nyc)

cols_to_keep = c('PUMA',
                 'PWGTP',
                 'RAC1P',
                 'AGEP',
                 'LANP')
nyc = nyc %>%
  select(cols_to_keep)

nyc %>% 
  group_by(PUMA, RAC1P) %>%
  summarise(population = n())

