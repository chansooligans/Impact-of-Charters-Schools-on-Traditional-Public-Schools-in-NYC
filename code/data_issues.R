library(ggplot2)
library(dplyr)
library(reshape2)
setwd('/Users/Chansoo/Desktop/Charter_School_Project/')
load(file='data/raw_merged_master.RDATA')
master = read.csv('data/master.csv',stringsAsFactors = F)

#########################
# Demographics Missing Poverty % from 2010, 2011, 2012 for all schools
#########################

master %>% 
  mutate(missing_ind = !is.na(povertypercent)) %>%
  group_by(year) %>%
  dplyr::summarize(data_avail=sum(missing_ind))

apply(is.na(master),2,sum)

# Missing Students.with.Disabilities
master %>% 
  mutate(missing_ind = is.na(disabled)) %>%
  group_by(year) %>%
  dplyr::summarize(missing=sum(missing_ind))

# Missing English.Language.Learners
master %>% 
  mutate(missing_ind = is.na(ell)) %>%
  group_by(year) %>%
  dplyr::summarize(missing=sum(missing_ind))


##################################################
# Location Check
##################################################

sum(is.na(master$location.1))

##################################################
# DBN Check
##################################################

# Number of Schools by Year
master %>%
  group_by(year) %>%
  dplyr::summarise(n=n_distinct(dbn))

# Number of Charter Schools by Year
master %>%
  filter(charter==1) %>%
  group_by(year) %>%
  dplyr::summarise(n=n_distinct(dbn))

# 
charters = master %>%
  filter(charter==1) %>%
  group_by(district,year) %>%
  dplyr::summarise(n=n_distinct(dbn)) %>%
  print(n=320)

ggplot(charters, aes(district, year)) + 
  geom_tile(aes(fill = n),colour = "white") + 
  scale_fill_gradient(low = "white",high = "steelblue")

# Count of # of Charters by Year and District
charters = dcast(charters,year ~ district)
charters[is.na(charters)]=0
row.names(charters) = charters$year
charters = charters %>% select(-year)
charters

# First Difference of # of Charters by Year and District
charters_copy = rbind(charters,rep(0,length(charters)))
charters_lag = rbind(rep(0,length(charters)),charters)
temp = charters_copy - charters_lag
temp = temp[1:13,]
temp

check = master %>%
  group_by(district) %>%
  dplyr::summarise(n=n_distinct(dbn))

# 


##################################################
# Districts with Most Charters
##################################################

# Find Charter:Student Ratio in 2018
count_charts = master %>%
  filter(charter==1,year==2018) %>%
  group_by(district) %>%
  dplyr::summarise(count_charts=n_distinct(dbn))

check = master %>%
  filter(math==1,year==2018,!is.na(total.enrollment)) %>%
  left_join(count_charts, by = 'district') %>%
  group_by(district) %>%
  dplyr::summarise(mean_charters = mean(count_charts),
                   tot_enrolled = sum(total.enrollment),
                   charts_to_studs_ratio=mean_charters/tot_enrolled) %>%
  arrange(charts_to_studs_ratio)

check$charts_to_studs_ratio[is.na(check$charts_to_studs_ratio)] = 0

# Histogram of Charter to Student Ratio
hist(check$charts_to_studs_ratio, breaks = 8)

# High Charter to Student Ratio = District # 7,16,13,5 (each around 5k)
# Low Charter to Student Ratio = District # 10,31,27,24 (each around 45k-60k)

# High:
# 7 = South Bronx
# 16 = Bedstuy/Bushwick
# 13 = Clinton Hill
# 5 = Morningside Heights

# Low:
# 10 = Bronx Community School District (near Fordham)
# 31 = Staten Island
# 27 = Ozone Park
# 24 = Corona

master$high = 'med'
master$high[master$district %in% c(20,25,26)] = 'no_charters'
master$high[master$district %in% c(10,31,27,24)] = 'low'
master$high[master$district %in% c(7,16,13,5)] = 'high'

colnames(master)


# Demographic Composition of Traditional Public Schools by High vs Low Charter:Student Ratio
#########################
# 4th Grade, in year 2018

# Demographics of TPS in 2018, grouped by high vs low vs medium charter:student ratio
master %>%
  filter(math==1,grade==4,year==2018,charter==F) %>%
  filter(!is.na(total.enrollment)) %>%
  group_by(high) %>%
  dplyr::summarize(asian = sum(asian)/sum(total.enrollment),
                   black = sum(black)/sum(total.enrollment),
                   hispanic = sum(hispanic)/sum(total.enrollment),
                   white = sum(white)/sum(total.enrollment),
                   poverty_percent = mean(povertypercent),
                   ell = mean(ell),
                   disabled = mean(disabled))

# Demographics of TPS in *2008*, grouped by high vs low vs medium charter:student ratio (where ratio computed in *2018*)
master %>%
  filter(math==1,grade==4,year==2008,charter==F) %>%
  filter(!is.na(total.enrollment)) %>%
  group_by(high) %>%
  dplyr::summarize(asian = sum(asian)/sum(total.enrollment),
                   black = sum(black)/sum(total.enrollment),
                   hispanic = sum(hispanic)/sum(total.enrollment),
                   white = sum(white)/sum(total.enrollment),
                   poverty_percent = mean(povertypercent),
                   ell = mean(ell),
                   disabled = mean(disabled))


# Demographic Composition of Charter vs Traditional
# 4th Grade, in year 2018
#########################
master %>%
  filter(math==1,grade==4,year==2018) %>%
  group_by(charter) %>%
  dplyr::summarize(asian = sum(asian)/sum(total.enrollment),
                   black = sum(black)/sum(total.enrollment),
                   hispanic = sum(hispanic)/sum(total.enrollment),
                   white = sum(white)/sum(total.enrollment),
                   poverty_percent = mean(povertypercent),
                   ell = mean(ell),
                   disabled = mean(disabled))

# TPS Scores, grouped by high/low/medium charter:student ratio
#########################

sum(is.na(master$mean.scale.score))

master %>%
  filter(math==1,grade==4,year==2018,charter==F) %>%
  filter(!is.na(mean.scale.score)) %>%
  group_by(high) %>%
  dplyr::summarize(mu = mean(mean.scale.score))
colnames(master)



##################################################
# Distribution of Demographics
##################################################

check = master %>%
  filter(math==1,grade==4,year==2018) %>%
  group_by(district) %>%
  dplyr::summarize(asian = sum(asian)/sum(total.enrollment),
                   black = sum(black)/sum(total.enrollment),
                   hispanic = sum(hispanic)/sum(total.enrollment),
                   white = sum(white)/sum(total.enrollment),
                   poverty_percent = mean(povertypercent),
                   ell = mean(ell),
                   disabled = mean(disabled),
                   chart_count = sum(charter)) %>%
  print(n=32)

sum(check$chart_count)

plot(check$asian,check$chart_count)
plot(check$black,check$chart_count)
plot(check$black,check$chart_count)

##################################################
# NAs from Scores
##################################################

master_raw %>%
  filter(math==1,charter==0,is.na(Mean.Scale.Score)) %>%
  group_by(Year,Grade) %>%
  dplyr::summarize(n=n())















