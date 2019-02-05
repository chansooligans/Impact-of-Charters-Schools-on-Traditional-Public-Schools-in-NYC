library(dplyr)
library(plyr)
library(lme4)
library(ggplot2)
setwd('/Users/Chansoo/Desktop/Charter_School_Project/data/')

#########################
# Load Data
#########################
rm(list=ls())
master = read.csv('master.csv')
master = master %>%
  mutate(cohort = cohort + 2) %>%
  filter(math == 1,
         grade <= 6)
set.seed(1234)



#########################
# Plots -- District Averages
#########################
temp = master %>%
  filter(cohort %in% c(2008,2012,2014)) %>%
  group_by(district, year, cohort, charter_share_district) %>%
  dplyr::summarise(y = mean(mean.scale.score))

ggplot(data = temp, aes(x=year, y = y, group = district, colour = charter_share_district), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("District Averages over Time by Cohort") +     
  labs(colour = '% charter \n in district') +
  facet_wrap(~cohort)

ggplot(data = temp, aes(x=year, y = y, group = district, colour = charter_share_district), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  geom_smooth(method='lm',formula=y~x, color='blue', se=F) +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("District Averages over Time by Cohort") +    
  labs(colour = '% charter \n in district') +
  facet_wrap(~cohort)



#########################
# Plots -- School Averages
#########################
temp = master %>%
  filter(district %in% c(1,11,21,29)) %>%
  group_by(district, dbn, year, grade, charter_share) %>%
  dplyr::summarise(y = mean(mean.scale.score))

ggplot(data = temp, aes(x=year, y = y, group = dbn, colour = charter_share), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("School Average Scores Over Time by District") +    
  labs(colour = '% charter \n in school zone') +
  facet_wrap(~district)

ggplot(data = temp, aes(x=year, y = y, group = dbn, colour = charter_share), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  geom_smooth(method='lm',formula=y~x, color = 'blue', se=F) +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("School Average Scores Over Time by District") +     
  labs(colour = '% charter \n in school zone') +
  facet_wrap(~district)



#########################
# Plots -- Cohort Averages
#########################

temp = master %>%
  filter(dbn %in% unique(master$dbn)[1:30]) %>%
  mutate(cohort = cohort + 2) %>%
  group_by(dbn, cohort, year, grade, charter_share) %>%
  dplyr::summarise(y = mean(mean.scale.score))

ggplot(data = temp, aes(x=year, y = y), alpha = 0.8) +
  geom_line(aes(group = cohort, color = as.factor(cohort))) + 
  geom_point(aes(group = cohort, color = as.factor(cohort))) +
  ggtitle("Scores over time by grade") +    
  labs(colour = 'Cohort (Year Class Started 3rd Grade') +
  facet_wrap(~dbn)

ggplot(data = temp, aes(x=year, y = y), alpha = 0.8) +
  geom_line(aes(group = cohort, color = as.factor(cohort))) + 
  geom_point(aes(group = cohort, color = as.factor(cohort))) +
  ggtitle("Scores over time by grade") +    
  labs(colour = 'Cohort (Year Class Started 3rd Grade') +
  facet_wrap(~dbn)

#########################
# Plots -- Charter Schools
#########################

temp = master %>%
  filter(math == 1) %>%
  group_by(district, esid_no, charter_score, year) %>%
  dplyr::summarise(y = mean(charter_score))

ggplot(data = temp, aes(x=year, y = y, group = as.factor(esid_no), color = as.factor(esid_no))) +
  geom_point(show.legend=FALSE) + 
  geom_line(show.legend=FALSE) +
  facet_wrap(~district)


