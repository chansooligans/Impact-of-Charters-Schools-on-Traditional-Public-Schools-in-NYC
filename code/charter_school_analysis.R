library(dplyr)
library(plyr)
library(lme4)
library(ggplot2)
setwd('/Users/Chansoo/Desktop/Charter_School_Project/data/')

# Read Data
master = read.csv('master.csv')

##############################
# Model
##############################

master2 = master %>% 
  filter(charter == 0) 

mm.mod1 = lmer(Mean.Scale.Score ~ charter_count + Poverty + Disabled + Ell + Asian + Black + Hispanic + charter_score + new +
                 (charter_count | GEOGRAPHICAL_DISTRICT_CODE), data = master2)

mm.mod2 = lmer(Mean.Scale.Score ~ charter_count + Poverty + Disabled + Ell + Asian + Black + Hispanic + charter_score + new +
                 (charter_count | GEOGRAPHICAL_DISTRICT_CODE/DBN), data = master2)

mm.mod3 = lmer(Mean.Scale.Score ~ charter_count + Poverty + Disabled + Ell + Asian + Black + Hispanic + Year + cohort + charter_score + new +
                 (charter_count | GEOGRAPHICAL_DISTRICT_CODE/DBN), data = master2)

summary(mm.mod1)
summary(mm.mod2)
summary(mm.mod3)

anova(mm.mod1, mm.mod2, refit = F)

