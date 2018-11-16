library(dplyr)
library(plyr)
library(lme4)
library(ggplot2)
setwd('/Users/Chansoo/Desktop/Charter_School_Project/data/')

# Read Data
rm(list=ls())
df = read.csv('master.csv')

# Data Exported from QGIS (schools merged with zones)
##############################
qgis_exports = list.files('qgis_exports/')
qgis_list = list()
qgis_list = lapply(paste('qgis_exports/',qgis_exports,sep='/'), read.csv, stringsAsFactors = F)

# Then combine list of data frames into single dataframe
qgis_df = rbind.fill(qgis_list)
cols_to_keep = c('DBN','esid_no')

#### Using Only Latest Zones
# Subset Dataframe
qgis_df = qgis_df[qgis_df$math==1 & qgis_df$Year == 2018,cols_to_keep]
qgis_df = unique(qgis_df)

# Filter / Subset Dataset
##############################
master = df %>% 
  filter(math == 1,
         Grade != 'All Grades') %>%
  mutate(year_sch = paste(DBN,Year,sep='_'))

master = master %>% 
  left_join(qgis_df, by=c('DBN'))

# Generate Year/Grade Column
master$GradeYear = paste(master$Grade,master$Year,sep='_')

# Standardize Scores for Math by Grade and Year
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
  dplyr::summarize(charter_count = sum(charter))

master = master %>% 
  left_join(temp, by=c('Year','esid_no'))

# Charter Count (District)
#########################
temp = master %>%
  group_by(GEOGRAPHICAL_DISTRICT_CODE, Year) %>%
  dplyr::summarize(charter_count_district = sum(charter))

master = master %>% 
  left_join(temp, by=c('Year','GEOGRAPHICAL_DISTRICT_CODE'))

# Charter Performance (Zone)
#########################
temp = master %>%
  group_by(esid_no, Year) %>%
  filter(charter == 1) %>% 
  dplyr::summarize(charter_score = mean(Mean.Scale.Score))

master = master %>% 
  left_join(temp, by=c('Year','esid_no'))


# Cohorts
#########################
master$cohort = master$Year-(as.numeric(master$Grade)-1)

# New Charters
#########################
new_charters = master %>% 
  group_by(esid_no,Year) %>%
  dplyr::summarize(n = mean(charter_count))

new_charters = new_charters %>%
  mutate(Year2 = Year - 1) %>%
  left_join(new_charters, by = c('Year2' = 'Year', 'esid_no' = 'esid_no')) %>%
  mutate(new = n.x - n.y)

new_charters$new[is.na(new_charters$new)] = 0

master = master %>%
  left_join(new_charters, by = c('Year', 'esid_no'))

#########################
# Preliminary Plots
#########################

# Plot Scores over time for 36 Schools
x = unique(master$DBN)[1:36]
temp = master %>%
  filter(DBN %in% x)
ggplot(data = temp, aes(x = Year, y = Mean.Scale.Score, colour = Grade)) +
  geom_line() + 
  geom_point() + 
  facet_wrap(~DBN, nrow=6)

#########################
# Plots by Grade
#########################

# Plot Scores over time by grade (Colored by Charter)
ggplot(data = master, aes(x = Year, y = Mean.Scale.Score, group = DBN, colour = as.factor(charter)), alpha = 0.4) +
  geom_line() + 
  geom_point() +
  scale_color_manual(values=c("#56B4E9", "#E69F00")) +
  ggtitle("Scores over time by grade") +
  labs(colour = 'charter') +
  facet_wrap(~Grade)

# Plot Scores over time by grade (Colored by # of Charters in Zone)
DBN.list = unique(master$DBN)
DBN.samples = sample(DBN.list,length(DBN.list)*0.25)
temp = master %>% filter(DBN %in% DBN.samples)
ggplot(data = temp, aes(x = Year, y = Mean.Scale.Score, group = DBN, colour = charter_count), alpha = 0.6) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by grade \n (displaying 25% of schools randomly selected)") +  
  labs(colour = '# charter \n in zone') +
  facet_wrap(~Grade)

# Plot Scores over time by grade (Colored by District)
x = unique(master$GEOGRAPHICAL_DISTRICT_CODE)[1:5]
temp = master %>%
  filter(GEOGRAPHICAL_DISTRICT_CODE %in% x)
ggplot(data = temp, aes(x = Year, y = Mean.Scale.Score, group = DBN, colour = as.factor(GEOGRAPHICAL_DISTRICT_CODE)), alpha = 0.3) +
  geom_line() + 
  geom_point() +
  ggtitle("Scores over time by grade") +    
  labs(colour = 'District') +
  facet_grid(cols=vars(Grade))

# Plot Scores over time for Districts
master$charter_count
temp = master %>%
  group_by(Year,Grade,GEOGRAPHICAL_DISTRICT_CODE) %>%
  dplyr::summarise(mean_district_scores = mean(Mean.Scale.Score),
                   charter_count_district = mean(charter_count_district))
ggplot(data = temp, aes(x=Year, y = mean_district_scores, group = GEOGRAPHICAL_DISTRICT_CODE, colour = charter_count_district), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by grade") +    
  labs(colour = '# charter \n in district') +
  facet_wrap(~Grade)

##############################
# Plots By Cohort
##############################

# Plot Scores over time by grade (Colored by Charter)
ggplot(data = master, aes(x = Year, y = Mean.Scale.Score, group = DBN, colour = as.factor(charter)), alpha = 0.4 ) +
  geom_line() + 
  geom_point() +
  scale_color_manual(values=c("#56B4E9", "#E69F00")) +
  ggtitle("Scores over time by cohort \n (cohort = year student started 3rd grade)") +    
  labs(colour = 'charter') +  
  facet_wrap(~cohort)

# Plot Scores over time by grade (Colored by # of Charters in Zone)
ggplot(data = master, aes(x = Year, y = Mean.Scale.Score, group = DBN, colour = charter_count), alpha = 0.4) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by cohort \n (cohort = year student started 3rd grade)") +    
  labs(colour = '# charter \n in zone') +    
  facet_wrap(~cohort)

# Plot Scores over time by grade (Colored by District)
x = unique(master$GEOGRAPHICAL_DISTRICT_CODE)[1:5]
temp = master %>%
  filter(GEOGRAPHICAL_DISTRICT_CODE %in% x)
ggplot(data = temp, aes(x = Year, y = Mean.Scale.Score, group = DBN, colour = as.factor(GEOGRAPHICAL_DISTRICT_CODE)), alpha = 0.3) +
  geom_line() + 
  geom_point() +
  ggtitle("Scores over time by cohort \n (cohort = year student started 3rd grade)") +    
  labs(colour = 'District') +  
  facet_grid(cols=vars(cohort))

# Plot Scores over time for Districts
temp = master %>%
  group_by(Year,cohort,GEOGRAPHICAL_DISTRICT_CODE) %>%
  dplyr::summarise(mean_district_scores = mean(Mean.Scale.Score),
                   charter_count_district = mean(charter_count_district))
ggplot(data = temp, aes(x=Year, y = mean_district_scores, group = GEOGRAPHICAL_DISTRICT_CODE, colour = charter_count_district), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by cohort \n (cohort = year student started 3rd grade)") +    
  labs(colour = '# charter \n in district') +  
  facet_wrap(~cohort)

##############################
# Plots based on # of New Charters
##############################

# Plot Scores over time by grade (Colored by # of New Charters in Zone)
DBN.list = unique(master$DBN)
DBN.samples = sample(DBN.list,length(DBN.list)*0.25)
temp = master %>% filter(DBN %in% DBN.samples)
ggplot(data = temp, aes(x = Year, y = Mean.Scale.Score, group = DBN, colour = new), alpha = 0.6) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by grade \n (displaying 25% of schools randomly selected)") +  
  labs(colour = '# new charters \n in zone') +
  facet_wrap(~Grade)

# Plot Scores over time by grade (Colored by # of New Charters in Zone)
ggplot(data = master, aes(x = Year, y = Mean.Scale.Score, group = DBN, colour = new), alpha = 0.4) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by cohort \n (cohort = year student started 3rd grade)") +    
  labs(colour = '# new charters \n in zone') +    
  facet_wrap(~cohort)

##############################
# Misc. Plots
##############################

temp = master %>%
  group_by(Year,cohort,GEOGRAPHICAL_DISTRICT_CODE) %>%
  dplyr::summarise(mean_district_scores = mean(Mean.Scale.Score),
                   charter_count_district = mean(charter_count_district))
ggplot(data = temp, aes(x=Year, y = charter_count_district, group = GEOGRAPHICAL_DISTRICT_CODE, colour = mean_district_scores), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="blue", high="red") +
  ggtitle("# of Charter Schools by District Over Time") +    
  labs(colour = 'Mean District Score') 



##############################
# Model
##############################

master2 = master %>% 
  filter(charter == 0) 
master2 = master2[!is.na(master2$esid_no),]

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

