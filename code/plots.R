library(dplyr)
library(plyr)
library(lme4)
library(ggplot2)
setwd('/Users/Chansoo/Desktop/Charter_School_Project/data/')

# Read Data
rm(list=ls())
master = read.csv('master.csv')
set.seed(1234)

#########################
# Preliminary Plots
#########################

# Plot Scores over time for 16 Random Schools
temp = master %>%
  filter(DBN %in% unique(master$DBN)[sample(length(unique(master$DBN)),16)])
ggplot(data = temp, aes(x = Year, y = Mean.Scale.Score, group = Grade, colour = Grade)) +
  geom_line() + geom_point() + 
  facet_wrap(~DBN, nrow=4)

#########################
# Plots by Grade
#########################

# Plot Scores over time by grade (Colored by Charter)
ggplot(data = master, aes(x = Year, y = Mean.Scale.Score, group = DBN, colour = as.factor(charter)), alpha = 0.4) +
  geom_line() + geom_point() +
  scale_color_manual(values=c("#56B4E9", "#E69F00")) +
  ggtitle("Scores over time by grade") +
  labs(colour = 'charter') +
  facet_wrap(~Grade)

# Scores Ending before 2018
end_2012 = master %>%
  group_by(DBN) %>%
  dplyr::summarise(max=max(Year)) %>%
  mutate(y2012 = max==2012) %>%
  filter(max < 2018)

# Plot Schools with Scores Ending in 2012
temp = master %>% 
  filter(DBN %in% end_2012$DBN) %>%
  left_join(end_2012, by = 'DBN') 
ggplot(data = temp, aes(x = Year, y = Mean.Scale.Score, group = DBN, colour = as.factor(y2012)), alpha = 0.4) +
  geom_line() + geom_point() +
  scale_color_manual(values=c("#56B4E9", "#E69F00")) +
  ggtitle("Scores over time by grade") +
  labs(colour = 'Last year = 2012') +
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
temp = master %>%
  filter(GEOGRAPHICAL_DISTRICT_CODE>0) %>%
  group_by(Year,Grade,GEOGRAPHICAL_DISTRICT_CODE) %>%
  dplyr::summarise(mean_district_scores = mean(Mean.Scale.Score),
                   charter_share_district = mean(charter_share_district))
ggplot(data = temp, aes(x=Year, y = mean_district_scores, group = GEOGRAPHICAL_DISTRICT_CODE, colour = charter_share_district), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by grade") +    
  labs(colour = '% charter \n in district') +
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
                   charter_share_district = mean(charter_share_district))
ggplot(data = temp, aes(x=Year, y = mean_district_scores, group = GEOGRAPHICAL_DISTRICT_CODE, colour = charter_share_district), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by cohort \n (cohort = year student started 3rd grade)") +    
  labs(colour = '% charter \n in district') +  
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


