library(dplyr)
library(plyr)
library(lme4)
library(ggplot2)
setwd('/Users/Chansoo/Desktop/Charter_School_Project/data/')

# Read Data
rm(list=ls())
master = read.csv('master.csv')
master = master %>%
  filter(math == 1,
         grade < 6)
set.seed(1234)

plot_list = list()

#########################
# Preliminary Plots
#########################

# Plot Scores over time for 16 Random Schools
temp = master %>%
  filter(dbn %in% unique(master$dbn)[sample(length(unique(master$dbn)),16)])
plot_list[[1]] = ggplot(data = temp, aes(x = year, y = mean.scale.score, group = grade, colour = as.factor(grade))) +
  geom_point() + 
  geom_smooth(method='lm',formula=y~x,se=F) +
  facet_wrap(~dbn, nrow=4) +
  ggtitle("Scores over time for 16 Random Schools")
  
# Plot Scores over time for 16 Random Zones (4th Grade Scores)
temp = master %>%
  filter(esid_no %in% unique(master$esid_no)[sample(length(unique(master$esid_no[master$charter_count>0])),16)]) %>%
  filter(grade == 4)
plot_list[[2]] = ggplot(data = temp, aes(x = year, y = mean.scale.score, group = dbn, colour = as.factor(charter))) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method='lm',formula=y~x,se=F) +
  facet_wrap(~esid_no, nrow=4) +
  ggtitle("4th Grade Scores for 16 random zones")

# Plot Scores over time for 16 Random Districts (4th Grade Scores)
temp = master %>%
  filter(district %in% unique(master$district)[sample(length(unique(master$district)),16)])
plot_list[[3]] = ggplot(data = temp, aes(x = year, y = mean.scale.score, group = dbn, colour = as.factor(charter))) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method='lm',formula=y~x,se=F) +
  facet_wrap(~district, nrow=4) +
  ggtitle("4th Grade Scores for 16 random districts")

#########################
# Plots by grade
#########################

# Plot Scores over time by grade (Colored by Charter)
plot_list[[4]] = ggplot(data = master, aes(x = year, y = mean.scale.score, group = dbn, colour = as.factor(charter)), alpha = 0.4) +
  geom_line() + geom_point() +
  scale_color_manual(values=c("#56B4E9", "#E69F00")) +
  ggtitle("Scores over time by grade") +
  labs(colour = 'charter') +
  facet_wrap(~grade)

# Scores Ending before 2018
end_2012 = master %>%
  group_by(dbn) %>%
  dplyr::summarise(max=max(year)) %>%
  mutate(y2012 = max==2012) %>%
  filter(max < 2018)

# Plot Schools with Scores Ending in 2012
temp = master %>% 
  filter(dbn %in% end_2012$dbn) %>%
  left_join(end_2012, by = 'dbn') 
plot_list[[5]] = ggplot(data = temp, aes(x = year, y = mean.scale.score, group = dbn, colour = as.factor(y2012)), alpha = 0.4) +
  geom_line() + geom_point() +
  scale_color_manual(values=c("#56B4E9", "#E69F00")) +
  ggtitle("Schools that were not opened in 2018 (i.e. schools that have closed, highlighting ones that closed in 2012)") +
  labs(colour = 'Last year = 2012') +
  facet_wrap(~grade)

# Plot Scores over time by grade (Colored by # of Charters in Zone)
dbn.list = unique(master$dbn)
dbn.samples = sample(dbn.list,length(dbn.list)*0.25)
temp = master %>% filter(dbn %in% dbn.samples)
plot_list[[6]] = ggplot(data = temp, aes(x = year, y = mean.scale.score, group = dbn, colour = charter_count), alpha = 0.6) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by grade \n (displaying 25% of schools randomly selected)") +  
  labs(colour = '# charter \n in zone') +
  facet_wrap(~grade)

# Plot Scores over time by grade (Colored by District)
x = unique(master$district)[1:5]
temp = master %>%
  filter(district %in% x)
plot_list[[7]] = ggplot(data = temp, aes(x = year, y = mean.scale.score, group = dbn, colour = as.factor(district)), alpha = 0.3) +
  geom_line() + 
  geom_point() +
  ggtitle("Scores over time by grade") +    
  labs(colour = 'District') +
  facet_grid(cols=vars(grade))

# Plot Scores over time for Districts
temp = master %>%
  filter(district>0) %>%
  group_by(year,grade,district) %>%
  dplyr::summarise(mean_district_scores = mean(mean.scale.score),
                   charter_share_district = mean(charter_share_district))
plot_list[[8]] = ggplot(data = temp, aes(x=year, y = mean_district_scores, group = district, colour = charter_share_district), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by grade") +    
  labs(colour = '% charter \n in district') +
  facet_wrap(~grade)


##############################
# Plots By Cohort
##############################

# Plot Scores over time by grade (Colored by Charter)
plot_list[[9]] = ggplot(data = master, aes(x = year, y = mean.scale.score, group = dbn, colour = as.factor(charter)), alpha = 0.4 ) +
  geom_line() + 
  geom_point() +
  scale_color_manual(values=c("#56B4E9", "#E69F00")) +
  ggtitle("Scores over time by cohort \n (cohort = year student started 3rd grade)") +    
  labs(colour = 'charter') +  
  facet_wrap(~cohort)

# Plot Scores over time by grade (Colored by # of Charters in Zone)
plot_list[[10]] = ggplot(data = master, aes(x = year, y = mean.scale.score, group = dbn, colour = charter_count), alpha = 0.4) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by cohort \n (cohort = year student started 3rd grade)") +    
  labs(colour = '# charter \n in zone') +    
  facet_wrap(~cohort)

# Plot Scores over time by grade (Colored by District)
x = unique(master$district)[1:5]
temp = master %>%
  filter(district %in% x)
plot_list[[11]] = ggplot(data = temp, aes(x = year, y = mean.scale.score, group = dbn, colour = as.factor(district)), alpha = 0.3) +
  geom_line() + 
  geom_point() +
  ggtitle("Scores over time by cohort \n (cohort = year student started 3rd grade)") +    
  labs(colour = 'District') +  
  facet_grid(cols=vars(cohort))

# Plot Scores over time for Districts
temp = master %>%
  group_by(year,cohort,district) %>%
  dplyr::summarise(mean_district_scores = mean(mean.scale.score),
                   charter_share_district = mean(charter_share_district))
plot_list[[12]] = ggplot(data = temp, aes(x=year, y = mean_district_scores, group = district, colour = charter_share_district), alpha = 0.8) +
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
dbn.list = unique(master$dbn)
dbn.samples = sample(dbn.list,length(dbn.list)*0.25)
temp = master %>% filter(dbn %in% dbn.samples)
plot_list[[13]] = ggplot(data = temp, aes(x = year, y = mean.scale.score, group = dbn, colour = new_charts), alpha = 0.6) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="grey", high="red") +
  ggtitle("Scores over time by grade \n (displaying 25% of schools randomly selected)") +  
  labs(colour = '# new charters \n in zone') +
  facet_wrap(~grade)

# Plot Scores over time by grade (Colored by # of New Charters in Zone)
plot_list[[14]] = ggplot(data = master, aes(x = year, y = mean.scale.score, group = dbn, colour = new_charts), alpha = 0.4) +
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
  group_by(year,cohort,district) %>%
  dplyr::summarise(mean_district_scores = mean(mean.scale.score),
                   charter_count_district = mean(charter_count_district))
plot_list[[15]] = ggplot(data = temp, aes(x=year, y = charter_count_district, group = district, colour = mean_district_scores), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  scale_color_gradient(low="blue", high="red") +
  ggtitle("# of Charter Schools by District Over Time") +    
  labs(colour = 'Mean District Score') 

save(plot_list,file='plot_list.RDATA')

