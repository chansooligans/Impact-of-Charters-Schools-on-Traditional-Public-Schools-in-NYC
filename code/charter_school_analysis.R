library(dplyr)
library(plyr)
library(lme4)
setwd('/Users/Chansoo/Desktop/Charter_School_Project/data/')

# Read Data
df = read.csv('master.csv')

# Data Exported from QGIS (schools merged with zones)
##############################
qgis_exports = list.files('qgis_exports/')
qgis_list = list()
qgis_list = lapply(paste('qgis_exports/',qgis_exports,sep='/'), read.csv, stringsAsFactors = F)

# Then combine list of data frames into single dataframe
qgis_df = rbind.fill(qgis_list)
cols_to_keep = c('DBN','esid_no','Year')

# Subset Dataframe
qgis_df = qgis_df[qgis_df$math==1,cols_to_keep]

# Filter / Subset Dataset
##############################
master = df %>% 
  filter(math == 1,
         Grade == 4) %>%
  mutate(year_sch = paste(DBN,Year,sep='_'))

master = master %>% 
  left_join(qgis_df, by=c('DBN','Year'))

# Standardize Scores for Math / All Grades
mean_scores_year = tapply(master$Mean.Scale.Score,master$Year,mean)
sd_year = tapply(master$Mean.Scale.Score,master$Year,sd)
years = names(mean_scores_year)

for(i in 1:length(years)){
  master$Mean.Scale.Score[master$Year==years[i]] = (master$Mean.Scale.Score[master$Year==years[i]] - mean_scores_year[i]) / sd_year[i]  
}

#########################
# Plots
#########################

# Plot Scores over time for 9 Schools
par(mfrow=c(4,4))
for(i in 1:16) {
  x = unique(master$DBN)[i]
  plot(master$Year[master$DBN == x],
       master$Mean.Scale.Score[master$DBN == x], type = 'l', col = master$charter[master$DBN == x], 
       main = x, ylab = 'std score', xlab = 'year')
}

# Plot Scores over time for 100 Schools (Colored by Charter)
par(mfrow=c(1,1))
x = unique(master$DBN)
x = x[sample(length(x))]
plot(master$Year[master$DBN == x[1]],
     master$Mean.Scale.Score[master$DBN == x[1]], type = 'l', 
     ylim=c(-2,2),
     ylab = 'std score', xlab = 'year')
for(i in 1:100){
  lines(master$Year[master$DBN == x[i]],
        master$Mean.Scale.Score[master$DBN == x[i]], type = 'l', col = mean(master$charter[master$DBN == x[i]])+1) 
}

# Plot Scores over time for 100 Schools (Colored by District)
par(mfrow=c(1,1))
x = unique(master$DBN)
x = x[sample(length(x))]
plot(master$Year[master$DBN == x[1]],
     master$Mean.Scale.Score[master$DBN == x[1]], type = 'l', 
     ylim=c(-2,2),
     ylab = 'std score', xlab = 'year')
for(i in 1:100){
  lines(master$Year[master$DBN == x[i]],
        master$Mean.Scale.Score[master$DBN == x[i]], type = 'l', col = master$GEOGRAPHICAL_DISTRICT_CODE[master$DBN == x[i]]) 
}


# Plot Scores over time for Districts
par(mfrow=c(1,1))
years = sort(unique(master$Year))
x = unique(master$GEOGRAPHICAL_DISTRICT_CODE)
plot(years,
     tapply(master$Mean.Scale.Score[master$GEOGRAPHICAL_DISTRICT_CODE == x[1]],
            master$Year[master$GEOGRAPHICAL_DISTRICT_CODE == x[1]], mean), 
     type = 'l', 
     ylim=c(-2,2),
     ylab = 'std score', xlab = 'year')
for(i in 1:length(x)){
  lines(years,
        tapply(master$Mean.Scale.Score[master$GEOGRAPHICAL_DISTRICT_CODE == x[i]],
               master$Year[master$GEOGRAPHICAL_DISTRICT_CODE == x[i]], mean),
        col = i) 
}


# Plot Scores over time, compare charter vs TPS
par(mfrow=c(2,3))
for(year in years){
  temp = master %>% filter(Year == as.integer(year))
  charter_indicator = temp$charter == 1
  plot(density(temp$Mean.Scale.Score[charter_indicator]), col = 2, 
       main = year, ylab = 'std score', xlab = 'year')
  lines(density(temp$Mean.Scale.Score[!charter_indicator]), col = 3)
}

 
# Plot Year/District Charter Count vs Score
par(mfrow=c(2,3))
for(year in years){
  temp = master %>%
    filter(Year==year) %>%
    group_by(GEOGRAPHICAL_DISTRICT_CODE) %>%
    dplyr::summarise(charter_count = sum(charter),
                     mean_score = mean(Mean.Scale.Score))
  plot(temp$charter_count,temp$mean_score,
       xlab = 'charter count in district in year',
       ylab = 'std score',
       main = year)
}


##############################
# Model
##############################

# Charter Count
temp = master %>%
  group_by(GEOGRAPHICAL_DISTRICT_CODE, Year) %>%
  dplyr::summarize(charter_count = sum(charter))

master2 = master %>% 
  filter(charter == 0) %>%
  left_join(temp, by=c('Year','GEOGRAPHICAL_DISTRICT_CODE'))

pooled.1 = lm(Mean.Scale.Score ~ charter_count, data = master2)
mm.mod1 = lmer(Mean.Scale.Score ~ (1 | GEOGRAPHICAL_DISTRICT_CODE), data = master2)
mm.mod2 = lmer(Mean.Scale.Score ~ (1 | DBN), data = master2)
mm.mod3 = lmer(Mean.Scale.Score ~ (1 | esid_no), data = master2)
mm.mod4 = lmer(Mean.Scale.Score ~ (1 | GEOGRAPHICAL_DISTRICT_CODE/DBN), data = master2)
mm.mod4 = lmer(Mean.Scale.Score ~ charter_count + (1 | GEOGRAPHICAL_DISTRICT_CODE), data = master2)
mm.mod5 = lmer(Mean.Scale.Score ~ charter_count + (1 | DBN), data = master2)
mm.mod6 = lmer(Mean.Scale.Score ~ charter_count + (1 | esid_no), data = master2)
mm.mod7 = lmer(Mean.Scale.Score ~ charter_count + (1 | GEOGRAPHICAL_DISTRICT_CODE/esid_no/DBN), data = master2)
mm.mod8 = lmer(Mean.Scale.Score ~ charter_count + Poverty + Disabled + Ell + Asian + Black + Hispanic + 
                 (1 | GEOGRAPHICAL_DISTRICT_CODE/esid_no/DBN), data = master2)


summary(pooled.1)
summary(mm.mod1)
summary(mm.mod2)
summary(mm.mod3)
summary(mm.mod4)
summary(mm.mod5)
summary(mm.mod6)
summary(mm.mod7)
summary(mm.mod8)

