setwd('/Users/Chansoo/Desktop/Charter_School_Project/')
# save(master,file='code/scores_sanity_check.RDATA')
load(file='code/scores_sanity_check.RDATA')

library(dplyr)

grade=5

df = master %>%
  filter(Grade == 5,
         Year == 2012)
  
df2 = master %>%
  filter(Grade == 5,
         Year == 2016)

par(mfrow=c(1,2))
hist(df$Mean.Scale.Score)
hist(df2$Mean.Scale.Score)

range(df$Mean.Scale.Score[df$Mean.Scale.Score %in% head(df$Mean.Scale.Score[order(df$Mean.Scale.Score)],20)])
range(df2$Mean.Scale.Score[df2$Mean.Scale.Score %in% head(df2$Mean.Scale.Score[order(df2$Mean.Scale.Score)],20)])

