library(dplyr)
library(plyr)
library(lme4)
library(car)
library(ggplot2)
setwd('/Users/Chansoo/Desktop/Charter_School_Project/data/')

# Read Data
master = read.csv('master.csv')
master = master %>% mutate(ellpercent = ell/total.enrollment,
                           disabledpercent = disabled/total.enrollment)
missing_data = which(apply(is.na(master[,c('povertypercent','disabledpercent','ellpercent','asianpercent','blackpercent','hispanicpercent','district','dbn','mean.scale.score')]),1,sum)>0)
master = master[-missing_data,]

df = master %>% 
  filter(math == 1,
         grade < 6,
         charter == 0) 

df$charter_count = (df$charter_count - mean(df$charter_count))/sd(df$charter_count)

##############################
# Models
##############################

# Unconditional Means Model
##############################
um.mod = lmer(mean.scale.score ~ (1 | district/dbn/cohort), data = df)
um.mod.vars = as.data.frame(VarCorr(um.mod))
um.mod.vars$vcov[1] / sum(um.mod.vars$vcov) # ICC cohort
um.mod.vars$vcov[2] / sum(um.mod.vars$vcov) # ICC zone
um.mod.vars$vcov[3] / sum(um.mod.vars$vcov) # ICC district

# 20.6% of variation is between cohorts 
# 36.7% of variation is between zones
# 32.3% of variation is between districts

# Add Treatment Covariate + Random Slope
##############################
mod1 = lmer(mean.scale.score ~ charter_count + (1 | district/dbn/cohort), data = df)
mod2 = lmer(mean.scale.score ~ charter_count + (charter_count | district/dbn/cohort), data = df)
summary(mod1)
summary(mod2)
anova(mod1,mod2,refit=F)

# Add Demographics
##############################
mod3 = lmer(mean.scale.score ~ charter_count + povertypercent + disabledpercent + ellpercent + asianpercent + blackpercent + hispanicpercent +
                 (charter_count | district/dbn/cohort), data = df)
summary(mod3)
anova(mod1,mod2,mod3,refit=F)

# Add new_charts, cohort
##############################
mod4 = lmer(mean.scale.score ~ charter_count + povertypercent + disabledpercent + ellpercent + asianpercent + blackpercent + hispanicpercent + new_charts +
              (charter_count | district/dbn), data = df)
mod5 = lmer(mean.scale.score ~ charter_count + povertypercent + disabledpercent + ellpercent + asianpercent + blackpercent + hispanicpercent + new_charts + cohort +
              (charter_count | district/dbn), data = df)
anova(mod3,mod4,mod5,refit=F)

plot(density(residuals(mod5)))
qqnorm(residuals(mod5))
qqline(residuals(mod5))

# Add year
############################## 
mod6 = lmer(mean.scale.score ~ charter_count + povertypercent + disabledpercent + ellpercent + asianpercent + blackpercent + hispanicpercent + new_charts + cohort + year +
              (charter_count | district/dbn), data = df)

# Try nlme
library(nlme)
mod7 = lme(mean.scale.score ~ year, random=~year|district, data = df, correlation=corAR1())
mod8 = lme(mean.scale.score ~ charter_count + povertypercent + disabledpercent + ellpercent + asianpercent + blackpercent + hispanicpercent + new_charts + cohort + year, 
           random=~year+charter_count|district/dbn, 
           data = df, 
           correlation=corAR1())
summary(mod7)
summary(mod8)
anova(mod8)
# 
# mod7 = lmer(mean.scale.score ~ year + (year | district), data = df)
# mod8 = lmer(mean.scale.score ~ year + (year || district) + (0 + year | district:dbn), data = df)
# summary(mod6)
# summary(mod7)

save(mod1,mod2,mod3,mod4,mod5,mod6,mod7,file='models.RDATA')

#######
# Questions: 
# 1) missing data demographics
# 2) convergence issues when including random slope for 'year'