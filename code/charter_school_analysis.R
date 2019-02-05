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
df$year = df$year - mean(df$year) 
df$district = as.factor(df$district)
df$esid_no = as.factor(df$esid_no)


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

# 11.4% of variation is between cohorts 
# 41.8% of variation is between zones
# 36.5% of variation is between districts

# Add Treatment Covariate + Random Slope
##############################
mod1 = lmer(mean.scale.score ~ charter_count + (1 | district/dbn), data = df)
mod2 = lmer(mean.scale.score ~ charter_count + (1 | district/dbn) + (charter_count | dbn), data = df)
summary(mod1)
summary(mod2)
anova(mod1,mod2,refit=F)

# Add Demographics
##############################
mod3 = lmer(mean.scale.score ~ charter_count + povertypercent + disabledpercent + ellpercent + asianpercent + blackpercent + hispanicpercent +
              (1 | district/dbn), data = df)
summary(mod3)
anova(mod1,mod2,mod3,refit=F)

# Add new_charts, cohort
##############################
mod4 = lmer(mean.scale.score ~ charter_count + povertypercent + disabledpercent + ellpercent + asianpercent + blackpercent + hispanicpercent + new_charts +
              (1 | district/dbn), data = df)
mod5 = lmer(mean.scale.score ~ charter_count + povertypercent + disabledpercent + ellpercent + asianpercent + blackpercent + hispanicpercent + new_charts + cohort +
              (1 | district/dbn), data = df)
anova(mod3,mod4,mod5,refit=F)

plot(density(residuals(mod5)))
qqnorm(residuals(mod5))
qqline(residuals(mod5))

# Add year
############################## 
fixed = c('mean.scale.score ~  charter_count + new_charts + 
              povertypercent + disabledpercent + ellpercent + asianpercent + blackpercent + hispanicpercent + 
          cohort + year')

# Random intercepts for school and district
mod6 = lmer(formula(paste(fixed, ' + (1 | district/dbn)')), 
            data = df)

# + Random slopes for year:

# Varying at school level (uncorrelated)
mod7 = lmer(formula(paste(fixed, ' + (1 | district/dbn) + (0+year|district:dbn)')), 
            data = df) 

# Varying at district level  (uncorrelated)
mod8 = lmer(formula(paste(fixed, ' + (1 | district/dbn) + (0+year | district)')), 
            data = df) 

# Varying at both (uncorrelated)
mod9 = lmer(formula(paste(fixed, ' + (1 | district/dbn) + (0 + year | district) + (0 + year | dbn)')), 
            data = df) 

# Varying at both (correlated for school)
mod10 = lmer(formula(paste(fixed, ' + (1 | district) + (0 + year | district) + (year | district:dbn)')), 
             data = df) 

# Varying at both (correlated for district)
mod11 = lmer(formula(paste(fixed, ' + (1 | district) + (year | district) + (0 + year | district:dbn)')), 
             data = df) 

# Varying at both (correlated for both)
mod12 = lmer(formula(paste(fixed, ' + (1 | district) + (year | district) + (year | district:dbn)')), 
             data = df) 

anova(mod6,mod7,mod8,mod9,mod10,mod11,mod12,refit=F)
summary(mod7)




# save(mod1,mod2,mod3,mod4,mod5,mod6,mod7,file='models.RDATA')

