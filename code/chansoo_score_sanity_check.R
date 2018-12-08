i=4
g=4

###
df2 = nyc_open13[[i]] %>%
  filter(Grade == g,
         Year == 2013,
         Category == 'All Students')

x = as.numeric(df2$Mean.Scale.Score)
x = x[!is.na(x)]
x = (x - mean(x))/sd(x)

###


df2 = nyc_open06[[i]] %>%
  filter(Year == 2010,
         Grade == g,
         Demographic == 'All Students')

df2$x2 = as.numeric(df2$Mean.Scale.Score)

df2 = df2[!is.na(df2$x2),]

tapply(df2$x2,df2$Grade,mean)


x2 = x2[!is.na(x2)]
x2 = (x2 - mean(x2))/sd(x2)


plot(density(x))
lines(density(x2),col=i+1)

###
