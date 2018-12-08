library(dplyr)
library(plyr)
library(lme4)
library(ggplot2)
setwd('/Users/Chansoo/Desktop/Charter_School_Project/data/')

#########################
# Plotting tool
#########################
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

#########################
# Load Data
#########################
rm(list=ls())
master = read.csv('master.csv')
master = master %>%
  filter(math == 1,
         grade < 6)
set.seed(1234)

#########################
# Plots
#########################
temp = master %>%
  filter(district != 0) %>%
  group_by(district, year, grade) %>%
  dplyr::summarise(y = mean(mean.scale.score))

ggplot(data = temp, aes(x=year, y = y, group = district, colour = as.factor(district)), alpha = 0.8) +
  geom_line() + 
  geom_point() +
  ggtitle("Scores over time by grade") +    
  labs(colour = '% charter \n in district') +
  facet_wrap(~grade)


temp = master%>%
  filter(district < 4)
ggplot(data = temp, aes(x=year, y = mean.scale.score), alpha = 0.8) +
  geom_point() +
  ggtitle("Scores over time by grade") +    
  labs(colour = '% charter \n in district') +
  facet_wrap(~grade)

