 #Load  data, the 'bb-2009-all-correct TOT calculated.csv' is the one no duplicats, just 408 observation with 2 players have two position in total
NBA<-read.csv('bb-2009-all-correct.csv')

#Generate_Statistics
fgprct<-NBA$fgm/NBA$fga
ftprct<-NBA$ftm/NBA$fga
tpprct<-NBA$tpm/NBA$tpa

#Summary
summary(NBA)


library(ggplot2)
## get started with tons of plots and tables...
qplot(NBA$salary,data=NBA,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))
qplot(minutes, salary, data = NBA, geom = c('point', 'smooth'))
qplot(asts, salary, data = NBA, geom = c('point', 'smooth'))
qplot(pf, salary, data = NBA, geom = c('point', 'smooth'))
qplot(reb, salary, data = NBA, geom = c('point', 'smooth'))

## Plots by position
qplot(NBA$salary,data=NBA,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000),facets = position~.)
qplot(position, salary,data=NBA,geom="boxplot")
qplot(minutes, salary, data = NBA,facets = position~.)
qplot(asts, salary, data = NBA,facets = position~.)
qplot(reb, salary, data = NBA, facets = position~.)

## plots by team
qplot(minutes, salary, data = NBA, geom = c('point', 'smooth'), facets=~team)
qplot(pts, salary, data = NBA, geom = c('point', 'smooth'), facets=~team)
qplot(fgm/fga, salary, data = NBA, geom = c('point', 'smooth'), facets=~team)
qplot(reb, salary, data = NBA, geom = c('point', 'smooth'), facets=~team)



# Regreession
NBA_allfit <- lm(salary~gp+minutes+pts+oreb+dreb+reb+asts+stl+blk+turnover+pf+fga+fgm+fta+ftm+tpa+tpm,data=NBA)
summary(NBA_allfit)

NBA_rebfit <- lm(salary~minutes,data=NBA)
summary(NBA_rebfit)

NBA_tpmfit <- lm(salary~asts,data=NBA)
summary(NBA_tpmfit)

NBA_6fit <- lm(salary~minutes+gp+oreb+dreb+asts+stl+turnover,data=NBA)
summary(NBA_6fit)

# model searching
