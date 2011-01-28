 #Load  data, the 'bb-2009-all-correct TOT calculated.csv' is the one no duplicats, just 408 observation with 2 players have two position in total
bb<-read.csv('bb-2009-all-correct.csv')

#Generate_Statistics
fgprct<-bb$fgm/bb$fga
ftprct<-bb$ftm/bb$fga
tpprct<-bb$tpm/bb$tpa

#Summary
summary(bb)


library(ggplot2)
## get started with tons of plots and tables...
qplot(bb$salary,data=bb,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))
qplot(minutes, salary, data = bb, geom = c('point', 'smooth'))
qplot(asts, salary, data = bb, geom = c('point', 'smooth'))
qplot(pf, salary, data = bb, geom = c('point', 'smooth'))
qplot(reb, salary, data = bb, geom = c('point', 'smooth'))

## Plots by position
qplot(bb$salary,data=bb,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000),facets = position~.)
qplot(position, salary,data=bb,geom="boxplot")
qplot(minutes, salary, data = bb,facets = position~.)
qplot(asts, salary, data = bb,facets = position~.)
qplot(reb, salary, data = bb, facets = position~.)

## plots by team
qplot(minutes, salary, data = bb, geom = c('point', 'smooth'), facets=~team)
qplot(pts, salary, data = bb, geom = c('point', 'smooth'), facets=~team)
qplot(fgm/fga, salary, data = bb, geom = c('point', 'smooth'), facets=~team)
qplot(reb, salary, data = bb, geom = c('point', 'smooth'), facets=~team)



# Regreession
bb_allfit <- lm(salary~gp+minutes+pts+oreb+dreb+reb+asts+stl+blk+turnover+pf+fga+fgm+fta+ftm+tpa+tpm,data=bb)
summary(bb_allfit)

bb_rebfit <- lm(salary~minutes,data=bb)
summary(bb_rebfit)

bb_tpmfit <- lm(salary~asts,data=bb)
summary(bb_tpmfit)

bb_6fit <- lm(salary~minutes+gp+oreb+dreb+asts+stl+turnover,data=bb)
summary(bb_6fit)

# model searching
