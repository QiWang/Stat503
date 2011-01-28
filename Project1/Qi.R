 #Load  data, the 'bb-2009-all-correct TOT calculated.csv' is the one no duplicats, just 408 observation with 2 players have two position in total
bb<-read.csv('bb-2009-all-correct.csv')

#Generate_Statistics
fgprct<-bb$fgm/bb$fga
ftprct<-bb$ftm/bb$fga
tpprct<-bb$tpm/bb$tpa
minpergame<-bb$minutes/bb$gp

tpmpergame<-bb$tpm/bb$gp

fgmpergame<-bb$fgm/bb$gp


#Summary
summary(bb)


library(ggplot2)
## Initial Exploration Plots
qplot(bb$salary,data=bb,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))
#As expected, the salary histogram is severely skewed to the right. The range of salaries is very large, and often there are only a few key players to a team
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



# Regression

summary(lm(salary~minutes+position+fgm+fga+ftm+fta+tpm+tpm+tpprct+fgprct+ftprct+minpergame+tpmpergame+fgmpergame,data=bb))

summary(lm(salary~gp + minutes + position + pts + oreb + dreb + reb + 
    asts + stl + blk + turnover + pf + fga + fgm + fta + ftm + 
    tpa + tpm + minpergame + tpmpergame + fgmpergame, data = bb))

bb_rebfit <- lm(salary~minutes,data=bb)
summary(bb_rebfit)

bb_tpmfit <- lm(salary~asts,data=bb)
summary(bb_tpmfit)

summary(lm(salary~position,data=bb))

summary(lm(salary~dreb,data=bb))

summary(lm(salary~dreb,data=bb))

summary(lm(salary~fta,data=bb))

summary(lm(salary~minpergame,data=bb))

summary(lm(salary~fgmpergame,data=bb))

summary(lm(salary~tpmpergame,data=bb))

bb_6fit <- lm(salary~minpergame+gp+oreb+dreb+asts+stl+turnover,data=bb)
summary(bb_6fit)

# model searching
