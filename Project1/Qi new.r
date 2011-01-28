 #Load  data
bb = read.csv('bb-2009-all-correct.csv')
library(ggplot2)
dim(bb)
summary(bb)

# Data cleaning
dup.players = with(bb, ilkid %in% ilkid[duplicated(ilkid)])
bb[dup.players, c(1:3, 5, 25, 26)]
print(qplot(minutes, salary, data = bb, color = factor(dup.players), size=as.integer(dup.players)) + scale_colour_hue('duplicate')+ scale_size(to=c(1,2),legend = FALSE))

bb = bb[!dup.players, -c(4, 6, 7)]

## now work on bb

#Generate_Statistics
fgprct<-bb$fgm/bb$fga
ftprct<-bb$ftm/bb$fga
tpprct<-bb$tpm/bb$tpa

#Summary
summary(bb)


## get started with tons of plots and tables...
qplot(bb$salary,data=bb,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))

## Figure**. Histogram of salary of 401 players in 2009
## The distribution for salary is right skewed for all 401 observations. It seems that there are only a few players could get high salary especially more than seventeen million.
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
NBA_allfit <- lm(salary~gp+minutes+pts+oreb+dreb+reb+asts+stl+blk+turnover+pf+fga+fgm+fta+ftm+tpa+tpm,data=bb)
summary(NBA_allfit)

NBA_rebfit <- lm(salary~minutes,data=bb)
summary(NBA_rebfit)

NBA_tpmfit <- lm(salary~asts,data=bb)
summary(NBA_tpmfit)

NBA_6fit <- lm(salary~minutes+gp+oreb+dreb+asts+stl+turnover,data=bb)
summary(NBA_6fit)

# model searching
library(MASS)
rows <- sample(1:dim(bb)[1],300)
training <- bb[rows, ]
validate <- bb[-rows, ]

lm1 <- lm(salary ~ ., data = subset(bb, select = -c(lastname, firstname, ilkid, year, team, leag)))
slm1 <- step(lm1)