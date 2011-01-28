 #Load  data, the 'bb-2009-all-correct TOT calculated.csv' is the one no duplicats, just 408 observation with 2 players have two position in total
source('preprocess.R')

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
print(qplot(salary,data=bb))
#As expected, the salary histogram is severely skewed to the right. The range of salaries is very large, and often there are only a few key players to a team
qplot(minutes, salary, data = bb, geom = c('point', 'smooth'))
#Here we will start to see a trend that is common throughout this dataset, which is high variance.  The increase in salary as a function of the minutes played is true on average, but very uncertain for prediction.  It is important to note that the corelation is linear.
qplot(asts, salary, data = bb, geom = c('point', 'smooth'))
#This plot exposes a high density of players with fewer assists.  It appears that there is a strong positive correlation when assists are fewer than 200.  After this point the variance increases greatly and correlation is decreased to almost zero.  This makes intuitive sense, because the less dominate players will likely get paid more if they are good support for the higher paid players. Likewise, higher paid players generally should be scoring points and having fewer assists.  This is shown by the triangular shape of the scatterplot.
qplot(pf, salary, data = bb, geom = c('point', 'smooth'))
#Since some personal fouls can be strategic and others are mistakes of temperamental players, it is not surprising that this data has a large variace. Since some positions might have more strategic reason to foul, it may be interesting to observe this plot seperated by possition type.
qplot(reb, salary, data = bb, geom = c('point', 'smooth'))
#Once again, this statistic is very dependent on the position.  Although we see a possitive correlation here, we expect that the possition specific plots will be more clear.

## Plots by position
qplot(salary,data=bb,facets = position~.)
qplot(position, salary,data=bb,geom="boxplot")
qplot(minutes/gp, log(salary), data = bb,facets = position~.,geom=c('point','smooth'))
#accross all of the positions we see a general increasing increase in salary as minutes per game increases.  The most sensitive position is guard.  It is possible that the reason for this is that guards who can stay in a game longer are more valuable.  Endurance may not be as important for a player that moves less, like the center.
qplot(asts, salary, data = bb,facets = position~.)
#The first thing we see here is that salary is clearly possitively correlated to assists.  The second attention grabbing detail is the likelihood of guards to have more assists than the other two possitions.  Third, it appears that there are two groups of centers.  The reason for this split is not very clear. Finally, there exists one forward who is a clear outlier for the number of assists he makes.  If the value of a player is well represented by this correlation, perhaps scouts should seek out this player.
qplot(reb, salary, data = bb, facets = position~.)
#It is clear that forwards and centers have the highest frequency of players with lots of rebounds.  The correlation amongst all possitions is similar and not very strong.

## plots by team
qplot(minutes, salary, data = bb, geom = c('point', 'smooth'), facets=~team)
qplot(pts, salary, data = bb, geom = c('point', 'smooth'), facets=~team)
qplot(tpm/tpa, salary, data = bb, geom = c('point', 'smooth'), facets=~team)
qplot(reb, salary, data = bb, geom = c('point', 'smooth'), facets=~team)
#From these plots it is clear that there is a large variance in team strategy.  Some teams have many similar salaried players, while some have one to three players that have a much greater salary. 
#From the points plot for Washington, it is clear that they have one player that is significantly underpaid.  Perhaps a scout should keep an eye on this player.

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
