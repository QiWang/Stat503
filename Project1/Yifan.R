##Loading file

bb <- read.csv('bb-2009-all-correct.csv', stringsAsFactors = FALSE)


##Package

install.packages("ggplot2")
library(ggplot2)


##Add new variables

bb$fgp <- bb$fgm/bb$fga
bb$ftp <- bb$ftm/bb$fta
bb$tpp <- bb$tpm/bb$tpa


##Plot, Minutes

qplot(minutes,salary,data=bb)

#From this plot we can find that as minutes go up, players' salary is also moving up.
#But it looks not that much linerly correlated.
#The first reason can be: "different teams have different levels of salary".
#We decide to add "team" as a factor in the plot.

qplot(minutes,salary,data=bb,facets=~team)

#Adding team to the plot makes the plot better, but we can still see some points in the middle of x-axe with a high y-axe position which reflecting that there are some players playing less in total minutes in the season with a higher salary.
#We guess the reason can be "some players can not finish the whole season's games because of some trouble".
#We decide to use "minutes/game" instead of "minutes" so that this problem might be ignored.

qplot(minutes/gp,log(salary),data=bb,facets=~team,geom=c('point','smooth'))

#Finally the plot is much better.
#Overall we can find that as minutes/game increases, the players salaty is increasing.
#There are some players still playing less with higher salary and some playing more with low salary.


##Plot, Points

qplot(pts/gp,log(salary),data=bb,facets=~team,geom=c('point','smooth'))

#Following the experience, we make the plot of points/game vs salary by teams.
#From the plot, we can see that as the points/game goes up, the players salary in increasing, especially for the top attackers in each team, their salary will be the top of the team.


##Plot, fgp ftp tpp

qplot(fgp,log(salary),data=bb,facets=~team,geom=c('point','smooth'),xlim=c(.3,.7),ylim=c(10,18))
qplot(ftp,log(salary),data=bb,facets=~team,geom=c('point','smooth'))
qplot(tpp,log(salary),data=bb,facets=~team,geom=c('point','smooth'))

#By looking at the these plots, we can find that most of the players have the similar percentage of shooting. So, it is really haed to say if there is any certain linear correlations from the plots.


##Plot, reb

qplot(reb/gp,salary,data=bb,facets=~team)

#Thinking about the defence, we choose reb as the variable to see if they are correalted. The plot looks good.
#Although we can see the correaltion that much clear as we have seen from points and minutes, it still makes sense. For most of the teams, the players with top reb/game will get more money comparing with the most of the others.
