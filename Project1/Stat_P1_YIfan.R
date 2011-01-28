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

qplot(minutes/gp,salary,data=bb,facets=~team)

#Finally the plot is much better.
#Overall we can find that as minutes/game increases, the players salaty is increasing.
#There are some players still playing less with higher salary and some playing more with low salary.


##Plot, Points

qplot(pts/gp,salary,data=bb,facets=~team)

#Following the experience, we make the plot of points/game vs salary by teams.
#From the plot, we can see that as the points/game goes up, the players salary in increasing, especially for the top attackers in each team, their salary will be the top of the team.


##Plot, fgp ftp tpp

qplot(fgp,salary,data=bb,facets=~team)
qplot(ftp,salary,data=bb,facets=~team)
qplot(tpp,salary,data=bb,facets=~team)

#By looking at the these plots, we can find that most of the players have the similar percentage of shooting. So, it is really haed to say if there is any certain linear correlations from the plots.


##Plot, reb

qplot(reb/gp,salary,data=bb,facets=~team)

#Thinking about the defence, we choose reb as the variable to see if they are correalted. The plot looks good.
#Although we can see the correaltion that much clear as we have seen from points and minutes, it still makes sense. For most of the teams, the players with top reb/game will get more money comparing with the most of the others.


##Researching on Years of Played

cc <- read.csv("players.csv")
head(cc)
dd <- cc[cc$lastseason==2009,]
dd$firstseason <- as.numeric(dd$firstseason)
dd$Year <- 2010 - dd$firstseason
ee <- dd[,c(2,3,12)]
ee$name <- paste(ee$firstname,ee$lastname)
bb$name <- paste(bb$firstname,bb$lastname)
bb.y <- merge(bb,ee,by="name",all=FALSE)
summary(bb.y)

#To explore the data in detail, we decide to add "Years of PLayed" into the dataset.
#We get another data from databasebasketball, which contains each player's Firstseason and Lastseason. So, it's easy to calculate the Years of Played by calculating the difference between the two variables.
#Then we add Years to the regular dataset.

qplot(Year,salary,data=bb.y)

#From this plot we can find that as the Years of PLayers increases, their top salary is also increasing. For a top player, their salary is strongly correlated to their age of NBA.
