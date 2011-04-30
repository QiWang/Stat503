###################################################
### chunk number 1: setup
###################################################
#line 40 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

options(width=85,useFancyQuotes=FALSE)

library(ggplot2)

theme_set(theme_grey(base_size = 9))



###################################################
### chunk number 2: duplicate-records
###################################################
#line 107 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

bb = read.csv('bb-2009-all-correct.csv')

dup.players = with(bb, ilkid %in% ilkid[duplicated(ilkid)])

bb[dup.players, c(1:3, 5, 25, 26)]



###################################################
### chunk number 3: duplicate-plot
###################################################
#line 137 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

print(qplot(minutes, salary, data = bb, color = factor(dup.players), size=as.integer(dup.players)) + scale_colour_hue('duplicate')+ scale_size(to=c(1,2),legend = FALSE))



###################################################
### chunk number 4: new-data
###################################################
#line 152 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

bb = bb[!dup.players, -c(4, 6, 7)]

dim(bb)



###################################################
### chunk number 5: hist-salary
###################################################
#line 166 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

print(qplot(salary,data=bb))



###################################################
### chunk number 6: salary-minutes
###################################################
#line 188 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

print(qplot(minutes, salary, data = bb, geom = c('point', 'smooth')))



###################################################
### chunk number 7: other-salary-plots eval=FALSE
###################################################
## #line 216 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"
## 
## qplot(asts, salary, data = bb, geom = c('point', 'smooth')) 
## 
## qplot(pf, salary, data = bb, geom = c('point', 'smooth')) 
## 
## qplot(reb, salary, data = bb, geom = c('point', 'smooth')) 
## 


###################################################
### chunk number 8: scatterplot-matrix eval=FALSE
###################################################
## #line 273 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"
## 
## ## in case the reader be curious about the scatter plot matrix
## 
## plotmatrix(bb[, 5:12])
## 


###################################################
### chunk number 9: salary-position
###################################################
#line 287 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

print(qplot(position, salary,data=bb,geom="boxplot"))



###################################################
### chunk number 10: salary-minpergp
###################################################
#line 302 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

print(qplot(minutes/gp, log(salary), data = bb,facets = position~.,geom=c('point','smooth')))



###################################################
### chunk number 11: mpg-creation
###################################################
#line 339 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

bb$mpg = bb$minutes/bb$gp   # new variable 'mpg'



###################################################
### chunk number 12: salary-team
###################################################
#line 348 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

print(qplot(mpg, log(salary), data = bb, geom = c('point', 'smooth'), facets=~team))



###################################################
### chunk number 13: salary-points eval=FALSE
###################################################
## #line 389 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"
## 
## qplot(pts/gp,log(salary),data=bb,facets=~team,geom=c('point','smooth'))
## 


###################################################
### chunk number 14: performance-pg
###################################################
#line 408 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

bb$fgp = bb$fgm/bb$fga  # filed goals success percentage

bb$ftp = bb$ftm/bb$fta  # free throws success percentage

bb$tpp = bb$tpm/bb$tpa  # 3 pointers success percentage

bb$rep = bb$reb/bb$gp   # rebounds per game

bb$asp = bb$asts/bb$gp  # assists per game

bb$stp = bb$stl/bb$gp   # steals per game

bb$blp = bb$blk/bb$gp   # blocks per game

bb$pfp = bb$pf/bb$gp    # fouls per game

bb$top = bb$turnover/bb$gp  # turnovers per game



###################################################
### chunk number 15: lm1
###################################################
#line 454 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

fit = lm(salary ~ . - reb - tpm, data = subset(bb, select = team:salary))

fit.coef = coef(summary(fit))

round(fit.coef[fit.coef[, 4] < .1, ], 2)  # vars with P-value < 0.1



###################################################
### chunk number 16: lm1-aic
###################################################
#line 472 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

round(coef(summary(fit1<-step(fit, trace=0))), 2)



###################################################
### chunk number 17: lm1-log
###################################################
#line 498 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

round(coef(summary(fit2<-step(update(fit, log(salary)~.), trace=0)) ), 4)



###################################################
### chunk number 18: residual-plot
###################################################
#line 507 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

par(mfrow=c(2,2),mar=c(4,4,1,.5),mgp=c(2,.9,0))

plot(fit1, which=1:2)

plot(fit2, which=1:2) 



###################################################
### chunk number 19: new-var-regression
###################################################
#line 541 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

## ANOVA on all the explanatory variables: most are significant

anova(fit3<-lm(log(salary)~.,data=na.omit(subset(bb, select=c(salary, team, position, mpg:top)))))

## a stepwise regression base on AIC

summary(fit4<-step(fit3, trace=0))



###################################################
### chunk number 20: residual-new
###################################################
#line 563 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

par(mfrow=c(1,2),mar=c(4,4,1,.5),mgp=c(2,.9,0))

plot(fit4, which=1:2)



###################################################
### chunk number 21: sessionInfo
###################################################
#line 644 "Stat503-Project1-Wang-Zhao-Rowcliffe-Xie.Rnw"

sessionInfo()



