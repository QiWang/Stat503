source('preprocess.R')

plotmatrix(bb[, -c(1:4)])
plotmatrix(bb[, 5:12])

bb$mpg = bb$minutes/bb$gp
print(qplot(mpg, log(salary), data = bb, geom = c('point', 'smooth'), facets=~team))

bb$fgp = bb$fgm/bb$fga  # filed goals success percentage
bb$ftp = bb$ftm/bb$fta  # free throws success percentage
bb$tpp = bb$tpm/bb$tpa  # 3 pointers success percentage
bb$rep = bb$reb/bb$gp   # rebounds per game
bb$asp = bb$asts/bb$gp  # assists per game
bb$stp = bb$stl/bb$gp   # steals per game
bb$blp = bb$blk/bb$gp   # blocks per game
bb$pfp = bb$pf/bb$gp    # fouls per game
bb$top = bb$turnover/bb$gp  # turnovers per game

fit = lm(salary ~ . - reb - tpm, data = subset(bb, select = team:salary))
fit.coef = coef(summary(fit))
fit.coef[fit.coef[, 4] < .1, ]

round(coef(summary(fit1<-step(fit, trace=0))), 2)
par(mfrow=c(1,2))
plot(fit1, which=1:2)

summary(fit2<-step(update(fit, log(salary)~.), trace=0))
par(mfrow=c(1,2))
plot(fit2, which=1:2)

anova(fit3<-lm(log(salary)~.,data=na.omit(subset(bb, select=c(salary, team, position, mpg:top)))))
summary(fit4<-step(fit3, trace=0))

par(mfrow=c(1,2))
plot(fit4, which=1:2)
