source('preprocess.R')

plotmatrix(bb[, -c(1:4)])
plotmatrix(bb[, 5:12])

bb$mpg = bb$minutes/bb$gp

fit = lm(salary ~ . - reb - tpm, data = subset(bb, select = team:salary))
fit.coef = coef(summary(fit))
fit.coef[fit.coef[, 4] < .1, ]

round(coef(summary(fit1<-step(fit, trace=0))), 2)
par(mfrow=c(1,2))
plot(fit1, which=1:2)

summary(fit2<-step(update(fit, log(salary)~.), trace=0))
par(mfrow=c(1,2))
plot(fit2, which=1:2)
