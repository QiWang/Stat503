#Load  LM
BB<-read.csv("bb-2009-all.csv")

#Generate_Statistics
fgprct<-BB$fgm/BB$fga
ftprct<-BB$ftm/BB$fga
tpprct<-BB$tpm/BB$tpa
minpergame<-BB$minutes/BB$gp

#Summary
Summary(BB)
summary(lm(salary~minutes+position+fgm+fga+ftm+fta+tpm+tpm+tpprct+fgprct+ftprct+minpergame,data=BB))

#Partition Minutes
library(ggplot2)
qplot(BB$minutes,data=BB,geom="histogram",xlab="Minutes",breaks=seq(0,3500,125))
qplot(BB$salary,data=BB,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))
summary(lm(salary~minutes,data=BB))

#Scatter note: non-functioning line
tmp<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*x))
tmp2<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*1))
qplot(jitter(minutes),salary,data=BB,geom="point",xlab="Min",ylab="Salary")+
  layer(data=tmp2,geom="line",mapping=aes(x=x,y=y),colour="grey60",size=1)+
  layer(data=tmp,geom="line",mapping=aes(x=x,y=y),colour="grey20",size=1)

#Remove instances with less than 750 minutes.
BB_MinGT750<-subset(BB, (minutes>=750))

summary(lm(salary~minutes,data=BB_MinGT750))
qplot(BB_MinGT750$minutes,data=BB_MinGT750,geom="histogram",xlab="Minutes",breaks=seq(0,3500,125))
qplot(BB_MinGT750$salary,data=BB_MinGT750,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))

#Scatter
tmp<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*x))
tmp2<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*1))
qplot(jitter(minutes),salary,data=BB_MinGT750,geom="point",xlab="Min",ylab="Salary")+
  layer(data=tmp2,geom="line",mapping=aes(x=x,y=y),colour="grey60",size=1)+
  layer(data=tmp,geom="line",mapping=aes(x=x,y=y),colour="grey20",size=1)

#General Summary greater than 750 minutes
fgprct2<-BB_MinGT750$fgm/BB_MinGT750$fga
ftprct2<-BB_MinGT750$ftm/BB_MinGT750$fga
tpprct2<-BB_MinGT750$tpm/BB_MinGT750$tpa
summary(BB_MinGT750)
summary(lm(salary~minutes+position+fgm+fga+ftm+fta+tpm+tpm+tpprct2+fgprct2+ftprct2,data=BB_MinGT750))

#General Summary less than 750 minutes
BB_MinLT750<-subset (BB, (minutes<750))
fgprct3<-BB_MinLT750$fgm/BB_MinLT750$fga
ftprct3<-BB_MinLT750$ftm/BB_MinLT750$fga
tpprct3<-BB_MinLT750$tpm/BB_MinLT750$tpa
summary(BB_MinLT750)
summary(lm(salary~minutes+position+fgm+fga+ftm+fta+tpm+tpm+tpprct3+fgprct3+ftprct3,data=BB_MinLT750))

#Salary VS Position
library(ggplot2)
qplot(BB$position,data=BB,geom="histogram",xlab="position")
qplot(BB$salary,data=BB,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))
summary(lm(salary~position,data=BB))

#Scatter note: non-functioning line
tmp<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*x))
tmp2<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*1))
qplot(jitter(position),salary,data=BB,geom="point",xlab="Min",ylab="Salary")+
  layer(data=tmp2,geom="line",mapping=aes(x=x,y=y),colour="grey60",size=1)+
  layer(data=tmp,geom="line",mapping=aes(x=x,y=y),colour="grey20",size=1)

#double instances with position="center".
BB_2Center<-read.csv(file.choose())
summary(lm(salary~position,data=BB_2Center))
qplot(BB_2Center$position,data=BB_2Center,geom="histogram",xlab="position",breaks=seq(0,3500,125))
qplot(BB_2Center$salary,data=BB_2Center,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))

#Scatter
tmp<-data.frame(x=c(250000),y=c("Center","Forward","Guard"))
tmp2<-data.frame(x=c(250000),y=c("Center","Forward","Guard"))
qplot(salary,position,data=BB_2Center,geom="point",xlab="Salary",ylab="position")+
  layer(data=tmp2,geom="line",mapping=aes(x=x,y=y),colour="grey60",size=1)+
  layer(data=tmp,geom="line",mapping=aes(x=x,y=y),colour="grey20",size=1)


