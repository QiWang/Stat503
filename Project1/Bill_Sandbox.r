#Load  LM
bb<-read.csv("bb-2009-all.csv")

#Generate_Statistics
fgprct<-bb$fgm/bb$fga
ftprct<-bb$ftm/bb$fga
tpprct<-bb$tpm/bb$tpa
minpergame<-bb$minutes/bb$gp
summary(lm(salary~minpergame,data=bb))
tpmpergame<-bb$tpm/bb$gp
summary(lm(salary~tpmpergame,data=bb))
fgmpergame<-bb$fgm/bb$gp
summary(lm(salary~fgmpergame,data=bb))

#Summary
Summary(bb)
summary(lm(salary~minutes+position+fgm+fga+ftm+fta+tpm+tpm+tpprct+fgprct+ftprct+minpergame+tpmpergame+fgmpergame,data=bb))

#Partition Minutes
library(ggplot2)
qplot(bb$minutes,data=bb,geom="histogram",xlab="Minutes",breaks=seq(0,3500,125))
qplot(bb$salary,data=bb,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))
summary(lm(salary~minutes,data=bb))

#Scatter note: non-functioning line
tmp<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*x))
tmp2<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*1))
qplot(jitter(minutes),salary,data=bb,geom="point",xlab="Min",ylab="Salary")+
  layer(data=tmp2,geom="line",mapping=aes(x=x,y=y),colour="grey60",size=1)+
  layer(data=tmp,geom="line",mapping=aes(x=x,y=y),colour="grey20",size=1)

#Remove instances with less than 750 minutes.
bb_MinGT750<-subset(bb, (minutes>=750))

summary(lm(salary~minutes,data=bb_MinGT750))
qplot(bb_MinGT750$minutes,data=bb_MinGT750,geom="histogram",xlab="Minutes",breaks=seq(0,3500,125))
qplot(bb_MinGT750$salary,data=bb_MinGT750,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))

#Scatter
tmp<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*x))
tmp2<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*1))
qplot(jitter(minutes),salary,data=bb_MinGT750,geom="point",xlab="Min",ylab="Salary")+
  layer(data=tmp2,geom="line",mapping=aes(x=x,y=y),colour="grey60",size=1)+
  layer(data=tmp,geom="line",mapping=aes(x=x,y=y),colour="grey20",size=1)

#General Summary greater than 750 minutes
fgprct2<-bb_MinGT750$fgm/bb_MinGT750$fga
ftprct2<-bb_MinGT750$ftm/bb_MinGT750$fga
tpprct2<-bb_MinGT750$tpm/bb_MinGT750$tpa
summary(bb_MinGT750)
summary(lm(salary~minutes+position+fgm+fga+ftm+fta+tpm+tpm+tpprct2+fgprct2+ftprct2,data=bb_MinGT750))

#General Summary less than 750 minutes
bb_MinLT750<-subset (bb, (minutes<750))
fgprct3<-bb_MinLT750$fgm/bb_MinLT750$fga
ftprct3<-bb_MinLT750$ftm/bb_MinLT750$fga
tpprct3<-bb_MinLT750$tpm/bb_MinLT750$tpa
summary(bb_MinLT750)
summary(lm(salary~minutes+position+fgm+fga+ftm+fta+tpm+tpm+tpprct3+fgprct3+ftprct3,data=bb_MinLT750))

#Salary VS Position
library(ggplot2)
qplot(bb$position,data=bb,geom="histogram",xlab="position")
qplot(bb$salary,data=bb,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))
summary(lm(salary~position,data=bb))

#Scatter note: non-functioning line
tmp<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*x))
tmp2<-data.frame(x=c(0,3500,250),y=c(250000,250000+2000*1))
qplot(jitter(position),salary,data=bb,geom="point",xlab="Min",ylab="Salary")+
  layer(data=tmp2,geom="line",mapping=aes(x=x,y=y),colour="grey60",size=1)+
  layer(data=tmp,geom="line",mapping=aes(x=x,y=y),colour="grey20",size=1)

#double instances with position="center".
bb_2Center<-read.csv(file.choose())
summary(lm(salary~position,data=bb_2Center))
qplot(bb_2Center$position,data=bb_2Center,geom="histogram",xlab="position",breaks=seq(0,3500,125))
qplot(bb_2Center$salary,data=bb_2Center,geom="histogram",xlab="salary",breaks=seq(0,23000000,500000))

#Scatter
tmp<-data.frame(x=c(250000),y=c("Center","Forward","Guard"))
tmp2<-data.frame(x=c(250000),y=c("Center","Forward","Guard"))
qplot(salary,position,data=bb_2Center,geom="point",xlab="Salary",ylab="position")+
  layer(data=tmp2,geom="line",mapping=aes(x=x,y=y),colour="grey60",size=1)+
  layer(data=tmp,geom="line",mapping=aes(x=x,y=y),colour="grey20",size=1)


