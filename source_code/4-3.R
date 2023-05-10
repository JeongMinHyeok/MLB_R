library(sand)
library(igraph)
g<-graph.formula(1-5, 1-7, 2-9, 2-4, 3-5, 4-5, 4-6, 4-7)
V(g)
E(g)
plot(g)

a<-subset(Batting, yearID>2011&yearID<2016&AB>=300)
hist(a$H, main='949 players', breaks=seq(from=0, to=300, by=30))

a<-subset(Batting,yearID>2011&yearID<2016&AB>=300&teamID=='NYA')
hist(a$H, main='32 players', breaks=seq(from=0, to=300, by=30))

curve(dnorm(x),-4,4,ylab='density')
curve(dt(x,df=3),add=TRUE,lty=2)

par(mfrow=c(1,2))
x<-c(1,2,4,8,16)
y<-c(1,2,3,4,5)
plot(x,y,type='b', lwd=3, main='Before Transformation')
x_adj<-log(x)
plot(x_adj,y,type='b', lwd=3, main='After Transformation')

library(Lahman)
rec<-subset(Teams, yearID==2014)
rec$wp<-rec$W/rec$G
a<-lm(wp~R, rec)
library(lmtest)
bptest(a)

#box-cox 변형
library(caret)
b<-BoxCoxTrans(rec$wp)
c<-cbind(rec,wp_adj=predict(b, rec$wp))
d<-lm(wp_adj~R, c)
bptest(d)