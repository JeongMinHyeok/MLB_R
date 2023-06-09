library(Lahman)
a<-subset(Batting, yearID>2010 & yearID<2017 & G>150)
b<-subset(People, sel=c('playerID', 'weight'))
c<-merge(a,b,by='playerID')
c$slg<-with(c,((H-X2B-X3B-HR)+2*X2B+3*X3B+4*HR)/AB)
with(c,plot(weight,slg,type='n'))
abline(lm(slg~weight, c))
fit<-lm(slg~weight, c)
fit_res<-resid(fit)
plot(c$weight, fit_res)
abline(0,0)
qqnorm(fit_res)
qqline(fit_res)