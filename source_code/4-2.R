a <- rbinom(10000, 5, 0.46)
table(a)/10000

curve(log(10*x^2*(1-x)^3),0,1,ylab='Log Probability', xlab='OBP')

OBP <- 0.4
base <- 0:5
P <- OBP^base*(1-OBP)^(5-base)
case<-choose(5, base)
EV <- P*case
EV

barplot(EV)

par(mfrow=c(2,2))
OBP <- 0.2
base <- 0:5
P <- OBP^base*(1-OBP)^(5-base)
case<-choose(5, base)
EV <- P*case
barplot(EV, main='OBP0.2 (20.48%)', ylab='possibility')

OBP <- 0.3
base <- 0:5
P <- OBP^base*(1-OBP)^(5-base)
case<-choose(5, base)
EV <- P*case
barplot(EV, main='OBP0.3 (30.87%)', ylab='possibility')

OBP <- 0.4
base <- 0:5
P <- OBP^base*(1-OBP)^(5-base)
case<-choose(5, base)
EV <- P*case
barplot(EV, main='OBP0.4 (34.56%)', ylab='possibility')

OBP <- 0.5
base <- 0:5
P <- OBP^base*(1-OBP)^(5-base)
case<-choose(5, base)
EV <- P*case
barplot(EV, main='OBP0.5 (31.25%)', ylab='possibility')