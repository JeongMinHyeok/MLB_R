library(Lahman)
a<-subset(Batting,yearID==2014)
b<-subset(Batting,yearID==2015)
c<-merge(a,b,by='playerID')
d<-c[c$AB.x>10 & c$AB.y>10,]

# 상관관계 계산
with(d, cor(HR.x, HR.y))
with(d, cor(H.x/AB.x, H.y/AB.y))

# 전처리
library(Lahman)
library(dplyr)
library(plyr)
data <- subset(Batting,yearID>2014&yearID<2017) # 2015 ~ 2016년의 데이터
data$teamID <- as.character(data$teamID)
data$playerID <- as.character(data$playerID)
a <- arrange(data,playerID,yearID) # playerID와 yearID 기준으로 정렬

# 각 데이터가 동일선상에 위치하도록 작업(sapply)
a$p_teamID <- as.character(sapply(1:nrow(a), function(x){a$teamID[x-1]}))
a$p_playerID <- as.character(sapply(1:nrow(a), function(x){a$playerID[x-1]}))
a$p_RBI <- as.numeric(sapply(1:nrow(a), function(x){a$RBI[x-1]})) # 타점
a$p_AB <- as.numeric(sapply(1:nrow(a), function(x){a$AB[x-1]})) # 타수
a$p_SF <- as.numeric(sapply(1:nrow(a), function(x){a$SF[x-1]})) # 희생플라이
a$p_SH <- as.numeric(sapply(1:nrow(a), function(x){a$SH[x-1]})) # 희생번트
a$p_H <- as.numeric(sapply(1:nrow(a), function(x){a$H[x-1]})) # 안타
a$same_person <- ifelse(a$playerID == a$p_playerID, 'same', 'different')
b <- a[a$same_person == 'same',] # same표시가 있는 동일선수만 남도록
b$moved <- ifelse(b$teamID == b$p_teamID, 'no', 'yes')
c <- b[b$moved == 'yes',] # 이적선수만 남도록

c$p_avg <- with(c,p_H/p_AB) # 2015타율
c$sac <- with(c,p_SF+p_SH) # 2015 희생타
d <- subset(c, AB>400&p_AB>400) # 400타석 이상의 선수들만 포함
d$change_rbi <- with(d, RBI/p_RBI)

# 전년도 타율과 타점 변화 간 상관관계
with(d, cor(p_avg, change_rbi))

library(ggplot2)
ggplot(d, aes(p_avg, change_rbi, lgID))+geom_point(size=2, aes(shape=lgID))+
  annotate('text', x=0.3, y=1.6, label='r=-0.49', size=5)+
  stat_smooth(method='lm', col='black')+
  labs(x='Batting Avg of Prior Year', y='Change in RBI')

# 리그 별로 독립분석
ggplot(d, aes(p_avg, change_rbi))+geom_point(size=2)+
  stat_smooth(method='lm', col='black')+facet_wrap(~d$lgID)+
  labs(x='Batting Avg of Prior Year', y = 'Change in RBI')

# 희생번트와 희생플라이를 많이 친 선수들이 이적 첫 연도에 만들어낸 타점
with(d, cor(sac, change_rbi))

# 시각화
ggplot(d, aes(sac, change_rbi, lgID))+geom_point(size=2, aes(shape=lgID))+
  annotate('text', x=4, y=1.6, label='r=0.50', size=5)+
  stat_smooth(method='lm', col='black')+
  labs(x='Sacrifice Flies & Hits', y='Change in RBI')

# 상관관계표
install.packages('tableHTML')
library(tableHTML)
e <- with(d, data.frame(change_rbi, sac, p_avg))
colnames(e) <- c('C_RBI', 'Sac', 'AVG')
cor(e)