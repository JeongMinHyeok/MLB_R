# 전처리
library(Lahman)
a <- subset(Batting, yearID>2010&yearID<2016, select=c(playerID, teamID))
a$teamID <- factor(a$teamID)
a$teamID <- as.character(a$teamID)

# dcast()를 통해 동일 playerID를 갖는 소속팀 정보를 동일한 하나의 행으로 변경
library(data.table) # dcast()를 사용하기 위해서 패키지 필요
move <- dcast(setDT(a)[,idx := 1:.N, by = playerID],
              playerID~idx, value.var=c('teamID'))
move[is.na(move)]<-''
move[,1]<-NULL
write.csv(move,file='move.csv')

# read.transactions() : 저장된 파일을 R로 불러들일 때 사용
library(arules)
move <- read.transactions('move.csv', sep=',')

summary(move)

# 막대그래프 시각화
itemFrequencyPlot(move, support=0.01,cex.names=0.6)

# apriori로 선수들 팀 간 이동패턴 찾기
pattern <- apriori(move, list(support=0.0015, confidence=0.50, minlen=2))
summary(inspect(pattern))

# 전처리
# 35경기 이상 출장한 투수
library(stringr)
a <- subset(Pitching, yearID>2014&yearID<2017&G>35, select=c('playerID','yearID','teamID'))
a$yearID <- str_remove(a$yearID, '20')
a$teamyear <- paste(a$teamID, a$yearID, sep='') # paste()를 통해 두 글자를 합침
b <- subset(Managers, yearID>2014&yearID<2017, select=c('playerID', 'yearID', 'teamID'))
b$yearID <- str_remove(b$yearID, '20')
b$teamyear <- paste(b$teamID, b$yearID, sep='')
c <- merge(a,b,by='teamyear')
d <- subset(c, select=c('playerID.x', 'playerID.y')) # 같은 시기에 활동했던 투수와 감독을 짝지음
colnames(d) <- c('pitcher_ID', 'manager_ID')

# 네트워크 시각화
library(igraph)
mlb_network <- graph.data.frame(d, directed = FALSE)
V(mlb_network)$label <- ifelse(V(mlb_network)$name %in% c(b$playerID)>0,
                               as.character(b$teamyear), NA)

manager <- V(mlb_network)$name %in% c(b$playerID)+1
plot(mlb_network, vertex.shapes='none', vertex.label.cex=1.5, vertex.label.color='black',
     vertex.label.font=2, vertex.label.dist=1,
     vertex.size=c(3,0)[manager], vertex.color=c('gray', 'white')[manager])

# 막대그래프
library(Lahman)
a <- subset(Teams, yearID==2015)
b <- barplot(a$HR)

# 데릭 지터의 홈런 분포를 히스토그램으로 그려보기
a <- subset(Batting, playerID=='jeterde01')

# hist(분석 대상 변수 위치, x축 이름, 차트의 제목, 차트의 눈금 숫자에 대한 방향조절)
hist(a$HR, xlab='Homerun', main="Histrogram of Jeter's HR", las=1)
text(b,par('usr')[3], labels=a$teamID, srt=60, adj=c(1,0.5),xpd=TRUE)