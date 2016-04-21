
players <- read.csv("NBA-Census-10.14.2013.csv", stringsAsFactors=FALSE)

summary(players)
str(players)
warriors <- subset(players, Team=="Warriors")
warriors.o <- warriors[order(warriors$Ht..In..),]
par(mar=c(5,10,5,5))
barplot(warriors.o$Ht..In.., names.arg=warriors.o$Name, horiz=TRUE, border=NA, las=1, main="Heights of Golden State Warriors")

avgHeights <- aggregate(Ht..In.. ~ POS, data=players, mean)
avgHeights.o <- avgHeights[order(avgHeights$Ht..In.., decreasing=FALSE),]
barplot(avgHeights.o$Ht..In.., names.arg=avgHeights.o$POS, border=NA, las=1)

htrange <- range(players$Ht..In..)  # 69 to 87 inches
cnts <- rep(0, 20)
y <- c()
for (i in 1:length(players[,1])) {
  
  cntIndex <- players$Ht..In..[i] - htrange[1] + 1
  cnts[cntIndex] <- cnts[cntIndex] + 1
  y <- c(y, cnts[cntIndex])
  
}
plot(players$Ht..In.., y, type="n", main="Player heights", xlab="inches", ylab="count")
points(players$Ht..In.., y, pch=21, col=NA, bg="#999999")


barplot(cnts, names.arg=69:88, main="Player heights", xlab="inches", ylab="count", border=NA, las=1)

par(mfrow=c(1,3), mar=c(3,3,3,3))
hist(players$Ht..In.., main="NBA Player Heights", xlab="inches", breaks=seq(65, 90, 1))
hist(players$Ht..In.., main="NBA Player Heights", xlab="inches", breaks=seq(65, 90, 2))
hist(players$Ht..In.., main="NBA Player Heights", xlab="inches", breaks=seq(65, 90, 5))

par(mfrow=c(1,3), mar=c(3,3,3,3))
hist(players$Ht..In.., main="NBA Player Heights", xlab="inches", breaks=c(seq(65, 75, 2), 80, 90))
hist(players$Ht..In.., main="NBA Player Heights", xlab="inches", breaks=c(65, 75, seq(80, 90, 2)))
hist(players$Ht..In.., main="NBA Player Heights", xlab="inches", breaks=c(65, seq(70, 80, 1), 90))

par(mfrow=c(2,3), las=1, mar=c(5,5,4,1))
positions <- unique(players$POS)
for (i in 1:length(positions)) {
  currPlayers <- subset(players, POS==positions[i])
  hist(currPlayers$Ht..In.., main=positions[i], breaks=65:90, xlab="inches", border="#ffffff", col="#999999", lwd=0.4)
}

par(mfrow=c(5,1), las=1, mar=c(5,5,4,1), xaxs="i", yaxs="i")
for (i in 1:length(avgHeights.o$POS)) {
  currPlayers <- subset(players, POS==avgHeights.o$POS[i])
  htMedian <- median(currPlayers$Ht..In..)
  h <- hist(currPlayers$Ht..In.., main=avgHeights.o$POS[i], breaks=65:90, xlab="inches", border=NA, col="#999999", lwd=0.4)
  maxFreq <- max(h$counts)
  segments(h$breaks, rep(0, length(h$breaks)), h$breaks, maxFreq, col="white")
  
  # Median line
  lines(c(htMedian, htMedian), c(-1, maxFreq), col="purple", lwd=2)
}

install.packages("rjson")
library(rjson)
install.packages("ggplot2")
library(ggplot2)


# shot data for Stephen Curry
playerID <- 201939
shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2014-15&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2014-15&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")

# import from JSON
shotData <- fromJSON(file = shotURL, method="C")

# unlist shot data, save into a data frame
shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=21, byrow = TRUE))

# shot data headers
colnames(shotDataf) <- shotData$resultSets[[1]][[2]]

# covert x and y coordinates into numeric
shotDataf$LOC_X <- as.numeric(as.character(shotDataf$LOC_X))
shotDataf$LOC_Y <- as.numeric(as.character(shotDataf$LOC_Y))
shotDataf$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))

# have a look at the data
View(shotDataf)

# simple plot using EVENT_TYPE to colour the dots
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
  geom_point(aes(colour = EVENT_TYPE))

library(grid)
install.packages("jpeg")
library(jpeg)
library(RCurl)


# plot using court shaped chart
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) + 
  geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) +
  xlim(-250, 250) +
  ylim(-50, 420)

# plot using ggplot 
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
  geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) +
  xlim(250, -250) +
  ylim(-50, 420) +
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", unique(shotDataf$PLAYER_NAME), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, lineheight = 0.9, face = "bold"))
