install.packages("Lahman")
library("Lahman")
install.packages("ggplot2")
library(ggplot2)
inatall.packages("dplyr")
library(dplyr)
data(Teams)
data(Pitching)
data(Games)
data(Fielding)
data(HallOfFame)
data(Batting)
data(Teams)

playerInfo("ortizda01")

summary(Pitching)

#Add IP
Pitching <- Pitching %>%
  mutate(IP = IPouts/3)


# narrow by innings and AB
battingseason2014 = subset(Batting, yearID == 2014)
pitchingseason2014 = subset(Pitching, yearID == 2014)


kimbrcr01 = subset(Pitching, playerID == 'kimbrcr01')

attach(kimbrcr01)

# Calculate  Defense-Independent Component ERA (DICE)
DICE =  (3.00 +((13*HR)+(3*(BB+HBP))+-(2*SO))/IP)

kimbrcr01["DICE"] = DICE
RvDICE = lm(ERA~DICE)
#summary(RvRCB)
plot(DICE,ERA)
abline(RvDICE)
summary(RvDICE)$r.squared
cor(ERA,DICE)

detach(kimbrcr01)

fiftyAB = subset(battingseason2014, AB >= 50)
hundredIP = subset(pitchingseason2014,(IPouts/3)>=100)

BOS = subset(Batting, teamID == 'BOS')
BOS_2014 = subset(BOS, yearID == 2014)
bogaexa01 = subset(Batting, playerID == 'bogaexa01')

#Calculate SLG
bogaexa01$SLG = ((bogaexa01$H)+(2*bogaexa01$X2B)+(3*bogaexa01$X3B)+(4*bogaexa01$HR))/bogaexa01$AB
#Calculate OBP On-Base-Percentage
bogaexa01$OBP = (bogaexa01$HR + bogaexa01$BB + bogaexa01$HBP) / (bogaexa01$AB + bogaexa01$BB + bogaexa01$HBP + bogaexa01$SF)


bogaexa01$OPS = bogaexa01$OBP + bogaexa01$SLG



# plot K/9 vs. ERA as pitchers
hundredIP$k9 = hundredIP$SO / 9 
pPlot = ggplot(hundredIP,aes(k9,ERA))

# error - must add more layers
pPlot

# add points in red
pPlot = pPlot + geom_point(color = 'firebrick3')
pPlot

# add titles
pPlot = pPlot + ggtitle('ERA vs. K/9 for 201, min. 100 IP')
pPlot = pPlot + xlab('Strikeouts per 9')
pPlot = pPlot + ylab('Earned Run Average')
pPlot

# add regression line
pPlot = pPlot + stat_smooth(method = 'lm',formula = y~x,color = 'black',size = 1)
pPlot

#Calculate SLG
Batting$SLG = ((Batting$H)+(2*Batting$X2B)+(3*Batting$X3B)+(4*Batting$HR))/Batting$AB
#Calculate OBP On-Base-Percentage
Batting$OBP = (Batting$HR + Batting$BB + Batting$HBP) / (Batting$AB + Batting$BB + Batting$HBP + Batting$SF)


# ===== CREATE LEAGUE SUMMARY TABLES
# 
# select a sub-set of teams from 1901 [the establishment of the American
# League] forward to 2012
Teams_sub <- as.data.frame(subset(Teams, yearID > 1900))
# calculate each team's average runs and runs allowed per game
Teams_sub$RPG <- Teams_sub$R/Teams_sub$G
Teams_sub$RAPG <- Teams_sub$RA/Teams_sub$G
# create new data frame with season totals for each team
LG_RPG <- aggregate(cbind(R, RA, G) ~ yearID + teamID, data = Teams_sub, sum)
write.csv(LG_RPG, file = "MyData.csv")
# calculate league + season runs and runs allowed per game
LG_RPG$LG_RPG <- LG_RPG$R/LG_RPG$G
LG_RPG$LG_RAPG <- LG_RPG$RA/LG_RPG$G
# select a sub-set of teams from 1901 [the establishment of the American
# League] forward to 2012 read the data into separate league tables
Royals_season <- (subset(LG_RPG, yearID > 1900 & teamID == "KCA"))
Mets_season <- (subset(LG_RPG, yearID > 1900 & teamID == "NYN"))
#
# ===== TRENDS: RUNS SCORED PER GAME
# 
# AMERICAN LEAGUE create new object ALRunScore.LO for loess model
ALRunScore.LO <- loess(Royals_season$LG_RPG ~ Royals_season$yearID)
ALRunScore.LO.predict <- predict(ALRunScore.LO)

# MULTI-PLOT -- MERGING AL AND NL RESULTS plot individual years as lines
ylim <- c(3, 6)
# start with AL line
plot(Royals_season$LG_RPG ~ Royals_season$yearID, type = "l", lty = "solid", col = "red", 
     lwd = 2, main = "Runs per team per game, 1901-2012", ylim = ylim, xlab = "year", 
     ylab = "runs per game")
# add NL line
lines(Mets_season$yearID, Mets_season$LG_RPG, lty = "solid", col = "blue", lwd = 2)
# chart additions
grid()
legend(1900, 3.5, c("Royals", "Mets"), lty = c("solid", "solid"), col = c("red", "blue"), 
       lwd = c(2, 2))