## Set up
library(tidyverse)
library(zoo)
library(lubridate)


## Read the data
data = read.table("data/ISDBv2.0.txt",sep="\t",header=TRUE)


## Feature extraction

ger1 = data %>% filter(Lge=="GER1") %>% select(-Lge) %>% 
  mutate(
    Sea = as.factor(Sea),
    Date = as.Date(Date,"%d/%m/%Y"), 
    Quarter = quarter(Date)
    )

seanames = levels(ger1$Sea)  # save season names

dt = data.frame(
  Sea = as.integer(c(ger1$Sea, ger1$Sea)), # change season names to 1,2,...,17,
                                           # so that it is easier to merge the previous season/round data
  t = c(ger1$Date, ger1$Date),
  Quarter = c(ger1$Quarter, ger1$Quarter),
  attack = as.factor(c(ger1$HT, ger1$AT)),
  defense = as.factor(c(ger1$AT, ger1$HT)),
  Home = c(rep(1,nrow(ger1)),rep(0,nrow(ger1))),
  Goals = c(ger1$HS, ger1$AS),
  GoalsConceded = c(ger1$AS, ger1$HS),
  GoalDf = c(ger1$GD, -ger1$GD)
  ) %>% 
  mutate(
    Points = 3*(GoalDf > 0) + 1*(GoalDf == 0)
  ) %>% arrange(t) %>% 
  group_by(Sea,attack) %>%
  mutate(
    DaysSincePrevMa = t - lag(t),
    MatchesPlayed = cumsum(attack==attack)-1,
    Round = MatchesPlayed + 1,
    PointsTally = cumsum(Points)-Points,
    Form = rollsumr(Points,k=3,fill=NA)/9,
    GoalDfTally = cumsum(GoalDf)-GoalDf
  ) %>% ungroup()


## Data exploration

'
ger1 %>% group_by(Sea) %>% count()
# From 00-01 to 16-17, 17 seasons in total. 306 game per season.

# Get the data of Season 00-01. Run from Aug to May
ger1_0001 = ger1 %>% filter(Sea=="00-01")
# Get the 18 teams 
sort(unique(ger1_0001$HT))

# 9 Games per round, 34 rounds in total.

# Check Season window: mostly Aug-May; except for 01-02, it is Jul-May
View(dt %>% mutate(
  month = lubridate::month(t)
) %>% 
  group_by(Sea) %>% select(Sea,month) %>% distinct() %>% pivot_wider(names_from = Sea, values_from = month, values_fn = list))
'

## 2: Newly promoted


teamlist = dt %>% distinct(Sea, attack) %>% group_by(Sea) %>% summarise(Sea,Teams = list(attack),.groups="drop") %>%
  distinct()

allteams = unique(ger1$HT)

newly = data.frame( t( apply( teamlist[2], 1, function(x) as.numeric( allteams %in% x[[1]]) ) ) ) %>% 
  rollapplyr(2,function(x) as.numeric(x[1]==0 & x[2]==1),fill=NA)
colnames(newly) = allteams
newly = cbind(teamlist["Sea"],newly)
newly = newly %>% pivot_longer(-Sea,names_to="attack",values_to="NewlyPromoted")

dt = left_join(dt,newly,by = c("Sea","attack"))



## 11,12: Previous season/round points tally/goal difference

levels(dt$Sea) = c(1:17)

seasonsummary = dt %>% group_by(Sea,attack) %>%  summarise(
  PrevSeasonPoints = sum(Points),
  PrevSeasonGD = sum(GoalDf)
  ) %>% arrange(Sea,desc(PrevSeasonPoints)) %>% 
  mutate(
    Sea = Sea + 1,
    PrevSeasonRankings = order(PrevSeasonPoints, decreasing = T)
  ) %>% ungroup() %>%  filter(Sea < 18)

roundsummary = dt %>% group_by(Sea,Round,attack) %>% summarise(
  PrevRoundPoints = sum(Points),
  PrevRoundGD = sum(GoalDf),
  .groups = "drop"
  ) %>% 
  group_by(Sea,attack) %>% 
  mutate(
    Round = Round + 1,
    PrevRoundPointsTally = cumsum(PrevRoundPoints),
    PrevRoundGDT = cumsum(PrevRoundGD)
  ) %>% ungroup() %>% 
  arrange(Sea,Round,desc(PrevRoundPointsTally)) %>%
  group_by(Sea,Round) %>% 
  mutate(
    PrevRoundRankings = order(PrevRoundPointsTally, decreasing = T)
  ) %>% ungroup() %>% 
  filter(Round < 35)

dt = dt %>% left_join(seasonsummary,by=c("Sea","attack")) %>%
  left_join(roundsummary,by=c("Sea","Round","attack")) %>% 
  mutate(
    Sea = ordered(Sea)
  )

levels(dt$Sea) = seanames # change the season names back


## Check data for one team

View(dt %>% filter(attack=="Dortmund"))




