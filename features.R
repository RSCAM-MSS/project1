## Set up
library(tidyverse)
library(zoo)
library(lubridate)


## Read the data
data = read.table("data/ISDBv2.0.txt",sep="\t",header=TRUE)


## Feature extraction

ger1 = data %>% filter(Lge=="GER1") %>% select(-Lge) %>% mutate(Date = as.Date(Date,"%d/%m/%Y"), Quarter = quarter(Date))

dt = data.frame(
  Sea = c(ger1$Sea, ger1$Sea),
  t = c(ger1$Date, ger1$Date),
  Quarter = c(ger1$Quarter, ger1$Quarter),
  attack = as.factor(c(ger1$HT, ger1$AT)),
  defense = as.factor(c(ger1$AT, ger1$HT)),
  Home = c(rep(1,nrow(ger1)),rep(0,nrow(ger1))),
  Goals = c(ger1$HS, ger1$AS),
  GoalsConceded = c(ger1$AS, ger1$HS),
  GoalDf = c(ger1$GD, -ger1$GD),
  Points = 3*(GoalDf > 0) + 1*(GoalDf == 0)
  ) %>% arrange(t) %>% 
  group_by(Sea,attack) %>%
  mutate(
    DaysSincePrevMa = t - lag(t),
    MatchesPlayed = cumsum(attack==attack)-1,
    Round = MatchesPlayed %/% 9 + 1,
    PointsTally = cumsum(Points)-Points,
    Form = rollsumr(Points,k=3,fill=NA)/9,
    GoalDfTally = cumsum(GoalDf)-GoalDf
  ) %>% ungroup()

'
# Check Season window: mostly Aug-May; except for 01-02, it is Jul-May
View(dt %>% mutate(
  month = lubridate::month(t)
) %>% 
  group_by(Sea) %>% select(Sea,month) %>% distinct() %>% pivot_wider(names_from = Sea, values_from = month, values_fn = list))
'

## Data exploration

'
ger1 %>% group_by(Sea) %>% count()
# From 00-01 to 16-17, 17 seasons in total. 306 game per season.

# Get the data of Season 00-01. Run from Aug to May
ger1_0001 = ger1 %>% filter(Sea=="00-01")
# Get the 18 teams 
sort(unique(ger1_0001$HT))

# 9 Games per round, 34 rounds in total.
'

## 2: Newly promoted

teamlist = ger1 %>% select(Sea, HT) %>% distinct() %>% pivot_wider(names_from = "Sea", values_from = "HT", values_fn = list)

ger1$Np = 0

## 11,12

seasonsummary = dt %>% group_by(Sea,attack) %>%  summarise(
  SeasonPoints = sum(Points),
  SeasonGD = sum(GoalDf)
  ) %>% arrange(Sea,desc(SeasonPoints)) %>% 
  mutate(
    Rankings = order(SeasonPoints, decreasing = T)
  )

roundsummary = dt %>% group_by(Sea,Round,attack) %>%  summarise(
  RoundPoints = sum(Points),
  RoundGD = sum(GoalDf)
) %>% arrange(Sea,Round,desc(RoundPoints)) %>% 
  mutate(
    Rankings = order(RoundPoints, decreasing = T)
  )


## Check data forn one team

View(dt %>% filter(attack=="Dortmund"))




