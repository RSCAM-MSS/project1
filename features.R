## Read the data
# data_soccer = read.table("data_input/ISDBv2.0.txt",sep="\t",header=TRUE) 
# AL download data setwd("~/GitHub/project1")
data_soccer <- read.delim("data_input/ISDBv2.0.txt")

## Feature extraction

#Create a function that exctract data for a league of interest
dataset_prep<-function(league_int){
  
  league_d = data_soccer %>% filter(Lge==league_int) %>% select(-Lge) %>% 
    mutate(
      #Get the season
      Sea = Sea,
      #get the date in a readable format
      Date = as.Date(Date,"%d/%m/%Y"), 
      #add the quarters of years 
      Quarter = quarter(Date),
      HT_names = as.character(HT),
      AT_names = as.character(AT),
      #!@ verify that it makes sens
      WDL_home = 2*(WDL == "W") + (WDL == "D"),
      WDL_attack = 2*(WDL == "L") + (WDL == "D")
    )
  
dt = data.frame(
    Sea = as.factor(c(league_d$Sea,league_d$Sea)),
    WDL = c(league_d$WDL_home,league_d$WDL_attack),
    # Date of the game
    t = as.Date(c(league_d$Date, league_d$Date)),
    # Quarter of the game
    Quarter = c(league_d$Quarter, league_d$Quarter),
    # Attack (team attacking/playing from home) 
    attack = as.factor(c(league_d$HT_names, league_d$AT_names)),
    # Attack as names 
    attack_names = c(league_d$HT_names, league_d$AT_names),
    # Defense (team defending)
    defense = as.factor(c(league_d$AT_names, league_d$HT_names)),
    # Defense as names
    defense_names = c(league_d$AT_names, league_d$HT_names),
    # Whether the game was played at home
    Home = c(rep(1,nrow(league_d)),rep(0,nrow(league_d))),
    # Whether the game was played away
    Away = c(rep(0,nrow(league_d)),rep(1,nrow(league_d))),
    # The number of goals scored before the game
    Goals = c(league_d$HS, league_d$AS),
    # The number of goals conceded before the game
    GoalsConceded = c(league_d$AS, league_d$HS),
    # Goal difference 
    GoalDf = c(league_d$GD, -league_d$GD)
    ) %>% 
    mutate(
    # not sure I get what is that. 
      Points = 3*(GoalDf > 0) + 1*(GoalDf == 0)
    ) %>% arrange(t) %>% 
    group_by(Sea,attack) %>%
    mutate(
      # 
      lag_time =  t-lag(t),
      # Attach is a categorical variable 
      MatchesPlayed = cumsum(attack==attack)-1,
      # Round 1=first match played, round 2: second...
      Round = MatchesPlayed + 1,
      #Points Tally
      PointsTally = cumsum(Points)-Points,
      # Form : A ninth of the total point gained in the last 3 matches 
      # When we don't have 3 matches previously, it is replaced with NA. 
      Form = rollsumr(Points,k=3,fill=NA)/9,
      # Goal difference tally
      GoalDfTally = cumsum(GoalDf)-GoalDf
    ) %>% ungroup()
  
  return(dt)

}

# Now we can prep dataset easily for any league 

dt = dataset_prep("GER2")

#seanames = levels(ger1$Sea)  # save season names

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
seanames = levels(dt$Sea)

dt = dt %>% mutate(
  Sea = as.integer(Sea) # change season names to 1,2,...,17,
                                                  # so that it is easier to merge the previous season/round data
)

## 2: Newly promoted/First game of the league in this season
## ------------------
# The block below is not very efficient code - could do better, but it works for now
teamlist = dt %>% distinct(Sea, attack) %>% group_by(Sea) %>% summarise(Sea,Teams = attack,.groups="drop") %>%
  distinct()
teamlist2 = dcast(teamlist,Sea~Teams,value.var="Sea",fill=0)  #! Add value.var="Sea" instead of using the for loop below
teamlist_league = colnames(teamlist2)[-1]
last_sea = max(teamlist$Sea)
last_rou = max(dt$Round)
sort_val<-function(col){
  if (is.na(col)){r = 0}
  else {r=1}
  return(r)
}
# Sea = c()
# attack = c()

teamlist2[,-1] = teamlist2[,-1] %>% rollapplyr(2,function(x) (x[1]==0)*(x[2]!=0), fill=0)
newly_promoted = teamlist2 %>% pivot_longer(-Sea,names_to="attack_names",values_to="NewlyPromoted")

# for (y in teamlist_league){
#   # for (x in 1:last_sea){
#   #   v = teamlist2[x,y]
#   #   if (is.na(v)){r = NA}
#   #   else {r=x}
#   #   teamlist2[x,y] = r
#   # }
#   # min_season = min(teamlist2[,y],na.rm=TRUE)
#   # if (min_season!=1){
#   #   Sea<-c(Sea,min_season)
#   #   attack<-c(attack,y)
#   # }
# }
# 
# newly_promoted<-data.frame(Sea, attack,newly_prom = rep(1,length(Sea)))

## ------------------------


## Add the information for the attack team
# !@ not sure it makes sense
# dtnew <- merge(dt, newly_promoted, by.x = c("Sea","attack_names"), by.y = c("Sea","attack"),all.x=TRUE)
dtnew <- left_join(dt, newly_promoted, by = c("Sea","attack_names"),all.x=TRUE)

#!@
# dtnew <- merge(dt, newly_promoted, by.x = c("Sea","defense_names"), by.y = c("Sea","attack"),all.x=TRUE)
# dtnew = dtnew %>% mutate(newly_prom = replace_na(newly_prom, 0))

dt = dtnew

# Dongrui code commented out
# # Get the list of all team 
# allteams = unique(ger1$HT)
# 
# newly = data.frame( t( apply( teamlist[2], 1, function(x) as.numeric( allteams %in% x[[1]]) ) ) ) %>% rollapplyr(2,function(x) as.numeric(x[1]==0 & x[2]==1),fill=NA)
# 
# colnames(newly) = allteams
# newly = cbind(teamlist["Sea"],newly)
# newly = newly %>% pivot_longer(-Sea,names_to="attack",values_to="NewlyPromoted")


## 11,12: Previous season/round points tally/goal difference

levels(dt$Sea) = c(1:last_sea)

seasonsummary = dt %>% group_by(Sea,attack) %>%  summarise(
  PrevSeasonPoints = sum(Points),
  PrevSeasonGD = sum(GoalDf)
  ) %>% arrange(Sea,desc(PrevSeasonPoints)) %>% 
  mutate(
    Sea = Sea + 1,
    PrevSeasonRankings = order(PrevSeasonPoints, decreasing = T)
  ) %>% ungroup() %>%  filter(Sea < last_sea+1)  #! save data of known seasons

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
  filter(Round < last_rou+1)

dt = dt %>% left_join(seasonsummary,by=c("Sea","attack")) %>%
  left_join(roundsummary,by=c("Sea","Round","attack")) %>% 
  mutate(
    Sea = ordered(Sea)
  )

#!@ did not know why important below 

levels(dt$Sea) = seanames # change the season names back


## Check data for one team
# 
# View(dt %>% filter(attack=="Dortmund"))


## Save data with features 
save(dt, file = "../project1/data_output/dt_features.Rdata")



