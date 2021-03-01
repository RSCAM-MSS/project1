# test the BL,CS,LF model

## set up 
library(tidyverse)


## read the data
data <- read.table("data/ISDBv2.0.txt",sep="\t",header=TRUE)

## feature extraction on a small subset
newdata <- data %>% filter(Lge=="GER1", Sea=="16-17") %>% 
  mutate(
    id = row_number(),
    .before = Sea
  ) %>% 
  pivot_longer(
    cols = c("HT","AT"),
    names_to = "Home",
    values_to = "Team"
  ) %>% 
  mutate(
    Points = 3*(WDL == "W")*(Home == "HT") + 3*(WDL == "L")*(Home == "AT") + (WDL == "D")
  ) %>% 
  group_by(Sea,Lge,Team) %>%
  mutate(
    PT = cumsum(Points)-Points, ## get points tally
    GD = cumsum(GD*(Home == "HT")) - GD*(Home == "HT"),
    SeasonGD = sum(GD*(Home == "HT"))
  ) %>% ungroup() %>%
  select(-Points) %>% 
  pivot_wider(names_from = Home,values_from = c(Team,PT,GD,SeasonGD)) %>% 
  summarise(
    id, Sea, Lge, Date,
    HT = ordered(Team_HT,levels = unique(.data$Team_HT)),
    AT = ordered(Team_AT,levels = unique(.data$Team_HT)),
    H = as.integer(HT),
    A = as.integer(AT),
    HPT = PT_HT, APT = PT_AT, HGD = GD_HT, AGD = GD_AT,
    HSGD = SeasonGD_HT, ASGD = SeasonGD_AT,
    WDL = 2*(WDL == "W") + (WDL == "D")
  )


## test
source("models/prop.R")

### BL
source("models/BL.R")

ptm <- proc.time()

init <- c(0.5,0.5)
optim(par=init,fn=ll_BL,data=newdata,method_pdf=Dav_pdf,method="BFGS",control=list(fnscale=-1))

proc.time() - ptm


### CS

source("models/CS.R")

ptm <- proc.time()

init <- c(rep(-2,17),.7,.7)  # N=18, alpha[2:N] = init[1:N-1], beta = init[N], delta = init[N+1]
optim(par=init,fn=ll_BL,data=newdata,method_pdf=Dav_pdf,method="BFGS",control=list(fnscale=-1))

proc.time() - ptm


### LF

source("models/LF.R")

ptm <- proc.time()

init <- c(.5,0,0,.5)
optim(par=init,fn=ll_LF,data=newdata,method_pdf=Dav_pdf,method="BFGS",control=list(fnscale=-1))

proc.time() - ptm