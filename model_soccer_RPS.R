# Packages 
## Set up
library(tidyverse)
library("dplyr")
#install.packages("zoo")
library("zoo")
library(lubridate)
# install.packages("reshape2")
library("reshape2")
library(lubridate)
library("fastDummies")

#Set your directory
setwd("~/GitHub/project1/")


## 0. Get data from features.R 
  # Run this to get the dataset 
  source("features.R")


  # get the functions that are necessary 
  league_chosen = "GER2"
  source("models/prop.R")

# the dataset is called dt 

## 1. Model 1 : Baseline 
  source("models/BL.R")

N = (nrow(dt))
init = c(.5,.5)
Nseason = nrow(dt)

#Little tweaks to subset the data or it will take hours 
tr_data_par = dt 
ptrdata = tr_data_par[550:600,]

#Create empty cols for the predictions 
ptrdata$par1 = NaN
ptrdata$par2 = NaN
for(i in 1:50) {
  # get the subset 
  endi <-i+550
  tr_subset = tr_data_par[500:endi,]
  # get the suited parameters
  re = optim(par=init,fn=ll_BL,data=tr_subset,method_pdf=Dav_pdf,method="BFGS",control=list(fnscale=-1))
  init = re$par
  ptrdata[i,"par1"] = init[1]
  ptrdata[i,"par2"] = init[2]
}

# Will get the proba of loosing (0), winning (2), having a draw(1)
ptrdata$p0 = NaN
ptrdata$p1 = NaN
ptrdata$p2 = NaN

# Get the RPS 
RPS_fun<-function(p0,p1,a0,a1){
  s1 = (p0-a0) 
  s2 = (p0-a0+p1-a1)
  r = (s1*s1 + s2*s2)/2
  return(r)
}

# Compute the probas, RPS, and accuracy (outcome and accuracy doesnt work)

ptrdata = ptrdata %>% mutate(
  p0 = Dav_pdf(0,par2,par1,Home,Away,t,ptrdata),
  p1 = Dav_pdf(1,par2,par1,Home,Away,t,ptrdata),
  p2 = Dav_pdf(2,par2,par1,Home,Away,t,ptrdata),
  # Build a table similar to what's in the paper
  # a1 = team i loose WDL_0 
  # a2 = team i draw WDL_1
  # a3 = team i win WDL_2
  WDL_0 = 1*(WDL == 0),
  WDL_1 = 1*(WDL == 1),
  WDL_2 = 1*(WDL == 2),
  RPS = RPS_fun(p0,p1,WDL_0,WDL_1),
  #!@ Problem with computing the accuracy
  max_p = max(p0,p1,p2,na.rm = TRUE),
  Outcome = 0*(max_p == p0)+1*(max_p == p1)+2*(max_p == p2),
  Accuracy = as.numeric(1 == WDL)
)

# Save the results
ptrdata.dav = ptrdata
View(ptrdata.dav)
mean(ptrdata.dav$RPS,na.rm=TRUE)


## 2. Model 2 : Ordinal 

# Does not work 

# 2 - For Ordinal function, using parameters estimated from previous games
N = (nrow(dt))
init = c(.5,.5)
Nseason = nrow(dt)
tr_data_par = dt #rbind(trdata_season, trdata) # Add the data from trdata_season GER1, to compute the parameters. 
ptrdata = tr_data_par[550:600,]
ptrdata$par1 = NaN
ptrdata$par2 = NaN
ptrdata$par3 = NaN

for(i in 1:50) {
  # get the subset 
  endi <-i+550
  tr_subset = tr_data_par[500:endi,]
  # get the suited parameters
  #get the suited parameters 
  #!@ ERROR IN THE OPTIM FUNCTION 
  re = optim(par=init,fn=ll_BL,data=tr_subset,method_pdf=Ord_pdf,method="BFGS",control=list(fnscale=-1))
  init = re$par
  ptrdata[i,"par1"] = init[1]
  ptrdata[i,"par2"] = init[2]
  ptrdata[i,"par3"] = init[3]
}

ptrdata = ptrdata %>% mutate(
  p0 = Ord_pdf(0,c(par2,par3),Home,Away,t,ptrdata),
  p1 = Ord_pdf(1,c(par2,par3),Home,Away,t,ptrdata),
  p2 = Ord_pdf(2,c(par2,par3),Home,Away,t,ptrdata),
  # Build a table similar to what's in the paper
  # a1 = team i loose WDL_0 
  # a2 = team i draw WDL_1
  # a3 = team i win WDL_2
  WDL_0 = 1*(WDL == 0),
  WDL_1 = 1*(WDL == 1),
  WDL_2 = 1*(WDL == 2),
  RPS = RPS_fun(p0,p1,WDL_0,WDL_1),
  #Problem with computing the accuracy
  max_p = max(p0,p1,p2,na.rm = TRUE),
  Outcome = 0*(max_p == p0)+1*(max_p == p1)+2*(max_p == p2),
  Accuracy = as.numeric(1 == WDL)
)

# Save the results
ptrdata.ord = ptrdata
View(ptrdata.ord)
mean(ptrdata.dav$RPS,na.rm=TRUE)

## 3. Model 3 : Model davidson + CS 
# does not work 
source("models/CS.R")


N = (nrow(dt))
init = c(.5,.5)
Nseason = nrow(dt)
tr_data_par = dt #rbind(trdata_season, trdata) # Add the data from trdata_season GER1, to compute the parameters. 
ptrdata = tr_data_par[550:600,]
ptrdata$par1 = NaN
ptrdata$par2 = NaN
for(i in 1:50) {
  # get the subset 
  endi <-i+550
  tr_subset = tr_data_par[500:endi,]
  # get the suited parameters
  re = optim(par=init,fn=ll_BL,data=tr_subset,method_pdf=Dav_pdf,method="BFGS",control=list(fnscale=-1))
  init = re$par
  ptrdata[i,"par1"] = init[1]
  ptrdata[i,"par2"] = init[2]
}

# Will get the proba of loosing (0), winning (2), having a draw(1)
ptrdata$p0 = NaN
ptrdata$p1 = NaN
ptrdata$p2 = NaN

# Get the RPS 
RPS_fun<-function(p0,p1,a0,a1){
  s1 = (p0-a0) 
  s2 = (p0-a0+p1-a1)
  r = (s1*s1 + s2*s2)/2
  return(r)
}

ptrdata = ptrdata %>% mutate(
  p0 = Dav_pdf(0,par2,par1,Home,Away,t,ptrdata),
  p1 = Dav_pdf(1,par2,par1,Home,Away,t,ptrdata),
  p2 = Dav_pdf(2,par2,par1,Home,Away,t,ptrdata),
  # Build a table similar to what's in the paper
  # a1 = team i loose WDL_0 
  # a2 = team i draw WDL_1
  # a3 = team i win WDL_2
  WDL_0 = 1*(WDL == 0),
  WDL_1 = 1*(WDL == 1),
  WDL_2 = 1*(WDL == 2),
  RPS = RPS_fun(p0,p1,WDL_0,WDL_1),
  #Problem with computing the accuracy
  max_p = max(p0,p1,p2,na.rm = TRUE),
  Outcome = 0*(max_p == p0)+1*(max_p == p1)+2*(max_p == p2),
  Accuracy = as.numeric(1 == WDL)
)

# Save the results
ptrdata.dav = ptrdata
View(ptrdata.dav)
mean(ptrdata.dav$RPS,na.rm=TRUE)



## 2. Model 2 : Model davidson + LF  
source("LF.R")
# To do 

N = (nrow(dt))
init = c(.5,.5)
Nseason = nrow(dt)
tr_data_par = dt #rbind(trdata_season, trdata) # Add the data from trdata_season GER1, to compute the parameters. 
ptrdata = tr_data_par[550:600,]
ptrdata$par1 = NaN
ptrdata$par2 = NaN
for(i in 1:50) {
  # get the subset 
  endi <-i+550
  tr_subset = tr_data_par[500:endi,]
  # get the suited parameters
  re = optim(par=init,fn=ll_BL,data=tr_subset,method_pdf=Dav_pdf,method="BFGS",control=list(fnscale=-1))
  init = re$par
  ptrdata[i,"par1"] = init[1]
  ptrdata[i,"par2"] = init[2]
}

# Will get the proba of loosing (0), winning (2), having a draw(1)
ptrdata$p0 = NaN
ptrdata$p1 = NaN
ptrdata$p2 = NaN

# Get the RPS 
RPS_fun<-function(p0,p1,a0,a1){
  s1 = (p0-a0) 
  s2 = (p0-a0+p1-a1)
  r = (s1*s1 + s2*s2)/2
  return(r)
}

ptrdata = ptrdata %>% mutate(
  p0 = Dav_pdf(0,par2,par1,Home,Away,t,ptrdata),
  p1 = Dav_pdf(1,par2,par1,Home,Away,t,ptrdata),
  p2 = Dav_pdf(2,par2,par1,Home,Away,t,ptrdata),
  # Build a table similar to what's in the paper
  # a1 = team i loose WDL_0 
  # a2 = team i draw WDL_1
  # a3 = team i win WDL_2
  WDL_0 = 1*(WDL == 0),
  WDL_1 = 1*(WDL == 1),
  WDL_2 = 1*(WDL == 2),
  RPS = RPS_fun(p0,p1,WDL_0,WDL_1),
  #Problem with computing the accuracy
  max_p = max(p0,p1,p2,na.rm = TRUE),
  Outcome = 0*(max_p == p0)+1*(max_p == p1)+2*(max_p == p2),
  Accuracy = as.numeric(1 == WDL)
)

# Save the results
ptrdata.dav = ptrdata
View(ptrdata.dav)
mean(ptrdata.dav$RPS,na.rm=TRUE)

