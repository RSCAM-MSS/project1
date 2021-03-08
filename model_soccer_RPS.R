
N = (nrow(trdata))
init = c(.5,.5)

# Add the data from trdata_season GER1, to compute the parameters. 
Nseason = nrow(trdata_season)
tr_data_par = rbind(trdata_season, trdata)

ptrdata = tr_data_par[613:918,]
ptrdata$par1 = NaN
ptrdata$par2 = NaN

for(i in 1:N) {
  # get the subset 
  endi <-i+612
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

ptrdata$p0 = mapply(function(par1,par2,i,j,t) Dav_pdf(0,par2,par1,i,j,t),par1 = ptrdata$par1, par2 = ptrdata$par2, i=ptrdata$H,j=ptrdata$A,t=ptrdata$Date)

ptrdata$p1 = mapply(function(par1,par2,i,j,t) Dav_pdf(1,par2,par1,i,j,t),par1 = ptrdata$par1, par2 = ptrdata$par2, i=ptrdata$H,j=ptrdata$A,t=ptrdata$Date)

ptrdata$p2 = mapply(function(par1,par2,i,j,t) Dav_pdf(2,par2,par1,i,j,t),par1 = ptrdata$par1, par2 = ptrdata$par2, i=ptrdata$H,j=ptrdata$A,t=ptrdata$Date)

# Build a table similar to what's in the paper
# a1 = team i loose WDL_0 
# a2 = team i draw WDL_1
# a3 = team i win WDL_2
ptrdata<-fastDummies::dummy_cols(ptrdata,select_columns = "WDL",remove_first_dummy = FALSE)


# Get the RPS 
ptrdata$RPS = mapply(function(p0,p1,a0,a1) ((p0-a0)^2 + (p0-a0+p1-a1)^2)/2,p0 = ptrdata$p0,p1 = ptrdata$p1, a0 =ptrdata$WDL_0,a1 = ptrdata$WDL_1)

# Get the predicted outcome and accuracy
ptrdata$Outcome = mapply(function(p0,p1,p2) (which.max(c(p0,p1,p2))-1),p0 = ptrdata$p0,p1 = ptrdata$p1,p2 = ptrdata$p2)

ptrdata$Accuracy = as.numeric(ptrdata$Outcome == ptrdata$WDL)

# Save the results
ptrdata.dav = ptrdata

View(ptrdata.dav)

mean(ptrdata.dav$RPS,na.rm=TRUE)

# 2 - For Ordinal function, using parameters estimated from previous games
# TO DO

init = c(-.5,0,.5)
ptrdata = trdata
ptrdata$par1 = NaN
ptrdata$par2 = NaN
ptrdata$par3 = NaN

for(i in 1:N) {
  # get the subset 
  endi <-i+612
  tr_subset = tr_data_par[500:endi,]
  
  #get the suited parameters 
  re = optim(par=init,fn=ll_BL,data=tr_subset,method_pdf=Ord_pdf,method="BFGS",control=list(fnscale=-1))
  init = re$par
  
  ptrdata[i,"par1"] = init[1]
  ptrdata[i,"par2"] = init[2]
  ptrdata[i,"par3"] = init[3]
}

# Will get the proba of loosing (0), winning (2), having a draw(1)
ptrdata$p0 = NaN
ptrdata$p1 = NaN
ptrdata$p2 = NaN

ptrdata$p0 = mapply(function(par1,par2,par3,i,j,t) Ord_pdf(0,c(par2,par3),par1,i,j,t),par1 = ptrdata$par1, par2 = ptrdata$par2, par3 = ptrdata$par3, i=ptrdata$H,j=ptrdata$A,t=ptrdata$Date)

ptrdata$p1 = mapply(function(par1,par2,par3,i,j,t) Ord_pdf(1,c(par2,par3),par1,i,j,t),par1 = ptrdata$par1, par2 = ptrdata$par2, par3 = ptrdata$par3, i=ptrdata$H,j=ptrdata$A,t=ptrdata$Date)

ptrdata$p2 = mapply(function(par1,par2,par3,i,j,t) Ord_pdf(2,c(par2,par3),par1,i,j,t),par1 = ptrdata$par1, par2 = ptrdata$par2, par3 = ptrdata$par3, i=ptrdata$H,j=ptrdata$A,t=ptrdata$Date)

# Build a table similar to what's in the paper
# a1 = team i loose WDL_0 
# a2 = team i draw WDL_1
# a3 = team i win WDL_2
ptrdata<-fastDummies::dummy_cols(ptrdata,select_columns = "WDL",remove_first_dummy = FALSE)


# Get the RPS 
ptrdata$RPS = mapply(function(p0,p1,a0,a1) ((p0-a0)^2 + (p0-a0+p1-a1)^2)/2,p0 = ptrdata$p0,p1 = ptrdata$p1, a0 =ptrdata$WDL_0,a1 = ptrdata$WDL_1)

# Get the predicted outcome and accuracy
ptrdata$Outcome = mapply(function(p0,p1,p2) (which.max(c(p0,p1,p2))-1),p0 = ptrdata$p0,p1 = ptrdata$p1,p2 = ptrdata$p2)

ptrdata$Accuracy = as.numeric(ptrdata$Outcome == ptrdata$WDL)

# Save the results
ptrdata.ord = ptrdata

View(ptrdata.ord)
mean(ptrdata.dav$RPS,na.rm=TRUE)
