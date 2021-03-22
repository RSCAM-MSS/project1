ll_LF = function(par,data,feature_list,method_prob) {
  nfea = length(feature_list)
  ngame = nrow(data)
  
  beta = par[1:nfea]
  delta = par[-c(1:nfea)]
  
  lit = colSums(t(data[feature_list])*beta)
  ljt = rep(0,ngame)
  ljt[seq(1,ngame,by=2)] = lit[seq(2,ngame,by=2)]
  ljt[seq(2,ngame,by=2)] = lit[seq(1,ngame,by=2)]
  
  y = data$WDL
  
  lh = method_prob(y,delta,lit,ljt) # the vector of likelihood
  ll = sum(log10(lh[!is.na(lh)])) # drop the NA values
  
  return(ll)
}

# ll_LF(c(rep(0,5),.5,.6),dt,feature_list=c("Home","PointsTally","GoalDfTally","PrevSeasonGD","PrevRoundRankings"),Ord_prob)
# optim(par=c(rep(0,5),.5,.6),fn=ll_LF,data=dt,feature_list=c("Home","PointsTally","GoalDfTally","PrevSeasonGD","PrevRoundRankings"),method_prob=Ord_prob,method="BFGS",control=list(fnscale=-1))
