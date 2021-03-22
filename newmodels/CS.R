ll_CS = function(par,data,team_list,method_prob) {
  nteam = length(team_list)
  
  alpha = par[1:nteam]
  beta = par[nteam+1]
  delta = par[-c(1:(nteam+1))]
  
  strength = alpha[as.numeric(data$attack)]
  
  lit = strength + beta*data$Home
  ljt = strength + beta*(1-data$Home)
  
  y = data$WDL
  
  lh = method_prob(y,delta,lit,ljt) # the vector of likelihood
  ll = sum(log10(lh))
  
  return(ll)
}

team_list = unique(dt$attack)
ll_CS(c(rep(0,length(team_list)),.5,.5,.6),dt,team_list,Ord_prob)

optim(par=c(rep(0,length(team_list)),.5,.5,.6),ll_CS,data=dt,team_list=team_list,method_prob = Ord_prob,method="BFGS",control=list(fnscale=-1) )