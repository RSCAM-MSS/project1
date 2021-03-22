ll_BL = function(par,data,method_prob) {
  beta = par[1]
  delta = par[-1]
  
  lit = beta*data$Home
  ljt = beta*(1-data$Home)
  
  y = data$WDL
  
  lh = method_prob(y,delta,lit,ljt) # the vector of likelihood
  ll = sum(log10(lh))
  
  return(ll)
}


# test on optim
# optim(par=c(0.5,0.5),fn=ll_BL,data=dt,method_prob=Dav_prob,method="BFGS",control=list(fnscale=-1))
# optim(par=c(-0.1,-0.1,1.),fn=ll_BL,data=dt,method_prob=Ord_prob,method="BFGS",control=list(fnscale=-1))
