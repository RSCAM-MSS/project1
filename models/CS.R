# The Constant Strengths Model

## \lambda_{it} = \alpha_{i} + \beta h_{it}

## Features: Home


lit <- function(beta,i,t,ma) {
  # `beta` is a vector of length N+1 (N is the number of teams)
  # alpha = beta[1:N], beta = beta[N+1]
  hit = (ma$H == i)
  return(beta[i]+beta[length(beta)]*hit)
}


# Compute the loglikelihood

ll_BL <- function(theta,data,method_pdf) {
  N = length(unique(data$H))
  
  beta = c(0,theta[1:N]) # set beta[1] = 0, see p.8 Identifiability
  delta = theta[N+1]
  
  N = nrow(data)
  ll = 0
  
  for (k in 1:N) {
    ma = data[k,]
    i = ma$H
    j = ma$A
    t = ma$Date
    y = ma$WDL
    
    ll = ll + log10(method_pdf(y,delta,beta,i,j,t,ma))
  }
  
  return(ll)
}