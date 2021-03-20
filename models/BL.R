# The Baseline Model

## \lambda_{it} = \beta h_{it}
# Baseline model adapted to the new names given to the features from features exctraction

## Features: Home

lit <- function(beta,i,t,ma) {
  hit = (ma$Home == i)
  return(beta*hit)
}


# Compute the loglikelihood

ll_BL <- function(theta,data,method_pdf) {
  beta = theta[1]
  delta = theta[-1]
  
  N = nrow(data)
  ll = 0
  
  for (k in 1:N) {
    ma = data[k,]
    i = ma$Home
    j = ma$Away
    t = ma$t
    y = ma$WDL
    
    ll = ll + log10(method_pdf(y,delta,beta,i,j,t,ma))
  }
  
  return(ll)
}