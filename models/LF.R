# The Linear with features Model

## \lambda_{it} = \sum_{k=1}^{p}\beta_{k}{\bf x}_{itk},
## where $x_{itk}$ is the $k$th element of the feature vector ${\bf x}_{it}$.

## Features: Home, Points tally, Goal Difference

## Home
lit <- function(beta,i,t, ma) {
  xit1 <- as.numeric(ma$H == i)
  
  if(xit1 == 1) {
    xit2 <- ma$HPT
    xit3 <- ma$HGD
  } else {
    xit2 <- ma$APT
    xit3 <- ma$AGD
  }
  
  xit <- c(xit1,xit2,xit3)
  
  return(beta %*% xit)
}

ll_LF <- function(theta,data,method_pdf) {
  beta = theta[1:3]
  delta = theta[4]
  
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