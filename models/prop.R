# Handling Draws: Calculate p(y_ijt=y)

## 1. Ordinal

Ord_cdf <- function(y,delta,beta,i,j,t,ma) {
  l_it <- lit(beta,i,t,ma)
  l_jt <- lit(beta,j,t,ma)
  num = exp(delta[y+1]+l_it)
  denom = num + exp(l_jt)
  return(num/denom)
}

Ord_pdf <- function(y,delta,beta,i,j,t,ma) {
  case_when(
    y == 2 ~ 1 - Ord_cdf(1,delta,beta,i,j,t,ma),
    y == 1 ~ Ord_cdf(1,delta,beta,i,j,t,ma) - Ord_cdf(0,delta,beta,i,j,t,ma),
    y == 0 ~ Ord_cdf(0,delta,beta,i,j,t,ma)
  )
}

## 2. Davidson

Dav_pdf <- function(y,delta,beta,i,j,t,ma) {
  piit <- exp(lit(beta,i,t,ma))
  pijt <- exp(lit(beta,j,t,ma))
  
  num = delta*sqrt(piit*pijt)
  denom = piit + pijt + num
  p1 = num/denom
  
  case_when(
    y == 1 ~ p1,
    y == 0 ~ (1-p1)*pijt/(piit+pijt),
    y == 2 ~ (1-p1)*piit/(piit+pijt)
  )
}
