# Handling Draws: Calculate p(y_ijt=y)

## 1. Ordinal

Ord_prob <- function(y,delta,lit,ljt) {
  delta0 = delta[1]
  delta1 = delta[2]
  
  P0 = exp(delta0+lit)/(exp(delta0+lit)+exp(ljt))
  P1 = exp(delta1+lit)/(exp(delta1+lit)+exp(ljt))

  return((y == 2)*(1 - P1) + (y == 1)*(P1-P0) + (y == 0)*P0)
}

## 2. Davidson

Dav_prob <- function(y,delta,lit,ljt) {
  piit <- exp(lit)
  pijt <- exp(ljt)
  
  num = delta*sqrt(piit*pijt)
  denom = piit + pijt + num
  p1 = num/denom
  
  return((y == 2)*((1-p1)*piit/(piit+pijt)) + (y == 1)*p1 + (y == 0)*((1-p1)*pijt/(piit+pijt)))
}

