# test the BL,CS,LF model

## set up 
library(tidyverse)


## read the data
#data <- read.table("data/ISDBv2.0.txt",sep="\t",header=TRUE)
# data_soccer <- load("~/GitHub/project1/data_output/dt_features.Rdata")
# Use the dataset produced by features.R

View(dt)
newdata = dt
## test
source("models/prop.R")

### BL
source("models/BL.R")

ptm <- proc.time()

init <- c(0.5,0.5)
optim(par=init,fn=ll_BL,data=newdata,method_pdf=Dav_pdf,method="BFGS",control=list(fnscale=-1))

proc.time() - ptm


### CS

source("models/CS.R")

ptm <- proc.time()

init <- c(rep(-2,17),.7,.7)  # N=18, alpha[2:N] = init[1:N-1], beta = init[N], delta = init[N+1]
optim(par=init,fn=ll_BL,data=newdata,method_pdf=Dav_pdf,method="BFGS",control=list(fnscale=-1))

proc.time() - ptm


### LF

source("models/LF.R")

ptm <- proc.time()

init <- c(.5,0,0,.5)
optim(par=init,fn=ll_LF,data=newdata,method_pdf=Dav_pdf,method="BFGS",control=list(fnscale=-1))

proc.time() - ptm