RPS_fun<-function(p0,p1,a0,a1){
  s1 = (p0-a0) 
  s2 = (p0-a0+p1-a1)
  r = (s1*s1 + s2*s2)/2
  return(r)
}

# Get the estimated parameters
trdata = dt %>% filter(Sea %in% c("02-03")) # get the training data
idlist = unique(trdata$id)

init = c(.5,.5,.6)
init = c(.5,.5)

out = matrix(ncol=length(init),nrow=length(idlist))
flag = 1

for(i in idlist) {
  trsubset = dt %>% filter(id<i)
  # re = optim(par=init,fn=ll_BL,data=trsubset,method_prob=Ord_prob,method="BFGS",control=list(fnscale=-1))
  # init optim for Dav
  re = optim(par=init,fn=ll_BL,data=trsubset,method_prob=Dav_prob,method="BFGS",control=list(fnscale=-1))
  init = re$par
  out[flag,] = init
  flag = flag+1
}

pred = trdata %>% left_join(data.frame(id=idlist,out),by="id")
parlist = c("X1")
lit = rowSums(pred[parlist]*pred["Home"])
ljt = rowSums(pred[parlist]*pred["Away"])


p0 = Ord_prob(0,pred[c("X2","X3")],lit,ljt)
p1 = Ord_prob(1,pred[c("X2","X3")],lit,ljt)
p2 = Ord_prob(2,pred[c("X2","X3")],lit,ljt)
a0 = as.numeric(trdata$WDL == 0)
a1 = as.numeric(trdata$WDL == 1)

p0 = Dav_prob(0,pred[c("X2")],lit,ljt)
p1 = Dav_prob(1,pred[c("X2")],lit,ljt)
p2 = Dav_prob(2,pred[c("X2")],lit,ljt)
a0 = as.numeric(trdata$WDL == 0)
a1 = as.numeric(trdata$WDL == 1)

rps = RPS_fun(p0,p1,a0,a1)
mean(rps$X3,na.rm = TRUE)
sd(rps$X3,na.rm=TRUE)/sqrt(length(a0))

rps = RPS_fun(p0,p1,a0,a1)
mean(rps$X2,na.rm = TRUE)
sd(rps$X2,na.rm=TRUE)/sqrt(length(a0))

# Ord: 0.2262799 (0.003808165)
# Dav: 0.2262788 (0.003808268)

outcomes = mapply(function(p0,p1,p2) which.max(c(p0,p1,p2))-1, p0=p0$X3,p1=p1$X3,p2=p2$X3)
accuracy = 1*(outcomes == pred$WDL)
mean(accuracy)
sd(accuracy,na.rm = TRUE)/sqrt(length(outcomes))

outcomes = mapply(function(p0,p1,p2) which.max(c(p0,p1,p2))-1, p0=p0$X2,p1=p1$X2,p2=p2$X2)
accuracy = 1*(outcomes == pred$WDL)
mean(accuracy)
sd(accuracy,na.rm = TRUE)/sqrt(length(a0))

# Ord: 0.4477124 (0.02011693)
# Dav: 0.4477124 (0.02011693)