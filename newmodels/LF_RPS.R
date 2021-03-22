# Define the RPS 
RPS_fun<-function(p0,p1,a0,a1){
  s1 = (p0-a0) 
  s2 = (p0-a0+p1-a1)
  r = (s1*s1 + s2*s2)/2
  return(r)
}

# Get the estimated parameters
trdata = dt %>% filter(Sea %in% c("03-04")) # get the training data
idlist = unique(trdata$id)
feature_list = c("Home","PointsTally","GoalDfTally","PrevSeasonGD","PrevRoundRankings")
nfea = length(feature_list)
ngame = nrow(trdata)

init = c(rep(0,5),c(.5,.6))
init = c(rep(0,5),c(.5))

out = matrix(ncol=length(init),nrow=length(idlist))
flag = 1

for(i in idlist) {
  trsubset = dt %>% filter(id<i)
  # re = optim(par=init,fn=ll_LF,data=trsubset,feature_list=feature_list,method_prob=Ord_prob,method="BFGS",control=list(fnscale=-1))
  re = optim(par=init,fn=ll_LF,data=trsubset,feature_list=feature_list,method_prob=Dav_prob,method="BFGS",control=list(fnscale=-1))
  init = re$par
  out[flag,1:nfea] = init[1:nfea]
  out[flag,-c(1:nfea)] = init[-c(1:nfea)]
  flag = flag+1
}

pred = trdata %>% left_join(data.frame(id=idlist,out),by="id")
parlist = c("X1","X2","X3","X4","X5")
lit = rowSums(pred[parlist]*pred[feature_list])
ljt = rep(0,ngame)
ljt[seq(1,ngame,by=2)] = lit[seq(2,ngame,by=2)]
ljt[seq(2,ngame,by=2)] = lit[seq(1,ngame,by=2)]

p0 = Ord_prob(0,pred[c("X6","X7")],lit,ljt)
p1 = Ord_prob(1,pred[c("X6","X7")],lit,ljt)
p2 = Ord_prob(2,pred[c("X6","X7")],lit,ljt)
a0 = as.numeric(trdata$WDL == 0)
a1 = as.numeric(trdata$WDL == 1)

p0 = Dav_prob(0,pred[c("X6")],lit,ljt)
p1 = Dav_prob(1,pred[c("X6")],lit,ljt)
p2 = Dav_prob(2,pred[c("X6")],lit,ljt)
a0 = as.numeric(trdata$WDL == 0)
a1 = as.numeric(trdata$WDL == 1)

rps = RPS_fun(p0,p1,a0,a1)
mean(rps$X7, na.rm = TRUE)
sd(rps$X7,na.rm=TRUE)/sqrt(length(a0))

rps = RPS_fun(p0,p1,a0,a1)
mean(rps$X6, na.rm = TRUE)
sd(rps$X6,na.rm=TRUE)/sqrt(length(a0))

# Ord: 0.2309812 (0.004394275)
# Dav: 0.2302486 (0.004421922)


outcomes = mapply(function(p0,p1,p2) which.max(c(p0,p1,p2))-1, p0=p0$X7,p1=p1$X7,p2=p2$X7)
accuracy = 1*(outcomes == pred$WDL)
mean(accuracy,na.rm = TRUE)
sd(accuracy,na.rm = TRUE)/sqrt(length(outcomes))

outcomes = mapply(function(p0,p1,p2) which.max(c(p0,p1,p2))-1, p0=p0$X6,p1=p1$X6,p2=p2$X6)
accuracy = 1*(outcomes == pred$WDL)
mean(accuracy,na.rm = TRUE)
sd(accuracy,na.rm = TRUE)/sqrt(length(outcomes))

# Ord: 0.411215 (0.01993674)
# Dav: 0.4018692 (0.01986469)



