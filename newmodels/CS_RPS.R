# Define the RPS 
RPS_fun<-function(p0,p1,a0,a1){
  s1 = (p0-a0) 
  s2 = (p0-a0+p1-a1)
  r = (s1*s1 + s2*s2)/2
  return(r)
}

# Get the estimated parameters
trdata = dt %>% filter(Sea %in% c("02-03")) # get the training data
idlist = unique(trdata$id)
ngame = nrow(trdata)
nteam = length(unique(dt$attack)) #  %>% filter(Home==1))
team_list = unique(dt$attack)
# init = c(rep(0.6,nteam),.5,.5,.6)
# init davidson 
init = c(rep(0.6,nteam),.5,.5)

out = matrix(ncol=length(init),nrow=length(idlist))
flag = 1

for(i in idlist) {
  trsubset = dt %>% filter(id<i)
  # re = optim(par=init,fn=ll_CS,data=trsubset,team_list=team_list,method_prob=Ord_prob,method="BFGS",control=list(fnscale=-1))
  re = optim(par=init,fn=ll_CS,data=trsubset,team_list=team_list,method_prob=Dav_prob,method="BFGS",control=list(fnscale=-1))
  init = re$par
  out[flag,] = init
  flag = flag+1
}

pred = trdata %>% left_join(data.frame(id=idlist,out),by="id")
parlist = colnames(pred)[61:87]
pred[87] = pred[87]*pred["Home"]
lit = rowSums(pred[c(parlist)])
ljt = rep(0,ngame)
ljt[seq(1,ngame,by=2)] = lit[seq(2,ngame,by=2)]
ljt[seq(2,ngame,by=2)] = lit[seq(1,ngame,by=2)]

p0 = Ord_prob(0,pred[c("X58","X59")],lit,ljt)
p1 = Ord_prob(1,pred[c("X58","X59")],lit,ljt)
p2 = Ord_prob(2,pred[c("X58","X59")],lit,ljt)
a0 = as.numeric(trdata$WDL == 0)
a1 = as.numeric(trdata$WDL == 1)

p0 = Dav_prob(0,pred[c("X58")],lit,ljt)
p1 = Dav_prob(1,pred[c("X58")],lit,ljt)
p2 = Dav_prob(2,pred[c("X58")],lit,ljt)
a0 = as.numeric(trdata$WDL == 0)
a1 = as.numeric(trdata$WDL == 1)

rps = RPS_fun(p0,p1,a0,a1)
mean(rps$X59,na.rm = TRUE)
sd(rps$X59,na.rm=TRUE)/sqrt(length(a0))

rps = RPS_fun(p0,p1,a0,a1)
mean(rps$X58,na.rm = TRUE)
sd(rps$X58,na.rm=TRUE)/sqrt(length(a0))

# Ord: 0.2262823 (0.003808137)
# Dav: 0.2262788 (0.003808269)


outcomes = mapply(function(p0,p1,p2) which.max(c(p0,p1,p2))-1, p0=p0$X59,p1=p1$X59,p2=p2$X59)
accuracy = 1*(outcomes == pred$WDL)
mean(accuracy)
sd(accuracy,na.rm = TRUE)/sqrt(length(outcomes))

outcomes = mapply(function(p0,p1,p2) which.max(c(p0,p1,p2))-1, p0=p0$X58,p1=p1$X58,p2=p2$X58)
accuracy = 1*(outcomes == pred$WDL)
mean(accuracy)
sd(accuracy,na.rm = TRUE)/sqrt(length(outcomes))

# Ord: 0.4477124 (0.02011693)
# Dav: 0.4477124 (0.02011693)
