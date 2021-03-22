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
init = c(.5,.5,.6)
l_init = length(init)
# init davidson 
# init = c(c(.5))

out = matrix(ncol=length(init),nrow=length(idlist))
flag = 1

for(i in idlist) {
  trsubset = dt %>% filter(id<i)
  re = optim(par=init,fn=ll_BL,data=trsubset,method_prob=Ord_prob,method="BFGS",control=list(fnscale=-1))
  init = re$par
  out[flag,] = init
  # out[flag,-c(1:l_init)] = init[-c(1:l_init)]
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

rps = RPS_fun(p0,p1,a0,a1)
mean(rps[!is.na(rps)])  # 0.2262799
# mean(rps,na.rm = TRUE) #@ doesn't work somehow

