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
init = c(rep(0.6,nteam),.5,.5,.6)
l_init = length(init)
# init davidson 
# init = c(c(.5))

out = matrix(ncol=length(init),nrow=length(idlist))
flag = 1

for(i in idlist) {
  trsubset = dt %>% filter(id<i)
  re = optim(par=init,fn=ll_CS,data=trsubset,team_list=team_list,method_prob=Ord_prob,method="BFGS",control=list(fnscale=-1))
  init = re$par
  out[flag,] = init
  # out[flag,-c(1:l_init)] = init[-c(1:l_init)]
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

rps = RPS_fun(p0,p1,a0,a1)
mean(rps[!is.na(rps)]) # .2262
# mean(rps,na.rm = TRUE) #@ doesn't work somehow
