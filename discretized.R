

V.fun = function(Vt, beta1, u){
  #Vt1 = Vt*exp(-beta1*(1-u))
  Vt1=rpois(Vt,exp(-beta1*(1-u)))
  return(Vt1)
}

L.fun = function(Vt, beta1, Lt){
  Lt1 = Lt + beta1*Vt
  return(Lt1)
}

I.fun = function(Vt, beta1, u, beta2, It){
 # It1 = It*exp((beta1*(1-u)*Vt)/It - beta2)
  It1 = It + (beta1*(1-u)*Vt - beta2*It)
  return(It1)
}

F.fun = function(Ft, beta2, It){
  Ft1 = Ft + beta2*It
  return(Ft1)
}

states = matrix(NA,nrow=10,ncol=4)
states[1,] = c(1,0,0,0)
beta1 = 1
beta2 = 1
u = sample(c(1,0),9,replace=TRUE,prob=c(.5,.5))

for(i in 1:9){
  V.tmp=sum(V.fun(Vt = states[i,1], beta1 = beta1, u = u[i]))
  L.tmp=L.fun(Vt = states[i,1], beta1 = beta1, Lt = states[i,2])
  I.tmp=I.fun(Vt = states[i,1], beta1 = beta1, u = u[i], beta2 = beta2, It = states[i,3])
  F.tmp=F.fun(Ft = states[i,4], beta2 = beta2, It = states[i,3])
  states[i+1,]= c(V.tmp,L.tmp,I.tmp,F.tmp)
}
max(states[,4])

par(mfrow=c(2,2))
plot(1:10,states[,1],type='b')
plot(1:10,states[,2],type='b')
plot(1:10,states[,3],type='b')
plot(1:10,states[,4],type='b')
cbind(states,c(u,NA))
