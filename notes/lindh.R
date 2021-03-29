baseCase = function(V,A,a,B,b){
  y = A*V^a-B*V^b
  return(y)
}

exponentialGrowth = function(x,P){baseCase(V=x,A=P,a=1,B=0,b=1)}
#Vmax = 1
logisticGrowth = function(x,P){baseCase(V=x,A=P,a=1,B=P,b=2)}
WBEGrowth = function(x,P){baseCase(V=x,A=P,a=3/4,B=P,b=1)}


veg = seq(0,1,by=0.01)
P = seq(0.25,4,by = 0.5)

plot(veg,1/veg*exponentialGrowth(x=veg,P=P[i]),type='n',ylim=c(0,5))
for(i in 1:length(P)){
lines(veg,1/veg*exponentialGrowth(x=veg,P=P[i]),type='l',col=i)
}

plot(veg,1/veg*logisticGrowth(x=veg,P=P[i]),type='n',ylim=c(0,5))
for(i in 1:length(P)){
lines(veg,1/veg*logisticGrowth(x=veg,P=P[i]),type='l',col=i)
}

plot(veg,1/veg*WBEGrowth(x=veg,P=P[i]),type='n',ylim=c(0,10))
for(i in 1:length(P)){
  lines(veg,1/veg*WBEGrowth(x=veg,P=P[i]),type='l',col=i)
}


