
f=function(a) {
v[i] = 1
  for(i in 1:4){
  v[i+1]=v[i]*a
  }
return(v)
}
f(a=1)
out=lapply(seq(1,2,by=.1),f)

plot(NA,NA,xlim=c(0,5),ylim=c(0,16))
for(i in 1:11){
  points(1:5,out[[i]],type='b',col=i)
}

plot(NA,NA,xlim=c(1,2),ylim=c(0,16))
for(i in 1:11){
  points(seq(1,2,by=.1)[i],(out[[11]]/out[[i]])[5],type='b')
}
