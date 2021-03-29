
library(deSolve)
# what ode method to use?
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

outputVec <- list.files("~/Dropbox/optimalControlProject/output/")

outputVec <- "unbranchedDeterminate-uniform-point5-1point1.RDS"
#outputVec<-outputVec[grep("uniform",outputVec)][23]
#outputVec<-outputVec[-grep("branched",outputVec)]

runList <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec))
names(runList)

fit <- runList$fit;
fvals <- runList$fvals;
penVec <- runList$penVec;
objVec <- runList$objVec;
u.list <- runList$u.list;
beta1.list <- runList$beta1.list;
beta2.list <- runList$beta2.list;
inits <- runList$inits

n = length(fvals)

source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList$model,".R"))
derivs=numeric(6); 

source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList$initsName,".R"))

# remove once script fixed
topt=seq(0,5,length=11); 


par(mfrow=c(3,1))
plot(2:n,fvals[2:n],type='b',
     xlab='Optimization iterations',ylab="Value",ylim=c(min(fvals[2:n]),max(fvals[2:n])))

plot(2:n,objVec[2:n],type='b',
     xlab='Optimization iterations',ylab="Objective")

plot(2:n,penVec[2:n],type='b',
     xlab='Optimization iterations',ylab="Penalty",
     ylim=c(0,max(penVec[2:n])))
abline(h=0)

# val = obj - pwt*pen - lambda*sum(wiggly^2)  ## SPE: sum instead of mean on wiggliness

# plot(2:n,-((-fvals[2:n])-objVec[2:n])+penVec[2:n]*10);abline(h=0,lty='dotted')
# abline(h=0)

#########################
# Plot controls
#########################

library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "PuRd"))(n)

par(mfrow=c(1,3))
# u
plot(c(0,5),c(0,1),type='n')
for(i in 1:n){
  lines(topt,u.list[[i]](topt),col=colors[i])
}

# beta1
plot(c(0,5),c(0,8),type='n')
for(i in 1:n){
  lines(topt,beta1.list[[i]](topt),col=colors[i])
}

# beta2
plot(c(0,5),c(0,8),type='n')
for(i in 1:n){
  lines(topt,beta2.list[[i]](topt),col=colors[i])
}

#########################
# Plot controls
#########################

library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "PuRd"))(n)

par(mfrow=c(1,3))
# u
plot(c(0,5),c(0,1),type='n')
lines(topt,u.list[[n]](topt),col=colors[i])

# beta1
plot(c(0,5),c(0,8),type='n')
lines(topt,beta1.list[[n]](topt),col=colors[i])

# beta1
plot(c(0,5),c(0,8),type='n')
lines(topt,beta2.list[[n]](topt),col=colors[i])

#########################
# Plot states
#########################

initVals = c(inits,other) 

par(mfrow=c(2,2))
library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "PuRd"))(n)

plot(seq(0,5,length.out=n),seq(0,1,length.out=n),type='n')
for(i in 1:n){
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
  lines(outMat[,1],outMat[,2],col=colors[i])
}

plot(seq(0,5,length.out=n),seq(0,1.2,length.out=n),type='n')
for(i in 1:n){
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
  lines(outMat[,1],outMat[,3],col=colors[i])
}

plot(seq(0,5,length.out=n),seq(0,1.2,length.out=n),type='n')
for(i in 1:n){
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
  lines(outMat[,1],outMat[,4],col=colors[i])
}

plot(seq(0,5,length.out=n),seq(0,1.2,length.out=n),type='n')
for(i in 1:n){
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
  lines(outMat[,1],outMat[,5],col=colors[i])
}

#########################
# Plot states
#########################

initVals = c(inits,other) 

par(mfrow=c(2,2))
library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "PuRd"))(n)

plot(seq(0,5,length.out=n),seq(0,1,length.out=n),type='n')
for(i in (n-1):n){
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
  lines(outMat[,1],outMat[,2],col=colors[i],lty=i)
}

plot(seq(0,5,length.out=n),seq(0,1.2,length.out=n),type='n')
for(i in (n-1):n){
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
  lines(outMat[,1],outMat[,3],col=colors[i],lty=i)
}

plot(seq(0,5,length.out=n),seq(0,1,length.out=n),type='n')
for(i in (n-1):n){
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
  lines(outMat[,1],outMat[,4],col=colors[i],lty=i)
}

plot(seq(0,5,length.out=n),seq(0,1.2,length.out=n),type='n')
for(i in (n-1):n){
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
  lines(outMat[,1],outMat[,5],col=colors[i],lty=i)
}
# 
# i=20
#   outMat20 = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
# i=21
#   outMat21 = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
#   i=22
#   outMat22 = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
#   
#   df<-cbind(outMat20[,7],outMat21[,7],outMat22[,7])
#   df
#   
#   df<-cbind(outMat20[,5],outMat21[,5],outMat22[,5])
#   df
#   
#   plot(outMat20[,7],type='n')
#   for(i in 2:n){
#     outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
#     lines(outMat[,7],col=colors[i])
#     }
#   
# print(runList$initsName)
# 
