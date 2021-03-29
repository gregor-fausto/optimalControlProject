
library(deSolve)
# what ode method to use?
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

outputVec <- list.files("~/Dropbox/optimalControlProject/discretize-optimize/analysisFour/")

outputVec <- "normal-0.1-1-1-0-.RDS"

tmp <- readRDS(paste0("~/Dropbox/optimalControlProject/discretize-optimize/analysisFour/",outputVec))
names(tmp)

fit <- tmp$fit;
fvals <- tmp$fvals;
penVec <- tmp$penVec;
objVec <- tmp$objVec;
u.list <- tmp$u.list;
beta1.list <- tmp$beta1.list;
beta2.list <- tmp$beta2.list;
inits <- tmp$inits

n = length(fvals)

source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",tmp$model,".R"))
derivs=numeric(6); 

n=length(tmp$u.list)
initVals = c(tmp$inits[c("P","V","I","L")],c(pen=0,obj=0))
mParms = tmp$inits[c("m1","m2","alpha","gamma")]
outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=tmp$u.list[[n]],f2=tmp$beta1.list[[n]],f3=tmp$beta2.list[[n]]);


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

#initVals = c(inits,other) 

par(mfrow=c(2,2))
library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "PuRd"))(n)

plot(seq(0,5,length.out=n),seq(0,1,length.out=n),type='n')
for(i in 1:n){
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
  lines(outMat[,1],outMat[,2],col=colors[i])
}

plot(seq(0,5,length.out=n),seq(0,2,length.out=n),type='n')
for(i in 1:n){
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
  lines(outMat[,1],outMat[,3],col=colors[i])
}

plot(seq(0,5,length.out=n),seq(0,.5,length.out=n),type='n')
for(i in 1:n){
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=u.list[[i]],f2=beta1.list[[i]],f3=beta2.list[[i]]);
  lines(outMat[,1],outMat[,4],col=colors[i])
}

plot(seq(0,5,length.out=n),seq(0,1,length.out=n),type='n')
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