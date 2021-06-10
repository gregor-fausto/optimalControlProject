
rm(list=ls(all=TRUE)) # clear R environment

par(mfrow=c(1,1))
library(deSolve)
# what ode method to use?
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

outputVec <- list.files("~/Dropbox/optimalControlProject/output/")
outputVec<-outputVec[grep("uniform1.RDS",outputVec)]
outputVec<-outputVec[!grepl("Resource",outputVec)&!grepl("Meristem",outputVec)][2]

runList <- list()


runList <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec))

derivs=numeric(6); 


# par(mfrow=c(2,2))
# library(RColorBrewer)
# colors <- colorRampPalette(brewer.pal(4, "PuRd"))(n)

source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList$model,".R"))
derivs=numeric(6); 
source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList$initsName,".R"))


initVals = c(inits,other) 
outMat = ode(y=initVals,times=seq(0,5,by=0.25),control,method=odemethod,parms=mParms,f1=runList$u.list[[20]],f2=runList$beta1.list[[20]],f3=runList$beta2.list[[20]]);

ut = runList$u.list[[20]](seq(0,5,by=0.25))
beta1 = runList$beta1.list[[20]](seq(0,5,by=0.25))
beta2 = runList$beta2.list[[20]](seq(0,5,by=0.25))

P=outMat[,2]
V=outMat[,3]
I=outMat[,4]
L = outMat[,5]

par(mfrow=c(1,2))
plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,1.25),ylim=c(0,1.25),
     xlab=expression(paste(beta)[1]), ylab=expression(paste(beta)[2]),
     xaxs="i", yaxs="i")

axis(side=1, at = seq(0,10,by=2))
axis(side=2, at = seq(0,10,by=2))

segments(x0=mParms[1],y0=0,y1=10,lty='dotted',lwd=2)
segments(x0=0,y0=mParms[2],x1=10,lty='dotted',lwd=2)

z = V/I
d = -(beta1/I)*P

library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(z))

for(i in 2:length(z)){
  points(x=beta1[i],y=beta2[i],pch=16,col=colors[i])
}

s <- seq(length(beta1)-1)  # one shorter than data
arrows(beta1[s], beta2[s], beta1[s+1], beta2[s+1], col= colors,length=.1)



plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,5),ylim=c(0,5),
     xlab=expression(paste(beta)[1]), ylab=expression(paste(beta)[2]),
     xaxs="i", yaxs="i")

axis(side=1, at = seq(0,10,by=2))
axis(side=2, at = seq(0,10,by=2))

segments(x0=mParms[1],y0=0,y1=10,lty='dotted',lwd=2)
segments(x0=0,y0=mParms[2],x1=10,lty='dotted',lwd=2)

library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(z))

for(i in 1:length(z)){
  if(I[i]==0) {
    abline(v=V[i]/P[i],col=colors[i])
  } else {
    abline(a= z[i], b = d[i],col=colors[i])
  }
}

for(i in 2:length(z)){
  points(x=beta1[i],y=beta2[i],pch=16,col=colors[i])
}

#V/I

par(mfrow=c(1,3))
plot(ut,beta1);abline(a=0,b=1)
plot(ut,beta2);abline(a=0,b=1)
plot(beta1,beta2);abline(a=0,b=1)



## TRAJECTORY

outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList$u.list[[20]],f2=runList$beta1.list[[20]],f3=runList$beta2.list[[20]]);

ut = runList$u.list[[20]](seq(0,5,by=0.1))
beta1 = runList$beta1.list[[20]](seq(0,5,by=0.1))
beta2 = runList$beta2.list[[20]](seq(0,5,by=0.1))

P=outMat[,2]
V=outMat[,3]
I=outMat[,4]
L = outMat[,5]

par(mfrow=c(1,2))
plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,1.25),ylim=c(0,1.25),
     xlab=expression(paste(beta)[1]), ylab=expression(paste(beta)[2]),
     xaxs="i", yaxs="i")

axis(side=1, at = seq(0,2,by=.5))
axis(side=2, at = seq(0,2,by=.5))

segments(x0=mParms[1],y0=0,y1=10,lty='dotted',lwd=2,col='gray')
segments(x0=0,y0=mParms[2],x1=10,lty='dotted',lwd=2,col='gray')

z = V/I
d = -(beta1/I)*P

library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(z))

# for(i in 2:length(z)){
#   points(x=beta1[i],y=beta2[i],pch=16,col=colors[i])
# }

s <- seq(length(beta1)-1)  # one shorter than data
arrows(beta1[s], beta2[s], beta1[s+1], beta2[s+1],length=.1,col=colors)


ut = plot(seq(0,5,by=0.1),runList$u.list[[20]](seq(0,5,by=0.1)),
          type='l',xlab='time',ylab='u(t)')

# Control variables

ut = runList$u.list[[20]](seq(0,5,by=0.01))
beta1 = runList$beta1.list[[20]](seq(0,5,by=0.01))
beta2 = runList$beta2.list[[20]](seq(0,5,by=0.01))

par(mfrow=c(3,1))
t = seq(0,5,by=0.01)
plot(t,ut,type='n',ylim=c(0,1));
abline(h=c(0,1),lty=2,col='red')
lines(t,ut,lwd=2)

plot(t,beta1,type='n',ylim=c(0,1));
abline(h=c(0,1),lty=2,col='red')
lines(t,beta1,lwd=2)

plot(t,beta2,type='n',ylim=c(0,1));
abline(h=c(0,1),lty=2,col='red')
lines(t,beta2,lwd=2)
