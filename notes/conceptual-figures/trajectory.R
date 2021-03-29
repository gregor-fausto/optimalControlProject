
par(mfrow=c(1,1))
library(deSolve)
# what ode method to use?
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

outputVec <- list.files("~/Dropbox/optimalControlProject/output/")
outputVec<-outputVec[grep("uniform",outputVec)]
outputVec<-outputVec[grep("unbranched",outputVec)][1]

n = length(outputVec)
runList <- list()

for(i in 1:n){
  runList[[i]] <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec[i]))
}

derivs=numeric(6); 


# par(mfrow=c(2,2))
# library(RColorBrewer)
# colors <- colorRampPalette(brewer.pal(4, "PuRd"))(n)

i=1
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  

  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.5),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[20]],f2=runList[[i]]$beta1.list[[20]],f3=runList[[i]]$beta2.list[[20]]);
  
  beta1 = runList[[i]]$beta1.list[[20]](seq(0,5,by=0.5))
  beta2 = runList[[i]]$beta2.list[[20]](seq(0,5,by=0.5))
  
  V=outMat[,2]
  P=outMat[,3]
  I=outMat[,4]
  
  
  plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,2),ylim=c(0,2),
       xlab=expression(paste(beta)[1]), ylab=expression(paste(beta)[2]),
       xaxs="i", yaxs="i")
  
  axis(side=1, at = seq(0,10,by=2))
  axis(side=2, at = seq(0,10,by=2))
  
  segments(x0=mParms[1],y0=0,y1=10,lty='dotted',lwd=2)
  segments(x0=0,y0=mParms[2],x1=10,lty='dotted',lwd=2)
  
  z = V/beta2
  d = -beta1/beta2*P
  
  library(RColorBrewer)
  colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(z))
  
  for(i in 1:length(z)){
abline(a= z[i], b = d[i],col=colors[i])
  }
  
  for(i in 1:length(z)){
  points(x=beta1[i],y=beta2[i],pch=16,col=colors[i])
  }

    s <- seq(length(beta1)-1)  # one shorter than data
  arrows(beta1[s], beta2[s], beta1[s+1], beta2[s+1], col= colors)
  