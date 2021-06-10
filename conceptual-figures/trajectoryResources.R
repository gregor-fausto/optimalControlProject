
par(mfrow=c(1,1))
library(deSolve)
# what ode method to use?
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

outputVec <- list.files("~/Dropbox/optimalControlProject/output/")
outputVec<-outputVec[grep("Resource",outputVec)][5]


runList <- list()

runList <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec))

derivs=numeric(6); 



# par(mfrow=c(2,2))
# library(RColorBrewer)
# colors <- colorRampPalette(brewer.pal(4, "PuRd"))(n)

i=1
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList$initsName,".R"))
  

  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=.5),control,method=odemethod,parms=mParms,f1=runList$u.list[[20]],f2=runList$beta1.list[[20]],f3=runList$beta2.list[[20]]);
  
  beta1 = runList$beta1.list[[20]](seq(0,5,by=0.5))
  beta2 = runList$beta2.list[[20]](seq(0,5,by=0.5))
  
  P=outMat[,2]
  V=outMat[,3]
  I=outMat[,4]
  L=outMat[,5]
  par(mfrow=c(1,2))
  plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,10),ylim=c(0,10),
       xlab=expression(paste(beta)[1]), ylab=expression(paste(beta)[2]),
       xaxs="i", yaxs="i")
  
  axis(side=1, at = seq(0,10,by=2))
  axis(side=2, at = seq(0,10,by=2))
  
  z = V/I
  d = -(beta1/I)*P
  
  library(RColorBrewer)
  colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(z))
  
  for(i in 1:length(z)){
    if(I[i]==0) {
           abline(v=V[i]/P[i],col=colors[i])
    } else {
           abline(a= z[i], b = d[i],col=colors[i])
    }
  }
  
  for(i in 1:length(z)){
  points(x=beta1[i],y=beta2[i],pch=16,col=colors[i])
  }

    s <- seq(length(beta1)-1)  # one shorter than data
  arrows(beta1[s], beta2[s], beta1[s+1], beta2[s+1], col= colors,length=.1)
  
  
  plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,2),ylim=c(0,2),
       xlab=expression(paste(beta)[1]), ylab=expression(paste(beta)[2]),
       xaxs="i", yaxs="i")
  
  axis(side=1, at = seq(0,10,by=2))
  axis(side=2, at = seq(0,10,by=2))
  
 #  segments(x0=mParms[1],y0=0,y1=10,lty='dotted',lwd=2)
  # segments(x0=0,y0=mParms[2],x1=10,lty='dotted',lwd=2)
  # 
  z = V/I
  d = -(beta1/I)*P
  
  library(RColorBrewer)
  colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(z))
  
  for(i in 1:length(z)){
    if(I[i]==0) {
      abline(v=V[i]/P[i],col=colors[i])
    } else {
      abline(a= z[i], b = d[i],col=colors[i])
    }
  }
  
  for(i in 1:length(z)){
    points(x=beta1[i],y=beta2[i],pch=16,col=colors[i])
  }
  
  s <- seq(length(beta1)-1)  # one shorter than data
  arrows(beta1[s], beta2[s], beta1[s+1], beta2[s+1], col=colors,length = .1)
  
  V/I
  
  par(mfrow=c(2,5))
  for(i in 2:length(z)){
    lim=z[i]+2
    plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,lim),ylim=c(0,lim),
         xlab=expression(paste(beta)[1]), ylab=expression(paste(beta)[2]),
         xaxs="i", yaxs="i",
         main=(seq(0,5,by=0.5)[i]))
    
    axis(side=1, at = seq(0,lim,by=2))
    axis(side=2, at = seq(0,lim,by=2))
 
    if(I[i-1]==0) {
      abline(v=V[i-1]/P[i-1],col="gray")
    } else {
      abline(a= z[i-1], b = d[i-1],col="gray")
    }
    
    if(I[i]==0) {
      abline(v=V[i]/P[i],col="black",lwd=2)
    } else {
      abline(a= z[i], b = d[i],col="black",lwd=2)
    }
    
    #abline(a= z[i-1], b = d[i-1],lwd=1)
    
      #abline(a= z[i], b = d[i],lwd=2)
    
      points(x=beta1[i],y=beta2[i],pch=16,col="black")
      
      arrows(beta1[i], beta2[i], beta1[i+1], beta2[i+1], col= "black",length=.1) 
      
    }

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
  plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,5),ylim=c(0,5),
       xlab=expression(paste(beta)[1]), ylab=expression(paste(beta)[2]),
       xaxs="i", yaxs="i")
  
  axis(side=1, at = seq(0,10,by=.5))
  axis(side=2, at = seq(0,10,by=.5))
  
  segments(x0=mParms[1],y0=0,y1=10,lty='dotted',lwd=2,col='gray')
  
  segments(x0=0,y0=mParms[2],x1=10,lty='dotted',lwd=2,col='gray')
  
  z = V/I
  d = -(beta1/I)*P
  
  # library(RColorBrewer)
  # colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(z))
  
  # for(i in 2:length(z)){
  #   points(x=beta1[i],y=beta2[i],pch=16,col=colors[i])
  # }
  
  s <- seq(length(beta1)-1)  # one shorter than data
  arrows(beta1[s], beta2[s], beta1[s+1], beta2[s+1],length=.1)
  
 plot(seq(0,5,by=0.1),runList$u.list[[20]](seq(0,5,by=0.1)),
            type='l',xlab='time',ylab='u(t)')
  
 par(mfrow=c(1,3))
 plot(ut,beta1);abline(a=0,b=1)
 plot(ut,beta2);abline(a=0,b=1)
 plot(beta1,beta2);abline(a=0,b=1)
 