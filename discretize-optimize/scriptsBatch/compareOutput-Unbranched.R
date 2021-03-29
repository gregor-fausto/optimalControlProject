
library(deSolve)
# what ode method to use?
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

outputVec <- list.files("~/Dropbox/optimalControlProject/output/")

outputVec<-outputVec[c(grep("unbranchedDeterminate-uniform-1-",outputVec))]
#outputVec<-outputVec[!grepl("unbranched",outputVec)]

n = length(outputVec)

alphaValue = c()

for(i in 1:n){
  tmp <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec[i]))
  parms = tmp$inits[names(tmp$inits) %in% c("m1","m2","alpha")]
  alphaValue[i] = parms["alpha"]
}

refMat=data.frame(cbind(df=1:n,alpha=alphaValue))
refMatSorted=refMat[order(refMat$alpha),]
outputVec <- c(outputVec[refMatSorted$df])

runList <- list()

for(i in 1:n){
  runList[[i]] <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec[i]))
}

derivs=numeric(6); 

j = length(runList[[1]]$beta1.list)


par(mfrow=c(2,2))
library(RColorBrewer)
colors <- colorRampPalette(c("yellow","#821C82"))(n)

plot(seq(0,5,length.out=20),seq(0,1,length.out=20),
     type='n',
     xlab="Time",ylab="State",main="Primary meristems")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,2],
        lty=i,
        col=colors[i])
}

plot(seq(0,5,length.out=20),seq(0,1.25,length.out=20),
     type='n',
     xlab="Time",ylab="State",main="Vegetative biomass")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,3],
        lty=i,
        col=colors[i])
}

plot(seq(0,5,length.out=20),seq(0,1,length.out=20),
     type='n',
     xlab="Time",ylab="State",main="Inflorescence meristems")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,4],
        lty=i,
        col=colors[i])
}
legend(0,1,
       refMatSorted$alpha,
       col=c(colors),
       lty=1:4,
       #lty=rep(1,length(refMat$alpha)),
       cex=.5)

plot(seq(0,5,length.out=20),seq(0,1,length.out=20),
     type='n',
     xlab="Time",ylab="State",
     main="Floral meristems")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,5],
        lty=i,
        col=colors[i])
}


## COMPARE CHANGING meristem constraint


outputVec <- list.files("~/Dropbox/optimalControlProject/output/")

outputVec<-outputVec[c(grep("unbranchedDeterminate-uniform-",outputVec))]
outputVec<-outputVec[c(grep("-1.RDS",outputVec))]

n = length(outputVec)
mValue = c()

for(i in 1:n){
  tmp <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec[i]))
  parms = tmp$inits[names(tmp$inits) %in% c("m1","m2","alpha")]
  mValue[i] = parms["m1"]
}

refMat=data.frame(cbind(df=1:n,m=mValue))

# check when fit finishes
refMatSorted=refMat[order(refMat$m),]#[-1,]
#n=n-1

outputVec <- c(outputVec[refMatSorted$df])

runList <- list()

for(i in 1:n){
  runList[[i]] <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec[i]))
}

derivs=numeric(6); 

j = length(runList[[1]]$beta1.list)


par(mfrow=c(2,2))
library(RColorBrewer)
colors <- colorRampPalette(c("green","#542788"))(n)

plot(seq(0,5,length.out=20),seq(0,1,length.out=20),
     type='n',
     xlab="Time",ylab="State",main="Primary meristems")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,2],
        lty=i,
        col=colors[i])
}

plot(seq(0,5,length.out=20),seq(0,1.25,length.out=20),
     type='n',
     xlab="Time",ylab="State",main="Vegetative biomass")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,3],
        lty=i,
        col=colors[i])
}

plot(seq(0,5,length.out=20),seq(0,1,length.out=20),
     type='n',
     xlab="Time",ylab="State",main="Inflorescence meristems")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,4],
        lty=i,
        col=colors[i])
}
legend(0,1,
       refMatSorted$m,
       col=c(colors),
       lty=1:4,
       #lty=rep(1,length(refMat$alpha)),
       cex=.5)

plot(seq(0,5,length.out=20),seq(0,1,length.out=20),
     type='n',
     xlab="Time",ylab="State",
     main="Floral meristems")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,5],
        lty=i,
        col=colors[i])
}
