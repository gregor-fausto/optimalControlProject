
library(deSolve)
# what ode method to use?
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

outputVec <- list.files("~/Dropbox/optimalControlProject/output/")

outputVec<-outputVec[c(grep("unbranchedDeterminateResource-uniform-1",outputVec))]

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

j = length(runList[[2]]$beta1.list)


par(mfrow=c(2,2))
library(RColorBrewer)
#colors <- colorRampPalette(brewer.pal(6, "OrRd"))(n)
#colors2 <- colorRampPalette(brewer.pal(3, "Blues"))(n)
colors <- colorRampPalette(c("#E2F614","#821C82"))(n)


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
        col=colors[i])
  }

plot(seq(0,5,length.out=20),seq(0,1,length.out=20),
     type='n',
     xlab="Time",ylab="State",main="Vegetative biomass",
     ylim=c(0,1.25))
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,3],
        col=colors[i])
  }

plot(seq(0,5,length.out=20),seq(0,.5,length.out=20),
     type='n',
     xlab="Time",ylab="State",main="Inflorescence meristems")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,4],
        col=colors[i])
  }
legend(4,.5,
       refMatSorted$alpha,
       col=colors,
       lty=rep(1,length(refMatSorted$alpha)),
       cex=.5)

plot(seq(0,5,length.out=20),seq(0,3.5,length.out=20),
     type='n',
     xlab="Time",ylab="State",
     main="Floral meristems",
     ylim=c(0,1))
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,5],
        col=colors[i])
  }


print(runList$initsName)

