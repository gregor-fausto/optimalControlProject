
library(deSolve)
# what ode method to use?
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

outputVec <- list.files("~/Dropbox/optimalControlProject/output/")
#outputVec<-outputVec[c(grep("Meristem",outputVec),grep("Resource",outputVec))]
# outputVec<-outputVec[c(grep("Meristem",outputVec),grep("Resource",outputVec),
#                        grep("branchedDeterminate-uniform2.RDS",outputVec))]

outputVec<-outputVec[c(grep("uniform2.RDS",outputVec))]

outputVec <- c(outputVec[c(1,4,3,6,2,5)])

#outputVec<-outputVec[grep("unbranched",outputVec)]

n = length(outputVec)
runList <- list()

for(i in 1:n){
runList[[i]] <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec[i]))
}

derivs=numeric(6); 



par(mfrow=c(2,2))
library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(3, "YlOrRd"))(6)
colors2 <- colorRampPalette(brewer.pal(3, "Blues"))(6)

plot(seq(0,5,length.out=20),seq(0,3,length.out=20),
     type='n',
     xlab="Time",ylab="State",main="Primary meristems")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[20]],f2=runList[[i]]$beta1.list[[20]],f3=runList[[i]]$beta2.list[[20]]);
  lines(outMat[,1],outMat[,2],
        lty=ifelse(grepl("Resource",runList[[i]]$model),2,
                   ifelse(grepl("Meristem",runList[[i]]$model),3,1)),
        col=ifelse(grepl("unbranched",runList[[i]]$model),colors[i],colors2[i]))
  }

plot(seq(0,5,length.out=20),seq(0,10,length.out=20),
     type='n',
     xlab="Time",ylab="State",main="Vegetative biomass",
     ylim=c(0,6.5))
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[20]],f2=runList[[i]]$beta1.list[[20]],f3=runList[[i]]$beta2.list[[20]]);
  lines(outMat[,1],outMat[,3],
        lty=ifelse(grepl("Resource",runList[[i]]$model),2,
                   ifelse(grepl("Meristem",runList[[i]]$model),3,1)),
        col=ifelse(grepl("unbranched",runList[[i]]$model),colors[i],colors2[i]))
  }

plot(seq(0,5,length.out=20),seq(0,2,length.out=20),
     type='n',
     xlab="Time",ylab="State",main="Inflorescence meristems")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[20]],f2=runList[[i]]$beta1.list[[20]],f3=runList[[i]]$beta2.list[[20]]);
  lines(outMat[,1],outMat[,4],
        lty=ifelse(grepl("Resource",runList[[i]]$model),2,
                   ifelse(grepl("Meristem",runList[[i]]$model),3,1)),
        col=ifelse(grepl("unbranched",runList[[i]]$model),colors[i],colors2[i]))
  }


plot(seq(0,5,length.out=20),seq(0,3.5,length.out=20),
     type='n',
     xlab="Time",ylab="State",
     main="Floral meristems",
     ylim=c(0,4))
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[20]],f2=runList[[i]]$beta1.list[[20]],f3=runList[[i]]$beta2.list[[20]]);
  lines(outMat[,1],outMat[,5],
        lty=ifelse(grepl("Resource",runList[[i]]$model),2,
                   ifelse(grepl("Meristem",runList[[i]]$model),3,1)),
        col=ifelse(grepl("unbranched",runList[[i]]$model),colors[i],colors2[i]))
  }
legend(0,4,
       c("unbranched","branched","resource constraint","meristem constraint"),
       col=c(colors[1],colors2[6],1,1),
       lty=c(1,1,2,3),
       cex=.5)

print(runList$initsName)

