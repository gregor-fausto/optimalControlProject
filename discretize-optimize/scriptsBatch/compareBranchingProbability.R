######################
# Load packages
######################
library(deSolve)
library(RColorBrewer)
library(tidyverse)
######################
# Identify settings
######################
# what ode method to use?
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

######################
# Write function to get state variable trajectory
######################

state <- function(development="unbranchedDeterminate",m=1,alpha=1){
  
  fileName <- list.files("~/Dropbox/optimalControlProject/output/")
  fileName <- fileName[c(grep(paste0("unbranchedDeterminate","-uniform-",m,"-",alpha),fileName))]
  
  tmp <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec))
  n = length(tmp$u.list)
  
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",tmp$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",tmp$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=tmp$u.list[[n]],f2=tmp$beta1.list[[n]],f3=tmp$beta2.list[[n]]);
  return(outMat)
}

######################
# Get combination of development, meristem constraint, resource constraint
######################

files <- list.files("~/Dropbox/optimalControlProject/output/")
files<-files[c(grep("branchedDeterminate-uniform-",files))]

n = length(files)
model = c()
alpha = c()
m = c()

for(i in 1:n){
  tmp <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",files[i]))
  parms = tmp$inits[names(tmp$inits) %in% c("m1","m2","alpha")]
  model[i] = tmp$model
  alpha[i] = parms["alpha"]
  m[i] = parms["m1"]
}

df = data.frame(model=model,m=m,alpha=alpha)

state(development=df$model[1],df$m[1],df$alpha[1])

ggplot(df) +
  geom_point(aes(x=alpha,y=m)) +
  facet_wrap(~model) +
  theme_bw()

######################
# Get combination of development, meristem constraint, resource constraint
######################


refMat=data.frame(cbind(df=1:n,alpha=alphaValue))
refMatSorted=refMat[order(refMat$alpha),]
outputVec <- c(outputVec[refMatSorted$df])

runList <- list()

for(i in 1:n){
  runList[[i]] <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec[i]))
}

derivs=numeric(6); 
j = length(runList[[1]]$beta1.list)



# branched ----------------------------------------------------------------
outputVec2 <- list.files("~/Dropbox/optimalControlProject/output/")

outputVec2<-outputVec2[c(grep("branchedDeterminate-uniform-1-",outputVec2))]
outputVec2<-outputVec2[!grepl("unbranched",outputVec2)]

n = length(outputVec2)

alphaValue = c()

for(i in 1:n){
  tmp <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec2[i]))
  parms = tmp$inits[names(tmp$inits) %in% c("m1","m2","alpha")]
  alphaValue[i] = parms["alpha"]
}

refMat=data.frame(cbind(df=1:n,alpha=alphaValue))
refMatSorted=refMat[order(refMat$alpha),]
outputVec2 <- c(outputVec2[refMatSorted$df])

runList2<-list()
for(i in 1:n){
  runList2[[i]] <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec2[i]))
}

# Meristem constraint -----------------------------------------------------

outputVec3 <- list.files("~/Dropbox/optimalControlProject/output/")

outputVec3<-outputVec3[c(grep("unbranchedDeterminate-uniform-",outputVec3))]
outputVec3<-outputVec3[c(grep("-1.RDS",outputVec3))] 

n2 = length(outputVec3)
mValue = c()

for(i in 1:n2){
  tmp <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec3[i]))
  parms = tmp$inits[names(tmp$inits) %in% c("m1","m2","alpha")]
  mValue[i] = parms["m1"]
}

refMat=data.frame(cbind(df=1:n2,m=mValue))

# check when fit finishes
refMatSorted2=refMat[order(refMat$m),]
outputVec3 <- c(outputVec3[refMatSorted2$df])
runList3 <- list()
for(i in 1:n2){
  runList3[[i]] <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec3[i]))
}


# branched 2 --------------------------------------------------------------


outputVec4 <- list.files("~/Dropbox/optimalControlProject/output/")

outputVec4<-outputVec4[c(grep("branchedDeterminate-uniform-",outputVec4))]
outputVec4<-outputVec4[!grepl("unbranched",outputVec4)]
outputVec4<-outputVec4[c(grep("-1.RDS",outputVec4))]

n2 = length(outputVec4)
mValue = c()

for(i in 1:n2){
  tmp <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec4[i]))
  parms = tmp$inits[names(tmp$inits) %in% c("m1","m2","alpha")]
  mValue[i] = parms["m1"]
}

refMat=data.frame(cbind(df=1:n2,m=mValue))
refMatSorted2=refMat[order(refMat$m),]
outputVec4 <- c(outputVec4[refMatSorted2$df])

runList4 <- list()
for(i in 1:n2){
  runList4[[i]] <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec4[i]))
}

colors <- colorRampPalette(c("yellow","#821C82"))(n)

par(mfrow=c(1,2))
plot(seq(0,1,length.out=20),seq(0,3,length.out=20),
     type='n',
     xlab="Branching probability",ylab="End-of-season fitness",
     main="Floral meristems")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  points(0,outMat[51,5],
         pch=16,
         col=colors[i])
  
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList2[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList2[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat2 = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList2[[i]]$u.list[[j]],f2=runList2[[i]]$beta1.list[[j]],f3=runList2[[i]]$beta2.list[[j]]);
  points(1,outMat2[51,5],
        pch=16,
        col=colors[i])
  
  segments(x0=0,x1=1,y0=outMat[51,5],y1=outMat2[51,5],col=colors[i])

}
legend(0,3,
       legend=refMatSorted$alpha,
       col=colors,
       lty=1,
       cex=.5,
       title="Resource constraint")



colors <- colorRampPalette(c("yellow","#821C82"))(n2)


plot(seq(0,1,length.out=20),seq(0,3,length.out=20),
     type='n',
     xlab="Branching probability",ylab="End-of-season fitness",
     main="Floral meristems")
for(i in 1:n2){
source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList3[[i]]$model,".R"))
derivs=numeric(6);
source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList3[[i]]$initsName,".R"))

initVals = c(inits,other)
outMat3 = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList3[[i]]$u.list[[j]],f2=runList3[[i]]$beta1.list[[j]],f3=runList3[[i]]$beta2.list[[j]]);
points(0,outMat3[51,5],
       pch=16,
       col=colors[i])

source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList4[[i]]$model,".R"))
derivs=numeric(6);
source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList4[[i]]$initsName,".R"))

initVals = c(inits,other)
outMat4 = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList4[[i]]$u.list[[j]],f2=runList4[[i]]$beta1.list[[j]],f3=runList4[[i]]$beta2.list[[j]]);
points(1,outMat4[51,5],
       pch=16,
       col=colors[i])

segments(x0=0,x1=1,y0=outMat3[51,5],y1=outMat4[51,5],col=colors[i])

}

legend(0,3,
       legend=refMatSorted2$m,
       col=colors,
       lty=1,
       cex=.5,
       title="Meristem constraint")



par(mfrow=c(1,2))
plot(seq(0,2.5,length.out=20),seq(0,1.5,length.out=20),
     type='n',
     xlab="Meristem constraint",ylab="End-of-season fitness",
     main="")
for(i in 1:n2){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList3[[i]]$model,".R"))
  derivs=numeric(6);
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList3[[i]]$initsName,".R"))
  
  initVals = c(inits,other)
  outMat3 = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList3[[i]]$u.list[[j]],f2=runList3[[i]]$beta1.list[[j]],f3=runList3[[i]]$beta2.list[[j]]);
  points(refMatSorted2$m[i],outMat3[51,5],
         pch=1,
         col=colors[i])
  
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList4[[i]]$model,".R"))
  derivs=numeric(6);
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList4[[i]]$initsName,".R"))
  
  initVals = c(inits,other)
  outMat4 = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList4[[i]]$u.list[[j]],f2=runList4[[i]]$beta1.list[[j]],f3=runList4[[i]]$beta2.list[[j]]);
  points(refMatSorted2$m[i],outMat4[51,5],
         pch=16,
         col=colors[i])
}



plot(seq(0,2.5,length.out=20),seq(0,3,length.out=20),
     type='n',
     xlab="Resource constraint",ylab="End-of-season fitness",
     main="")
for(i in 1:n){
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  points(refMatSorted$alpha[i],outMat[51,5],
         pch=1,
         col=colors[i])
  
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",runList2[[i]]$model,".R"))
  derivs=numeric(6); 
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-",runList2[[i]]$initsName,".R"))
  
  initVals = c(inits,other) 
  outMat2 = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList2[[i]]$u.list[[j]],f2=runList2[[i]]$beta1.list[[j]],f3=runList2[[i]]$beta2.list[[j]]);
  points(refMatSorted$alpha[i],outMat2[51,5],
         pch=16,
         col=colors[i])
}

legend(0,3,
       legend=c(0,1),
       pch=c(1,16),
       cex=.5,
       title="Branching probability")
