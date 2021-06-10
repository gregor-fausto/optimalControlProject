# I optimized fitness for a combination of initial conditions and constraints
# I then relaxed the resource constraint by taking alpha/.9
# I then relaxed the meristem constraint by taking m1/.9 and m2/.9

# Load libraries 
library(deSolve)
library(RColorBrewer)

# Set ODE method to use
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

# Read in list of files
outputVec <- list.files("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/analysisOne/")
outputVec <- outputVec[grep("60-0-1-1",outputVec)]

seasonEnd=60

# Order files 
index=c(grep("relax",outputVec,invert=TRUE),grep("relaxAlpha.RDS",outputVec),grep("relaxMeristem.RDS",outputVec))

# Number of files
n = length(outputVec)

# Create list of outputs
runList <- list()
for(i in 1:n){
  runList[[i]] <- readRDS(paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/analysisOne/",outputVec[index[i]]))
}

# empty vector of derivatives
derivs=numeric(6); 

# number of grid points
j = length(runList[[1]]$beta1.list)

# Plotting parameters
par(mfrow=c(2,2))

# Axis limits
ylim.p = c(0,3)
xlim.p = c(0,60)

# Colors
# Gray: baseline
# Red: relax alpha, resource constraint
# Purple: relax m1, meristem constraint
# orange: relax both constraints simultaneously
colors <- c("gray75","red","purple")

# Vegetative meristem trajectory
plot(NA,NA,xlim=xlim.p,ylim=ylim.p,
     type='n',
     xlab="Time",ylab="State",main="Vegetative meristems")
for(i in 1:n){
  source(paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/models/control-determinate.R"))
  derivs=numeric(6); 
  
  tmp=runList[[i]]
  inits=tmp$inits[1:4]
  mParms=tmp$inits[5:8]
  other=c(pen=0,obj=0)
  dist = c(distribution=as.character(tmp$seasonDistribution))
  seasonParms = c(max=tmp$max,min=tmp$min)
  
  outMat = ode(y=c(inits,other),times=seq(0,60,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,2],
        lty=i, lwd = 2,
        col=colors[i])
}

# Leaf number trajectory
plot(NA,NA,xlim=xlim.p,ylim=ylim.p*2,
     type='n',
     xlab="Time",ylab="State",main="Leaf number")
for(i in 1:n){
  source(paste0("model-scripts/models/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  
  tmp=runList[[i]]

  inits=tmp$inits[1:4]
  mParms=tmp$inits[5:8]
  other=c(pen=0,obj=0)
  dist = c(distribution=as.character(tmp$seasonDistribution))
  seasonParms = c(max=tmp$max,min=tmp$min)
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,60,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,3],
        lty=i, lwd = 2,
        col=colors[i])
}

# Inflorescence meristem trajectory
plot(NA,NA,xlim=xlim.p,ylim=ylim.p,
     type='n',
     xlab="Time",ylab="State",main="Inflorescence meristems")
for(i in 1:n){
  source(paste0("model-scripts/models/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  
  tmp=runList[[i]]
  
  inits=tmp$inits[1:4]
  mParms=tmp$inits[5:8]
  other=c(pen=0,obj=0)
  dist = c(distribution=as.character(tmp$seasonDistribution))
  seasonParms = c(max=tmp$max,min=tmp$min)
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,60,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,4],
        lty=i,  lwd = 2,
        col=colors[i])
}
# legend(x=0,y=1,c("Baseline","Relax alpha", "Relax m", "Relax alpha & m"),
#        lty = c(1:4),col=colors,cex=.5)



# Flower number trajectory
plot(NA,NA,xlim=xlim.p,ylim=ylim.p,
     type='n',
     xlab="Time",ylab="State",
     main="Flower numbers")

for(i in 1:n){
  source(paste0("model-scripts/models/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  
  tmp=runList[[i]]
  
  inits=tmp$inits[1:4]
  mParms=tmp$inits[5:8]
  other=c(pen=0,obj=0)
  dist = c(distribution=as.character(tmp$seasonDistribution))
  seasonParms = c(max=tmp$max,min=tmp$min)
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,60,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,5],
        lty=i,  lwd = 2,
        col=colors[i])
}

abline(v=30)
