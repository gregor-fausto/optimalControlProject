# I optimized fitness for a combination of initial conditions and constraints
# I then relaxed the resource constraint by taking alpha/.9
# I then relaxed the meristem constraint by taking m1/.9 and m2/.9
# I then relaxed both the meristem and resource constraint by relaxing both by 10%

# Load libraries 
library(deSolve)
library(RColorBrewer)

# Set ODE method to use
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

# Read in list of files
outputVec <- list.files("model-scripts/analysisX")

# Order files 
index=c(grep("relax",outputVec,invert=TRUE),grep("relaxAlpha.RDS",outputVec),grep("relaxMeristem.RDS",outputVec),
        grep("relaxAlphaMeristem",outputVec))

# Number of files
n = length(outputVec)

# Create list of outputs
runList <- list()
for(i in 1:n){
  runList[[i]] <- readRDS(paste0("model-scripts/analysisX/",outputVec[index[i]]))
}

# empty vector of derivatives
derivs=numeric(6); 

# number of grid points
j = length(runList[[1]]$beta1.list)

# Plotting parameters
par(mfrow=c(2,2))

# Axis limits
ylim.p = c(0,2)
xlim.p = c(0,5)

# Colors
# yellow: baseline
# red: relax alpha, resource constraint
# green: relax m1, meristem constraint
# orange: relax both constraints simultaneously
colors <- c("#ffffbf","#fc8d59","#91bfdb","#fdae61")

# Primary meristem trajectory
plot(NA,NA,xlim=xlim.p,ylim=ylim.p,
     type='n',
     xlab="Time",ylab="State",main="Primary meristems")
for(i in 1:n){
  source(paste0("model-scripts/models/control-",runList[[i]]$model,".R"))
  derivs=numeric(6); 
  
  tmp=runList[[i]]
  inits=tmp$inits[1:4]
  mParms=tmp$inits[5:8]
  other=c(pen=0,obj=0)
  dist = c(distribution=as.character(tmp$seasonDistribution))
  seasonParms = c(max=tmp$max,min=tmp$min)
  
  outMat = ode(y=c(inits,other),times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,2],
        lty=i, lwd = 2,
        col=colors[i])
}

# Vegetative biomass trajectory
plot(NA,NA,xlim=xlim.p,ylim=ylim.p,
     type='n',
     xlab="Time",ylab="State",main="Vegetative biomass")
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
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
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
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,4],
        lty=i,  lwd = 2,
        col=colors[i])
}

# Floral meristem trajectory
plot(NA,NA,xlim=xlim.p,ylim=ylim.p,
     type='n',
     xlab="Time",ylab="State",
     main="Floral meristems")

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
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=runList[[i]]$u.list[[j]],f2=runList[[i]]$beta1.list[[j]],f3=runList[[i]]$beta2.list[[j]]);
  lines(outMat[,1],outMat[,5],
        lty=i,  lwd = 2,
        col=colors[i])
}

