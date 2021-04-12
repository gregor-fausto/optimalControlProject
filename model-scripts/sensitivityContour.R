# I optimized fitness for a combination of initial conditions and constraints
# I then relaxed the resource constraint by taking alpha/.9
# I then relaxed the meristem constraint by taking m1/.9 and m2/.9

# Load libraries 
library(deSolve)
library(RColorBrewer)

# Set ODE method to use
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

# Read in list of files
outputVec <- list.files("model-scripts/analysisX")
outputVec <- outputVec[grep("5-0-0.75-0.5",outputVec)]

# Order files 
index=c(grep("relax",outputVec,invert=TRUE),grep("relaxAlpha.RDS",outputVec),grep("relaxMeristem.RDS",outputVec))

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

outMat.list = list()

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
  outMat.list[[i]] = outMat
}

data.frame(outMat.list[[2]])$L/data.frame(outMat.list[[1]])$L
data.frame(outMat.list[[3]])$L/data.frame(outMat.list[[1]])$L

