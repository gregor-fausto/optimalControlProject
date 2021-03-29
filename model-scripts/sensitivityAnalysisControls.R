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

j = length(runList[[2]]$beta1.list)

ut <- list()
beta1 <- list()
beta2 <- list()

for(i in 1:length(runList)){
  ut[[i]] = runList[[i]]$u.list[[j]]
  beta1[[i]] = runList[[i]]$beta1.list[[j]]
  beta2[[i]] = runList[[i]]$beta2.list[[j]]
}

# Colors
# yellow: baseline
# red: relax alpha, resource constraint
# green: relax m1, meristem constraint
# orange: relax both constraints simultaneously
colors <- c("#ffffbf","#fc8d59","#91bfdb","#fdae61")

par(mfrow=c(1,3))
t = seq(0,5,by=0.01)

plot(t,t,type='n',ylim=c(0,1),xlim=c(0,5),
     xlab="Time (t)",
     ylab=c(expression(paste(u,"(t)"))));
abline(h=c(0,1),lty=1,col='gray')
for(i in 1:length(ut)){
  lines(t,ut[[i]](t),lwd=1,
        col=colors[i])
}


plot(t,t,type='n',ylim=c(0,1.1),xlim=c(0,5),
     xlab="Time (t)",
     ylab=c(expression(paste(beta[1],"(t)"))));
abline(h=c(0),lty=1,col='gray')
for(i in 1:length(beta1)){
  lines(t,beta1[[i]](t),lwd=1,
        col=colors[i])
}


plot(t,t,type='n',ylim=c(0,1.1),xlim=c(0,5),
     xlab="Time (t)",
     ylab=c(expression(paste(beta[2],"(t)"))));
abline(h=c(0),lty=1,col='gray')
for(i in 1:length(beta2)){
  lines(t,beta2[[i]](t),lwd=1,
        lty=ifelse(grepl("Resource",runList[[i]]$model),2,
                   ifelse(grepl("Meristem",runList[[i]]$model),3,1)),
        col=colors[i])
}



