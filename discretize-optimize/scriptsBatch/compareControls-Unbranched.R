
library(deSolve)
# what ode method to use?
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

outputVec <- list.files("~/Dropbox/optimalControlProject/output/")

outputVec<-outputVec[c(grep("unbranchedDeterminate-uniform-1-",outputVec))]

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

j = length(runList[[2]]$beta1.list)

ut <- list()
beta1 <- list()
beta2 <- list()

for(i in 1:length(runList)){

  ut[[i]] = runList[[i]]$u.list[[j]]
  beta1[[i]] = runList[[i]]$beta1.list[[j]]
  beta2[[i]] = runList[[i]]$beta2.list[[j]]
}

# Controls

library(RColorBrewer)
colors <- colorRampPalette(c("#E2F614","#821C82"))(n)

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
legend(0,.98,
       refMatSorted$alpha,
       col=colors,
       lty=rep(1,length(refMatSorted$alpha)),
       cex=.5)


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



## VARIATION IN MERISTEM CONSTRAINT



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
refMatSorted=refMat[order(refMat$m),][-1,]
n=n-1
outputVec <- c(outputVec[refMatSorted$df])

runList <- list()

for(i in 1:n){
  runList[[i]] <- readRDS(paste0("~/Dropbox/optimalControlProject/output/",outputVec[i]))
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

# Controls

library(RColorBrewer)
colors <- colorRampPalette(c("green","#542788"))(n)

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
legend(0,.98,
       refMatSorted$alpha,
       col=colors,
       lty=rep(1,length(refMatSorted$alpha)),
       cex=.5)


plot(t,t,type='n',ylim=c(0,2.1),xlim=c(0,5),
     xlab="Time (t)",
     ylab=c(expression(paste(beta[1],"(t)"))));
abline(h=c(0,refMatSorted$m),lty=1,col='gray')
for(i in 1:length(beta1)){
  lines(t,beta1[[i]](t),lwd=1,
        col=colors[i])
}


plot(t,t,type='n',ylim=c(0,2.1),xlim=c(0,5),
     xlab="Time (t)",
     ylab=c(expression(paste(beta[2],"(t)"))));
abline(h=c(0,refMatSorted$m),lty=1,col='gray')
for(i in 1:length(beta2)){
  lines(t,beta2[[i]](t),lwd=1,
        lty=ifelse(grepl("Resource",runList[[i]]$model),2,
                   ifelse(grepl("Meristem",runList[[i]]$model),3,1)),
        col=colors[i])
}


