
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




# par(mfrow=c(2,2))
# library(RColorBrewer)
# colors <- colorRampPalette(brewer.pal(4, "PuRd"))(n)

ut <- list()
beta1 <- list()
beta2 <- list()

for(i in 1:length(runList)){

  ut[[i]] = runList[[i]]$u.list[[20]]
  beta1[[i]] = runList[[i]]$beta1.list[[20]]
  beta2[[i]] = runList[[i]]$beta2.list[[20]]
}

# Controls


par(mfrow=c(3,1))
t = seq(0,5,by=0.01)

plot(t,t,type='n',ylim=c(0,1),xlim=c(0,5),
     xlab="Time (t)",
     ylab=c(expression(paste(u,"(t)"))));
for(i in 1:length(ut)){
  abline(h=c(0,1),lty=1,col='gray')
  lines(t,ut[[i]](t),lwd=1,
        lty=ifelse(grepl("Resource",runList[[i]]$model),2,
                   ifelse(grepl("Meristem",runList[[i]]$model),3,1)),
        col=ifelse(grepl("unbranched",runList[[i]]$model),colors[i],colors2[i]))
}


plot(t,t,type='n',ylim=c(0,5),xlim=c(0,5),
     xlab="Time (t)",
     ylab=c(expression(paste(beta[1],"(t)"))));
abline(h=c(0,1),lty=1,col='gray')
for(i in 1:length(beta1)){
  lines(t,beta1[[i]](t),lwd=1,
        lty=ifelse(grepl("Resource",runList[[i]]$model),2,
                   ifelse(grepl("Meristem",runList[[i]]$model),3,1)),
        col=ifelse(grepl("unbranched",runList[[i]]$model),colors[i],colors2[i]))
}


plot(t,t,type='n',ylim=c(0,8),xlim=c(0,5),
     xlab="Time (t)",
     ylab=c(expression(paste(beta[2],"(t)"))));
abline(h=c(0,1),lty=1,col='gray')
for(i in 1:length(beta2)){
  lines(t,beta2[[i]](t),lwd=1,
        lty=ifelse(grepl("Resource",runList[[i]]$model),2,
                   ifelse(grepl("Meristem",runList[[i]]$model),3,1)),
        col=ifelse(grepl("unbranched",runList[[i]]$model),colors[i],colors2[i]))
}

legend(0,8,
       c("unbranched","branched","resource constraint","meristem constraint"),
       col=c(colors[1],colors2[6],1,1),
       lty=c(1,1,2,3),
       cex=.6)

