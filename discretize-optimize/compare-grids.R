library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()
#colors=rep(colors,each=2)

directory = "~/Dropbox/optimalControlProject/discretize-optimize/output/"
#outputFiles <- paste0(directory,list.files(directory))[1:8]
outputFiles <- paste0(directory,list.files(directory))[c(3,7)]

fxIndex<-grep("fx",outputFiles)

fxFiles = outputFiles[fxIndex]
trajectoryFiles = outputFiles[-fxIndex]

#trajectoryDF <- lapply(trajectoryFiles, readRDS)
trajectoryDF <- readRDS(trajectoryFiles)

n = length(trajectoryDF)
colors=brewer.pal(n,"YlOrRd")


par(mfrow=c(1,1))
plot(seq(0,5,length=100),seq(.5,1,length=100),type='n')
for(i in 1:n){
  lines(trajectoryDF[[i]]$time,trajectoryDF[[i]]$P,col=colors[i])
}

plot(seq(0,5,length=100),seq(0,.5,length=100),type='n')
for(i in 1:n){
  lines(trajectoryDF[[i]]$time,trajectoryDF[[i]]$V,col=colors[i])
}

plot(seq(0,5,length=100),seq(0,.1,length=100),type='n')
for(i in 1:n){
  lines(trajectoryDF[[i]]$time,trajectoryDF[[i]]$I,col=colors[i])
}

plot(seq(0,5,length=100),seq(0,.5,length=100),type='n')
for(i in 1:n){
  lines(trajectoryDF[[i]]$time,trajectoryDF[[i]]$L,col=colors[i])
}

# a=sub("-1.RDS", "", trajectoryFiles)
# b=sub("m2", "", a)
# c=strsplit(b, '-')
# t=as.numeric(sapply(c, `[`, 4))

# fitness<-sapply(trajectoryDF, `[`, 5)
# time<-sapply(trajectoryDF, `[`, 1)
# 
# par(mfrow=c(1,1))
# plot(seq(0,1,length=100),seq(0,1,length=100),type='n',
#      xlab="beta_1",ylab="Fitness at t=5")
# for(i in 1:n){
#   points(t[i],max(trajectoryDF[[i]]$L),col=colors[i],pch=16)
# }


fxList <- readRDS(fxFiles)
uList=sapply(fxList, `[`, 1)
uList=list(fxList[[1]][[1]],fxList[[2]][[1]],fxList[[3]][[1]],fxList[[4]][[1]])

beta1List=sapply(fxList, `[`, 2)
beta2List=sapply(fxList, `[`, 3)

par(mfrow=c(1,1))
plot(seq(0,5,length=100),seq(0,1,length=100),type='n')
for(i in 1:n){
  lines(seq(0,5,by=0.1),uList[[i]](seq(0,5,by=0.1)),col=colors[i],pch=16)
}

plot(seq(0,5,length=100),seq(0,.25,length=100),type='n')
abline(h=seq(.1,1,by=.1),lty='dotted',col='gray')
for(i in 1:n){
  lines(seq(0,5,by=0.1),beta1List[[i]](seq(0,5,by=0.1)),col=colors[i],pch=16)
}

plot(seq(0,5,length=100),seq(0,1,length=100),type='n')
for(i in 1:n){
  lines(seq(0,5,by=0.1),beta2List[[i]](seq(0,5,by=0.1)),col=colors[i],pch=16)
}
# 
# par(mfrow=c(1,1))
# plot((seq(0,5,by=0.1)),beta1List[[1]](seq(0,5,by=0.1)),ylim=c(.05,.15),col=colors[1])
# points((seq(0,5,by=0.05)),beta1List[[1]](seq(0,5,by=0.05)),pch=16,cex=0.5,col=colors[1])
# 
# #par(mfrow=c(1,1))
# points((seq(0,5,by=0.1)),beta1List[[2]](seq(0,5,by=0.1)),col=colors[2])
# points((seq(0,5,by=0.05)),beta1List[[2]](seq(0,5,by=0.05)),pch=16,cex=0.5,col=colors[2])
# 
# points((seq(0,5,by=0.1)),beta1List[[3]](seq(0,5,by=0.1)),col=colors[3])
# points((seq(0,5,by=0.05)),beta1List[[3]](seq(0,5,by=0.05)),pch=16,cex=0.5,col=colors[3])
# 
# points((seq(0,5,by=0.1)),beta1List[[4]](seq(0,5,by=0.1)),col=colors[4])
# points((seq(0,5,by=0.05)),beta1List[[4]](seq(0,5,by=0.05)),pch=16,cex=0.5,col=colors[4])

