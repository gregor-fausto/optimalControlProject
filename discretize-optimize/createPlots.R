library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()
colors=brewer.pal(5,"PuRd")
colors=rep(colors,each=2)


library(RColorBrewer)

directory = "~/Dropbox/optimalControlProject/discretize-optimize/runs/"
outputFolders <- paste0(directory,list.files(directory))[3]

outputFiles = list.files(outputFolders,full.names = TRUE)

fxIndex<-grep("fx",outputFiles)

fxFiles = outputFiles[fxIndex]
trajectoryFiles = outputFiles[-fxIndex]

n = length(fxFiles)

trajectoryDF <- lapply(trajectoryFiles, readRDS)

par(mfrow=c(2,2))
plot(seq(0,5,length=100),seq(0,1,length=100),type='n')
for(i in 1:n){
  lines(trajectoryDF[[i]]$time,trajectoryDF[[i]]$P,col=colors[i])
}

plot(seq(0,5,length=100),seq(0,1,length=100),type='n')
for(i in 1:n){
  lines(trajectoryDF[[i]]$time,trajectoryDF[[i]]$V,col=colors[i])
}

plot(seq(0,5,length=100),seq(0,1,length=100),type='n')
for(i in 1:n){
  lines(trajectoryDF[[i]]$time,trajectoryDF[[i]]$I,col=colors[i])
}

plot(seq(0,5,length=100),seq(0,1,length=100),type='n')
for(i in 1:n){
  lines(trajectoryDF[[i]]$time,trajectoryDF[[i]]$L,col=colors[i])
}

a=sub("-1.RDS", "", trajectoryFiles)
b=sub("m2", "", a)
c=strsplit(b, '-')
t=as.numeric(sapply(c, `[`, 4))

par(mfrow=c(1,1))
plot(seq(0,1,length=100),seq(0,1,length=100),type='n',
     xlab="beta_1",ylab="Fitness at t=5")
for(i in 1:n){
  points(t[i],max(trajectoryDF[[i]]$L),col=colors[i],pch=16)
}


fxList <- lapply(fxFiles, readRDS)
uList=sapply(fxList, `[`, 1)
beta1List=sapply(fxList, `[`, 2)
beta2List=sapply(fxList, `[`, 3)

par(mfrow=c(1,3))
plot(seq(0,5,length=100),seq(0,1,length=100),type='n')
for(i in 1:n){
  lines(seq(0,5,by=0.1),uList[[i]](seq(0,5,by=0.1)),col=colors[i],pch=16)
}

plot(seq(0,5,length=100),seq(0,1,length=100),type='n')
abline(h=seq(.1,1,by=.1),lty='dotted',col='gray')
for(i in 1:n){
  lines(seq(0,5,by=0.1),beta1List[[i]](seq(0,5,by=0.1)),col=colors[i],pch=16)
}

plot(seq(0,5,length=100),seq(0,1,length=100),type='n')
for(i in 1:n){
  lines(seq(0,5,by=0.1),beta2List[[i]](seq(0,5,by=0.1)),col=colors[i],pch=16)
}


