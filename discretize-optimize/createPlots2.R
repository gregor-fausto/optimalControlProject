library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()
colors=brewer.pal(5,"PuRd")
colors=rep(colors,each=2)
colors = c("#003f5c","#444e86","#955196","#dd5182","#ff6e54","#ffa600")

# go to the directory with all the run outputs
directory = "~/Dropbox/optimalControlProject/discretize-optimize/runs-old/Psetto10,uniform(2,5)/"

# get a vector of each folder
outputFolders <- paste0(directory,list.files(directory))

# for each folder, create a vector of the filenames in the folder
outputFiles <- lapply(outputFolders,list.files,full.names=TRUE)

fxFiles = c()

outputFiles = unlist(outputFiles)
fxIndex = grep(pattern = "fx", outputFiles)

fxFiles = outputFiles[fxIndex]
trajectoryFiles = outputFiles[-fxIndex]

unbranchedIndex = grep(pattern = "unbranched", trajectoryFiles)

unbranchedFiles = trajectoryFiles[unbranchedIndex]
branchedFiles = trajectoryFiles[-unbranchedIndex]

#trajectoryDF <- lapply(trajectoryFiles, readRDS)
 unbranchedDF <- lapply(unbranchedFiles, readRDS)
branchedDF <- lapply(branchedFiles, readRDS)

n = length(unbranchedFiles)

par(mfrow=c(3,2))
for(i in 1:n){
  plot(seq(0,5,length=100),seq(0,10,length=100),type='n')
  lines(unbranchedDF[[i]]$time,unbranchedDF[[i]]$P,col=colors[i],lty=1)
  lines(branchedDF[[i]]$time,branchedDF[[i]]$P,col=colors[i],lty=3)
}

par(mfrow=c(3,2))
for(i in 1:n){
  plot(seq(0,5,length=100),seq(0,20,length=100),type='n')
  lines(unbranchedDF[[i]]$time,unbranchedDF[[i]]$V,col=colors[i],lty=1)
  lines(branchedDF[[i]]$time,branchedDF[[i]]$V,col=colors[i],lty=3)
  }

par(mfrow=c(3,2))
for(i in 1:n){
  plot(seq(0,5,length=100),seq(0,5,length=100),type='n')
  lines(unbranchedDF[[i]]$time,unbranchedDF[[i]]$I,col=colors[i],lty=1)
  lines(branchedDF[[i]]$time,branchedDF[[i]]$I,col=colors[i],lty=3)
  }

par(mfrow=c(3,2))
for(i in 1:n){
  plot(seq(0,5,length=100),seq(0,10,length=100),type='n')
  lines(unbranchedDF[[i]]$time,unbranchedDF[[i]]$L,col=colors[i],lty=1)
  lines(branchedDF[[i]]$time,branchedDF[[i]]$L,col=colors[i],lty=3)
  }


a=sub("-1.RDS", "", unbranchedFiles)
b=sub("m2", "", a)
c=strsplit(b, '-')
t=as.numeric(sapply(c, `[`, 5))

par(mfrow=c(1,1))
plot(seq(0,1,length=100),seq(0,10,length=100),type='n',
     xlab="beta_1",ylab="Fitness at t=5")
for(i in 1:n){
  points(t[i],max(unbranchedDF[[i]]$L),col=colors[i],pch=16)
  points(t[i],max(branchedDF[[i]]$L),col=colors[i],pch=1)
}


unbranchedIndex = grep(pattern = "unbranched", fxFiles)

unbranchedFiles = fxFiles[unbranchedIndex]
branchedFiles = fxFiles[-unbranchedIndex]

unbranchedFx <- lapply(unbranchedFiles, readRDS)
branchedFx <- lapply(branchedFiles, readRDS)

uListUB=sapply(unbranchedFx, `[`, 1)
beta1ListUB=sapply(unbranchedFx, `[`, 2)
beta2ListUB=sapply(unbranchedFx, `[`, 3)

uListB=sapply(branchedFx, `[`, 1)
beta1ListB=sapply(branchedFx, `[`, 2)
beta2ListB=sapply(branchedFx, `[`, 3)

## u(t)
par(mfrow=c(2,3))
for(i in 1:n){
  plot(seq(0,5,length=100),seq(0,1,length=100),type='n',ylab="u(t)")
  lines(seq(0,5,by=0.1),uListUB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=1)
  lines(seq(0,5,by=0.1),uListB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=2)
}

par(mfrow=c(2,1))
plot(seq(0,5,length=100),seq(0,1,length=100),type='n',ylab="u(t)")
for(i in 1:n){
  lines(seq(0,5,by=0.1),uListUB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=1)
}

plot(seq(0,5,length=100),seq(0,1,length=100),type='n',ylab="u(t)")
for(i in 1:n){
  lines(seq(0,5,by=0.1),uListB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=2)
}

## beta1
par(mfrow=c(2,3))
for(i in 1:n){
  plot(seq(0,5,length=100),seq(0,1,length=100),type='n',ylab="beta1(t)")
  lines(seq(0,5,by=0.1),beta1ListUB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=1)
  lines(seq(0,5,by=0.1),beta1ListB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=2)
}

par(mfrow=c(2,1))
plot(seq(0,5,length=100),seq(0,1,length=100),type='n',ylab="beta1(t)")
for(i in 1:n){
  lines(seq(0,5,by=0.1),beta1ListUB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=1)
}

plot(seq(0,5,length=100),seq(0,1,length=100),type='n',ylab="beta1(t)")
for(i in 1:n){
  lines(seq(0,5,by=0.1),beta1ListB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=2)
}

# beta2list
par(mfrow=c(2,3))
for(i in 1:n){
  plot(seq(0,5,length=100),seq(0,1,length=100),type='n',ylab="beta2(t)")
  lines(seq(0,5,by=0.1),beta2ListUB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=1)
  lines(seq(0,5,by=0.1),beta2ListB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=2)
}

par(mfrow=c(2,1))
plot(seq(0,5,length=100),seq(0,1,length=100),type='n',ylab="beta2(t)")
for(i in 1:n){
  lines(seq(0,5,by=0.1),beta2ListUB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=1)
}

plot(seq(0,5,length=100),seq(0,1,length=100),type='n',ylab="beta2(t)")
for(i in 1:n){
  lines(seq(0,5,by=0.1),beta2ListB[[i]](seq(0,5,by=0.1)),col=colors[i],lty=2)
}
