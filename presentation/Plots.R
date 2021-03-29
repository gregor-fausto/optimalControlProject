
rm(list=ls(all=TRUE)) # clear R environment
par()  

par(mfrow=c(1,1),mar=c(5,6,4,2)+.1)
plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,2),ylim=c(0,2),
     xlab=expression("Development rate"), ylab=expression("Age at first flower"),
     xaxs="i", yaxs="i",cex.lab=2)

axis(side=1, at = c(0,.3,1.8,2),labels=c("","slow","fast",""),cex.axis=1.25)
axis(side=2, at =  c(0,.3,1.8,2),labels=c("","young","old",""),cex.axis=1.2)

segments(x0=.2,y0=1.8,x1=1.8,y1=.2,lty=1,lwd=3)

## Plot 2
par(mfrow=c(1,1),mar=c(5,6,4,2)+.1)
plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,2),ylim=c(0,2),
     xlab=expression("Size at first flower"), ylab=expression("Age at first flower"),
     xaxs="i", yaxs="i",cex.lab=2)

axis(side=1, at = c(0,.3,1.8,2),labels=c("","small","large",""),cex.axis=1.25)
axis(side=2, at =  c(0,.3,1.8,2),labels=c("","young","old",""),cex.axis=1.2)

f <- function(tmp){
  
  return( rnorm(1,tmp,.2))
}
x = runif(20,.3,1.5)
points(x,unlist(lapply(x,f)),pch=16,cex=2)
#segments(x0=.2,y0=1.8,x1=1.8,y1=.2,lty=2,lwd=3)


# plot 3

par(mfrow=c(1,1),mar=c(5,6,4,2)+.1)
plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,1),ylim=c(0,1),
     xlab=expression("Meristem division rate"), ylab=expression("Fitness"),
     xaxs="i", yaxs="i",cex.lab=2)

axis(side=1, at = seq(0,1,.25),cex.axis=1.25,labels=FALSE)
axis(side=2, at = seq(0,1,.25),cex.axis=1.2,labels=FALSE)

f <- function(tmp){
  
  return( .75-2*(x-.5)^2)
}
x = seq(0,1,by=.01)
#x = runif(100,0,1)
library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(100))

colfunc <- colorRampPalette(c("purple", "orange","purple"))
colfunc(100)

df <-data.frame(x,f(x))

points(x,f(x),pch=16,cex=1,col=colfunc(100))


## Plot 3

par(mfrow=c(1,1),mar=c(5,6,4,2)+.1)
plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,1),ylim=c(0,1),
     xlab=expression("alpha"), ylab=expression("m"),
     xaxs="i", yaxs="i",cex.lab=2)

axis(side=1, at = seq(0,2,.25),cex.axis=1.25,labels=FALSE)
axis(side=2, at = seq(0,2,.25),cex.axis=1.2,labels=FALSE)

f <- function(x){
  y=x*seq(0,2,by=.1)
  return(y)
}

lines(f(1),f(1))

lines(f(1),f(.75),col='green')
lines(f(1),f(.5),col='green')
lines(f(1),f(.25),col='green')
lines(f(1),f(.1),col='green')

lines(f(1),x=f(.75),col='red')
lines(f(1),x=f(.5),col='red')
lines(f(1),x=f(.25),col='red')
lines(f(1),x=f(.1),col='red')

f2 <- function(alpha,m){
  
  return(alpha/m)
}

points(.1,f2(.1,1))

points(1,1,pch=16,cex=2)
points(1*seq(1,2,by=.1),1*seq(1,2,by=.1),pch=16,cex=1)

points(1,.5,pch=16,cex=2)
points(1*seq(1,2,by=.1),.5*seq(1,2,by=.1),pch=16,cex=1)

points(.5,1,pch=16,cex=2)
points(.5*seq(1,2,by=.1),1*seq(1,2,by=.1),pch=16,cex=1)

points(1,.1,pch=16,cex=2)
points(1*seq(1,2,by=.1),.1*seq(1,2,by=.1),pch=16,cex=1)

points(.1,1,pch=16,cex=2)
points(.1*seq(1,2,by=.1),1*seq(1,2,by=.1),pch=16,cex=1)


#x = runif(100,0,1)
library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "YlOrRd"))(length(100))

colfunc <- colorRampPalette(c("purple", "orange","purple"))
colfunc(100)

df <-data.frame(x,f(x))

points(x,f(x),pch=16,cex=1,col=colfunc(100))

####### SEASON LENGTH DISTRIBUTIONS

mu = .5
sd = .05

par(mfrow=c(2,2))
plot(seq(0,1,by=0.01),dnorm(seq(0,1,by=0.01),mu,sd),type='l')
plot(seq(0,1,by=0.01),pnorm(seq(0,1,by=0.01),mu,sd),type='l')

n=100
t<-rnorm(n,mu,sd)
plot(x=NA,y=NA,xlim=c(0,1),ylim=c(0,n),type='n')
segments(x0=0,x1=t,y0=1:n)
plot(x=NA,y=NA,xlim=c(0,1),ylim=c(0,1),type='n')

segments(x0=0,x1=t[order(t)],y0=(1:n)/n,col='red')
lines(seq(0,1,by=0.01),pnorm(seq(0,1,by=0.01),mu,sd),type='l')
