## Conceptual figure

## Meristem constraints only
par(mfrow=c(1,1))

plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,10),ylim=c(0,10),
     xlab=expression(paste(beta)[1]), ylab=expression(paste(beta)[2]),
     xaxs="i", yaxs="i")

axis(side=1, at = seq(0,10,by=2))
axis(side=2, at = seq(0,10,by=2))

segments(x0=4,y0=0,y1=10,lty='dotted',lwd=2)
segments(x0=0,y0=4,x1=10,lty='dotted',lwd=2)

points(x=4,y=4,pch=16,cex=2)


## Resource constraints only
par(mfrow=c(1,1))

plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,10),ylim=c(0,10),
     xlab=expression(paste(beta)[1]), ylab=expression(paste(beta)[2]),
     xaxs="i", yaxs="i")

axis(side=1, at = seq(0,10,by=2))
axis(side=2, at = seq(0,10,by=2))

abline(a=3,b=-1,lty='dashed',lwd = 3)

points(x=2,y=1,pch=16,cex=2)
points(x=1,y=2,pch=16,cex=2)
points(x=3,y=0,pch=16,cex=2)


## Meristem and resource constraints 
par(mfrow=c(1,1))

plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,10),ylim=c(0,10),
     xlab=expression(paste(beta)[1]), ylab=expression(paste(beta)[2]),
     xaxs="i", yaxs="i")

axis(side=1, at = seq(0,10,by=2))
axis(side=2, at = seq(0,10,by=2))

segments(x0=4,y0=0,y1=10,lty='dotted',lwd=2)
segments(x0=0,y0=4,x1=10,lty='dotted',lwd=2)


abline(a=2,b=-1,lty='dashed',lwd = 2)
abline(a=4,b=-1,lty='dashed',lwd = 1)
abline(a=6,b=-1,lty='dashed',lwd = .5)
abline(a=12,b=-1,lty='dashed',lwd = .25)

