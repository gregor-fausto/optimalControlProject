
# Interpolation -----------------------------------------------------------

topt=seq(0,5,length=11); 
par0 = runif(length(topt),0.01,0.05); 
f1 = approxfun(topt,par0,rule=2,method='linear');

plot(topt,f1(topt),pch=16,xlab='Time',ylab="Control")
segments(x0=topt,x1=c(topt[-1],topt[11]),y0=f1(topt))


# Error -------------------------------------------------------------------

plot(seq(-1,1,by=0.01),abs(seq(-1,1,by=0.01))^2,type='l')
lines(seq(-1,1,by=0.01),abs(seq(-1,1,by=0.01))^1.25,type='l')
lines(seq(-1,1,by=0.01),abs(seq(-1,1,by=0.01)),type='l')

vals = seq(-.02,.02,by=0.001)

plot(vals,abs(vals)^2,type='l')
lines(vals,abs(vals)^1.25,type='l')
lines(vals,abs(vals)^1.5,type='l')
lines(vals,abs(vals)^1.75,type='l')
lines(vals,abs(vals),type='l')

