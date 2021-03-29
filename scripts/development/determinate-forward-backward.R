### Optimal control for King & Roughgarden 1982 ###
### Forward-Backward Sweep ###
### Gregor Siegmund ###
### gs589@cornell.edu ###
### Help links

# clear history
 rm(list=ls(all=TRUE))

# load deSolve
library(deSolve)
library(compiler)

###########################################################
# Starting conditions for ODEs and optimizer 
###########################################################
mu = 2.5
del = 2.5
tl = mu - del
tf = mu + del
Tf = 5
xA = c(P=.1,V=0,I=0,L=0)

# time steps
nt = 101

## time steps for ODE
tVals=seq(0,5,length=nt) # evaluation points 

### Starting point: initial guess at the optimal control 
uVals = rep(.1,nt); 

## parameters in ODE
parmsA=c();

###########################################################
# ODE for state variables x
###########################################################
## The control u(t) must be an externally defined function [uFunc(t)]
derivs = numeric(4)
forward <- function(t, y, parms, f1, ...) {
  
  # P, V, I, L are the four entries in y (ODE)
  P = y[1]
  V = y[2]
  I = y[3]
  L = y[4]
  
  # control function calculated by f1 at different time points
  pt <- f1(t)
  
  dPdt = 2 * pt * P - pt * P - (1 - pt) * P
  dVdt = pt * P + (1 - pt) * P
  dIdt = (1 - pt) * P
  dLdt = (1 - pt) * P + I
  ratio = .1/1*V
  
  #derivs[1] = 2 * pt * P - pt * P - (1 - pt) * P
  derivs[1] = ifelse(dPdt <= ratio, dPdt, ratio)
  derivs[2] = pt * P + (1 - pt) * P
  derivs[3] = (1 - pt) * P
  derivs[4] = (1 - pt) * P + I
  
  ## other parameters beta1 = parms[1] beta2 = parms[2]
  
  # derivs[1]=2*beta1*pt*P - beta1*pt*P - (1-pt)*beta1*P; derivs[2]=beta1*pt*P +
  # (1-pt)*beta1*P; derivs[3]=(1-pt)*beta1*P; derivs[4]=(1-pt)*beta1*P + beta2*I;
  
  return(list(derivs))
}
forward = cmpfun(forward)

###########################################################
# ODE for adjoint variables lambda
###########################################################
## The state x(t) must be an externally defined function [xFunc(t)]
derivs = numeric(4)
backward = function(t,y,parms,f1,f2) {
  
  lambda1=y[1];
  lambda2=y[2];
  lambda3=y[3];
  lambda4=y[4];
  
  pt = f1(t);
  L = f2(t);
  
  derivs[1] = -((2*lambda1 - lambda3 - lambda4)*pt + (-lambda1+lambda2+lambda2+lambda4))
  derivs[1] = -(2*lambda1-lambda3-lambda4)*pt-lambda4-lambda3-lambda2+lambda1
  derivs[2] = 0
  derivs[3] = -lambda4
  derivs[4] = -(1/L)
  
  return(list(derivs))
}
backward = cmpfun(backward)

###########################################################
# Set season length distribution
###########################################################
seasonLength <- dunif(tVals,min=tl,max=tf)

par(mfrow=c(2,1))

plot(tVals,seasonLength,
     type='n',ylim=c(0,1),
     xlab="Time",ylab="Frequency")
abline(v=tl,col='lightgray');abline(v=tf,col='lightgray')
for(i in 1:length(tVals)){
  segments(x0=tVals[i],y0=0,x1=tVals[i],y1=seasonLength[i])
}

plot(tVals,uVals,type='n',ylim=c(0,1))
abline(v=tl,col='lightgray');abline(v=tf,col='lightgray')
lines(tVals,uVals)

###########################################################
# Forward backward sweep
###########################################################
counter = 0
stop_criteria = 1

while(stop_criteria > 1e-5){
  
  # Using current u(t) solve forward for new x(t)
  uFunc=splinefun(tVals,uVals); 
  out = ode(xA,tVals,forward,parms=parmsA, atol = 1e-7, f1 = uFunc); 
  new_xVals = out[,2:5]; 

  # Function for reproductive value
  xFunc = approxfun(tVals,new_xVals[,4]);	

  # Using current u(t) and x(t), solve backwards for new lambda(t) 
  # note, ode() will solve in reverse time 
  out = ode(c(0,0,0,0),rev(tVals),backward,parms=parmsA , f1 = uFunc, f2 = xFunc, atol = 1e-7, method="euler");
  new_lambdaVals = list(rev(out[,2]),rev(out[,3]) ,rev(out[,4]),rev(out[,5]));
  
  # update control u(t), using Maximum Principle solution 
  switch = new_xVals[,1]*(2*new_lambdaVals[[1]]-new_lambdaVals[[3]]-new_lambdaVals[[4]]);

  # new_uVals = ifelse(switch<0,0,1)
  new_uVals = uVals + 0.01*switch;
  # constraint ??
  #new_uVals = ifelse(new_xVals[,2]/new_xVals[,1] > 1,   1, new_uVals)
  new_uVals[new_uVals<0]=0;
  new_uVals[new_uVals>1]=1;
  
  # use constraints or bounds?
  delta = abs(uVals - new_uVals);
  stop_criteria = max(delta);
  uVals = new_uVals;
  
  lines(tVals,uVals);
  
  counter = counter + 1;
}

plot(tVals,uVals,type='n',ylim=c(0,1))
abline(v=tl,col='lightgray');abline(v=tf,col='lightgray')
lines(tVals,uVals)

#U
# 
#pdf(file=paste0("~/Dropbox/optimalControlProject/figures/forwardBackwardSweep-x1=",xA[1],"-x2=",xA[2],",uniform(",mu-delta,",",mu+delta,").pdf"))
par(mfrow=c(3,1))

plot(tVals[(1:(nt-1))],seasonLength[(1:(nt-1))],
     type='n',ylim=c(0,1),
     xlab="Time",ylab="Frequency",
     main=paste0("x1=",xA[1],", x2=",xA[2]))
for(i in 1:length(tVals)){
  segments(x0=tVals[i],y0=0,x1=tVals[i],y1=seasonLength[i])
}
legend("topright",paste0("Uniform(",tl,",",tf,")"),
       bty='n', inset=c(0,0),cex=0.75)

plot(tVals[(1:(nt-1))],uVals[(1:(nt-1))],
     type='l',ylim=c(0,1),
     xlab="Time (t)",ylab="u(t)")
legend("topright",c(paste("iterations =",counter)),
       bty="n", inset=c(0,0),cex=0.75)

plot(tVals[(1:(nt-1))],new_lambdaVals[[1]][(1:(nt-1))],
     type='l',
     xlab="Time (t)",ylab="Adjoint variables")
lines(tVals[(1:(nt-1))],new_lambdaVals[[2]][(1:(nt-1))],lty='dashed')
lines(tVals[(1:(nt-1))],new_lambdaVals[[3]][(1:(nt-1))],lty='dotted',col='red')
lines(tVals[(1:(nt-1))],new_lambdaVals[[4]][(1:(nt-1))],lty='dotdash')
legend("topright",
       c(expression(lambda[1]),expression(lambda[2]),
         expression(lambda[3]),expression(lambda[4])),
       lty=c("solid","dashed","dotted","dotdash"),
       bty="n", inset=c(0,0))
#dev.off()

par(mfrow=c(1,1))
plot(tVals[(1:(nt-1))],new_xVals[,1][(1:(nt-1))],
     type='l',
     xlab="Time (t)",ylab="State variables",
     ylim=c(0,max(new_xVals)))
lines(tVals[(1:(nt-1))],new_xVals[,2][(1:(nt-1))],lty='dashed')
lines(tVals[(1:(nt-1))],new_xVals[,3][(1:(nt-1))],lty='dotted')
lines(tVals[(1:(nt-1))],new_xVals[,4][(1:(nt-1))],lty='dotdash')
legend("topleft",c(expression(P),expression(V),expression(I),expression(F)),
       lty=c("solid","dashed","dotted","dotdash"),
       bty="n", inset=c(0,0))

# # the oscillations in control u are a product of the
# # numerical method not being able to identify the switching time, and so overcorrecting
# # see 13.5 in https://syscop.de/files/2020ss/NOC/book-NOCSE.pdf
# # so oscillations indicate a singular control 
# # I can't understand if this is OK or not...