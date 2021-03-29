### Optimal control for King & Roughgarden 1982 ###
### Forward-Backward Sweep ###
### Gregor Siegmund ###
### gs589@cornell.edu ###
### Help links

# clear history
 rm(list=ls(all=TRUE))

setwd("~/Dropbox/optimalControlProject/king-roughgarden") #### Edit as needed 

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
xA = c(x1=1,x2=45.4)

# time steps
nt = 101

## time steps for ODE
tVals=seq(0,5,length=nt) # evaluation points 

### Starting point: initial guess at the optimal control 
#uVals = rep(.1,nt); 
uVals = runif(nt,0,1); 

## parameters in ODE
parmsA=c();

###########################################################
# ODE for state variables x
###########################################################
## The control u(t) must be an externally defined function [uFunc(t)]
derivs=numeric(2); 
forward <- function(times0,y,parms,f1,...) {
  
  # x1 and x2 are the two entries in y (ode)
  x1=y[1]; 
  
  # control function calculated f1 at different time points
  u <- f1(times0);
  
  derivs = c(u*x1,(1-u)*x1) 
  return(list(derivs));
}

###########################################################
# ODE for adjoint variables lambda
###########################################################
## The state x(t) must be an externally defined function [xFunc(t)]
backward = function(t,y,parms,f1,f2) {
  
  lambda1=y[1];
  lambda2=y[2];
  u = f1(t);
  x2 = f2(t);
  
  return(list(c((lambda2-lambda1)*u - lambda2, -(1/x2) )))
}

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
  new_xVals = out[,c(2,3)]; 

  # Adjust reproductive output by season length distribution
  xFunc = approxfun(tVals,new_xVals[,2]*seasonLength);	

  # Using current u(t) and x(t), solve backwards for new lambda(t) 
  # note, ode() will solve in reverse time 
  out = ode(c(0,0),rev(tVals),backward,parms=parmsA , f1 = uFunc, f2 = xFunc, method="euler");
  new_lambdaVals = list(rev(out[,2]),rev(out[,3]) );
  
  # update control u(t), using Maximum Principle solution 
  switch = new_xVals[,1]*(new_lambdaVals[[1]]-new_lambdaVals[[2]]);

  # new_uVals = ifelse(switch<0,0,1)
  new_uVals = uVals + 0.01*switch;
  new_uVals[new_uVals<0]=0;
  new_uVals[new_uVals>1]=1;
  # use constraints or bounds?
  delta = abs(uVals - new_uVals);
  stop_criteria = max(delta);
  uVals = new_uVals;
  
  lines(tVals,uVals);
  
  counter = counter + 1;
}

# plot(tVals,uVals,type='n',ylim=c(0,1))
# abline(v=tl,col='lightgray');abline(v=tf,col='lightgray')
# lines(tVals,uVals)

#U
# 
#pdf(file=paste0("~/Dropbox/optimalControlProject/figures/forwardBackwardSweep-x1=",xA[1],"-x2=",xA[2],",uniform(",mu-delta,",",mu+delta,").pdf"))
par(mfrow=c(3,1))

plot(tVals[(1:(m-1))],seasonLength[(1:(m-1))],
     type='n',ylim=c(0,1),
     xlab="Time",ylab="Frequency",
     main=paste0("x1=",xA[1],", x2=",xA[2]))
for(i in 1:length(tVals)){
  segments(x0=tVals[i],y0=0,x1=tVals[i],y1=seasonLength[i])
}
legend("topright",paste0("Uniform(",tl,",",tf,")"),
       bty='n', inset=c(0,-0.03),cex=0.75)

plot(tVals[(1:(m-1))],uVals[(1:(m-1))],
     type='l',ylim=c(0,1),
     xlab="Time (t)",ylab="u(t)")
legend("topright",c(paste("iterations =",counter)),
       bty="n", inset=c(0,-0.03),cex=0.75)

plot(tVals[(1:(m-1))],new_lambdaVals[[1]][(1:(m-1))],
     type='l',
     xlab="Time (t)",ylab="Adjoint variables")
lines(tVals[(1:(m-1))],new_lambdaVals[[2]][(1:(m-1))],lty='dotted')
legend("topright",c(expression(lambda[1]),expression(lambda[2])),lty=c("solid","dotted"),
       bty="n", inset=c(0,-0.03))
#dev.off()
# 
# # plot(tVals[(1:(m-1))],U[(1:(m-1)),iterations],
# #      type='l',ylim=c(0,1),
# #      xlab="Time (t)",ylab="u(t)")
# # 
# # plot(tVals[(1:(m-1))],new_lambdaVals[[1]][(1:(m-1))],
# #      type='l',
# #      xlab="Time (t)",ylab="Adjoint variables")
# # lines(tVals[(1:(m-1))],new_lambdaVals[[2]][(1:(m-1))],lty='dotted')
# # legend(4,.5,c("lambda_1","lambda_2"),cex=0.5,lty=c("solid","dotted"))
# 
# # plot(S[1,],type='n',ylim=c(-.2,1.5))
# # for(i in 1:51){
# # lines(S[i,])
# # }
# 
# #par(mfrow=c(2,2))
# #hist(S[,iterations]);
# #plot(S[,iterations],type='l')
# #S[,iterations]
# 
# # par(mfrow=c(2,2))
# # plot(U[,iterations])
# # plot(S[,iterations],type='l')
# 
# # plot((U[26:57,iterations]))
# # plot((S[26:57,iterations]))
# # plot((S[31:47,iterations]))
# # plot(sign(S[31:47,iterations]))
# 
# # the oscillations in control u are a product of the
# # numerical method not being able to identify the switching time, and so overcorrecting
# # see 13.5 in https://syscop.de/files/2020ss/NOC/book-NOCSE.pdf
# # so oscillations indicate a singular control 
# # I can't understand if this is OK or not...