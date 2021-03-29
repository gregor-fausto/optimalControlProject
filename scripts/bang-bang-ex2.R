### Gregor Siegmund ###
### gs589@cornell.edu ###
## Based on Kamien and Schwartz p 205 ###

# clear history
rm(list=ls(all=TRUE))

# load deSolve
library(deSolve)
library(compiler)

###########################################################
# Starting conditions for ODEs and optimizer 
###########################################################
# initial conditions
x0 = 1

# time steps
nt = 1001
Tfinal = 5

## time steps for ODE
tVals=seq(0,Tfinal,length=nt) # evaluation points 

### Starting point: initial guess at the optimal control 
zVals = rep(.25,nt)

## parameters in ODE
parmsA=c(r=1);

###########################################################
# ODE for state variables x
###########################################################
## The control u(t) must be an externally defined function [uFunc(t)]
derivs = numeric(1)
forward <- function(t, y, parms, f1, ...) {
  
  # state variables
  y = y[1]
  
  # control function calculated by f1 at different time points
  zt <- f1(t)
  
  # parameters
  r = parms[1]

  derivs[1] = zt*r*y

  return(list(derivs))
}
forward = cmpfun(forward)

###########################################################
# ODE for adjoint variables lambda
###########################################################
## The state x(t) must be an externally defined function [xFunc(t)]
derivs = numeric(1)
backward = function(t,y,parms,f1,f2, s1, s2,s3,s4) {
  
  lambda1=y[1];

  zt = f1(t);

  r = parms[1]
  
  derivs[1] = -((1-zt)*r+lambda1*zt*r)

  return(list(derivs))
}
backward = cmpfun(backward)

###########################################################
# use this if wanting to plot distribution of season length
###########################################################

## uncomment to plot running update of controls
plot(tVals,zVals,type='n',ylim=c(0,1))
lines(tVals,zVals,lty='solid')

###########################################################
# Forward backward sweep
###########################################################
counter = 0
stop_criteria = 1

grad_p <- grad_q <-matrix(NA,nrow=nt,ncol=100)

while(stop_criteria > 1e-5){
#  for(i in 1:100){

# interpolate control functions
zFunc=approxfun(tVals,zVals,rule=2); 

# Using current u(t) solve forward for new x(t)
out = ode(x0,tVals,forward,parms=parmsA, atol = 1e-7, f1 = zFunc); 
new_xVals = out[,c(2)]; 

# Using current u(t) and x(t), solve backwards for new lambda(t) 
out = ode(c(0),rev(tVals),backward,parms=parmsA ,atol = 1e-7,  f1 = zFunc, method="euler");
new_lambdaVals = list(rev(out[,2]) );

# # update control using gradient, first p then q
# switch_z = -parmsA[1]*new_xVals+new_lambdaVals[[1]]*parmsA[1]*new_xVals;
# new_zVals = zVals + 0.025*switch_z;
# new_zVals[new_zVals<0]=0;
# new_zVals[new_zVals>1]=1;

# # update control using KT conditions
new_zVals = zVals
new_zVals[new_lambdaVals[[1]]==1]=-parmsA[1]*new_xVals+new_lambdaVals[[1]]*parmsA[1]*new_xVals;
new_zVals[new_lambdaVals[[1]]>1]=1
new_zVals[new_lambdaVals[[1]]<1]=0

delta = abs(zVals - new_zVals);
zVals = new_zVals;

# check for exiting while statement
stop_criteria = max(delta);

lines(tVals,zVals,lty='solid')

counter = counter + 1;
}

# calculate optimal switch
tStar = (parmsA[1]*Tfinal-1)/parmsA[1]


par(mfrow=c(1,3))
## uncomment to plot running update of controls
plot(tVals,zVals,type='n',ylim=c(0,1))
lines(tVals,zVals,lty='solid')
abline(v=tStar,col='red',lty='dotted')

plot(tVals,new_xVals,type='l')
abline(v=tStar,col='red',lty='dotted')

plot(tVals,new_lambdaVals[[1]],type='l')
abline(v=tStar,col='red',lty='dotted')

print(counter)
