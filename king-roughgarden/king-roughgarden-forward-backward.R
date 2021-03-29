### Optimal control for King & Roughgarden 1982 ###
### Forward-Backward Sweep ###
### Gregor Siegmund ###
### gs589@cornell.edu ###
### Help links

# clear history
# rm(list=ls(all=TRUE))

setwd("~/Dropbox/optimalControlProject/king-roughgarden") #### Edit as needed 

# load deSolve
library(deSolve)
library(compiler)
library(BB) # for projectLinear 

################################################## 
# Construct the constraint matrix 
##################################################
# Number of time steps
# t is also used in control() and optim_fun()
nt = 101;

## Block 1
## Constraint: u-1 <= 0
## u4 positive
A1 = matrix(0,nrow=nt,ncol=nt);
diag(A1) = -1; c1 = rep(-1,nt);

## Block 2
## Constraint: u >= 0
A2 = matrix(0,nrow=nt,ncol=nt);
diag(A2) = 1; c2 = rep(0,nt);

# create matrix A and vector b
Amat = rbind(A1,A2);
bvec = c(c1,c2);

###########################################################
# Starting conditions for ODEs and optimizer 
###########################################################
Tf = 5
xA = c(x1=1,x2=45.4)

## time steps for ODE
#seq_length = 100;
timesA=seq(0,Tf,length=nt);

## parameters in ODE
parmsA=c();

###########################################################
# Initial guess
###########################################################
# initial guess for control u over the interval
uVals_init = runif(nt);

# check to make sure initial guess is acceptable 
# meaning does it satisfy the constraints 
# if FALSE ok to proceed
any(Amat %*% uVals_init - bvec <= 0)

## ODE for x 
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

## ODE for lambda
## The state x(t) must be an externally defined function [xFunc(t)]
backward = function(t,y,parms,f1,f2) {
  
  lambda1=y[1];
  lambda2=y[2];
  u = f1(t);
  x2 = f2(t);
  
  return(list(c((lambda2-lambda1)*u - lambda2, -(1/x2) )))
}


m = 101 # number of points for function evaluation 
tVals=seq(0,5,length=m) # evaluation points 

### Starting point: initial guess at the optimal control 
iterations = 1000
uVals = rep(.1,m); 
U = matrix(0,m,iterations)  # matrix to store iterations for plotting 
S = matrix(0,m,iterations)
counter = 0

## Forward-backward sweep 
#for(j in 1:iterations) {
stop_criteria = 1

plot(tVals,uVals,type='l',ylim=c(0,1))

while(stop_criteria > 1e-5){
  
  # Using current u(t) solve forward for new x(t)
  uFunc=splinefun(tVals,uVals); 
  # or, = approxfun(tVals,uVals,rule=2)
  out = ode(xA,tVals,forward,parms=parms0, atol = 1e-7, f1 = uFunc); 
  new_xVals = out[,c(2,3)] 
 
  # Using current u(t) and x(t), solve backwards for new lambda(t) 
  #xFunc = approxfun(tVals,new_xVals[,2]);	
  xFunc = splinefun(tVals,new_xVals[,2]);
 
  #plot(seq(0,Tf,length=100),xFunc(seq(0,Tf,length=100)))
  out = ode(c(0,0),rev(tVals),backward,parms=parms0 , f1 = uFunc, f2 = xFunc)
  # note, ode() will solve in reverse time 
  new_lambdaVals = list(rev(out[,2]),rev(out[,3]) )
  
  # update control u(t), using Maximum Principle solution 
  switch = new_xVals[,1]*(new_lambdaVals[[1]]-new_lambdaVals[[2]])
  # switch = ifelse(abs(switch) < 1e-5, 0, new_xVals*switch)
  
  # new_uVals = ifelse(switch<0,0,1)
  new_uVals = uVals + 0.05*switch
  new_uVals[new_uVals<0]=0
  new_uVals[new_uVals>1]=1
  # use constraints or bounds?
  delta = abs(uVals - new_uVals)
  stop_criteria = max(delta)
  uVals = new_uVals
  
  # take a halfway step to reduce odds of overshoot  
 # uVals = 0.5*(uVals + new_uVals); 	
  # uVals = 0.98*uVals + 0.02*new_uVals
  # U[,j]=uVals; 
  # S[,j]=switch
  
  lines(tVals,uVals)
  
  counter = counter + 1
}
U
# 
# pdf(file=paste0("~/Dropbox/optimalControlProject/figures/forwardBackwardSweep-x1=",xA[1],"-x2=",xA[2],".pdf"))
# par(mfrow=c(2,1))
# #matplot(U[(1:(m-1)),c((iterations-10):iterations)],type='l',ylim=c(0,1))
# 
# plot(tVals[(1:(m-1))],uVals[(1:(m-1))],
#      type='l',ylim=c(0,1),
#      xlab="Time (t)",ylab="u(t)")
# legend("topright",c(paste("iterations =",counter)),
#        bty="n", inset=c(0,-0.03),cex=0.75)
# 
# plot(tVals[(1:(m-1))],new_lambdaVals[[1]][(1:(m-1))],
#      type='l',
#      xlab="Time (t)",ylab="Adjoint variables")
# lines(tVals[(1:(m-1))],new_lambdaVals[[2]][(1:(m-1))],lty='dotted')
# legend("topright",c(expression(lambda[1]),expression(lambda[2])),lty=c("solid","dotted"),
#        bty="n", inset=c(0,-0.03))
# dev.off()
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