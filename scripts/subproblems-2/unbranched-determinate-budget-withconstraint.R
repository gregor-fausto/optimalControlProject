### Gregor Siegmund ###
### gs589@cornell.edu ###

# clear history
rm(list=ls(all=TRUE))

par(mfrow=c(1,1))

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

# initial conditions
xA = c(P=1,V=.5,L=0)

# time steps
nt = 101

## time steps for ODE
tVals=seq(0,5,length=nt) # evaluation points 

### Starting point: initial guess at the optimal control 
uVals = rep(.6,nt)
beta1Vals = rep(.5,nt)

## parameters in ODE
parmsA=c();

M = 1
alpha=10

###########################################################
# ODE for state variables x
###########################################################
## The control u(t) must be an externally defined function [uFunc(t)]
derivs = numeric(3)
forward <- function(t, y, parms, f1, f2, ...) {
  
  # P, V, I, L are the four entries in y (ODE)
  P = y[1]
  V = y[2]
  L = y[3]
  
  # control function calculated by f1 at different time points
  ut <- f1(t)
  beta1t <- f2(t)

  derivs[1] =  - (beta1t) * ((1 - ut) * P)
  derivs[2] = (beta1t) * ut * ( P )
  derivs[3] = (beta1t) * ((1 - ut) * P)
  
  return(list(derivs))
}
forward = cmpfun(forward)

###########################################################
# ODE for adjoint variables lambda
###########################################################
## The state x(t) must be an externally defined function [xFunc(t)]
derivs2 = numeric(4)
backward = function(t,y,parms,f1,f2, s1, s2, s3 ) {
  
  lambda1=y[1];
  lambda2=y[2];
  lambda3=y[3];

  ut <- f1(t)
  beta1t <- f2(t)

  P = s1(t);
  V = s2(t);
  L = s3(t); 
  
 # eta= (lambda1+lambda2-lambda3) - (lambda3-lambda1)/(beta1t-ut)
  
  #const = alpha*V-beta1t*P
  phi=c()
 # for(i in 1:length(const)){
#    phi[i] = ifelse(const[i]<=0,0,eta[i])
#  }
  
  phi = ifelse(alpha*V-beta1t*P>0,0,ifelse(beta1t-ut==0,0,(lambda1+lambda2-lambda3) - (lambda3-lambda1)/(beta1t-ut)))

  derivs2[1] = beta1t * ut * ( lambda3 + phi - lambda1 - lambda2 ) + beta1t * (lambda1 - lambda3)
  derivs2[2] = ifelse(phi==0,0,-alpha*phi)
  derivs2[3] = -(1/L)
  derivs2[4] = phi

  return(list(derivs2))
}
backward = cmpfun(backward)

###########################################################
# Set season length distribution
# use this if wanting to plot distribution of season length
###########################################################
# seasonLength <- dunif(tVals,min=tl,max=tf)

# par(mfrow=c(2,1))

## uncomment to plot season length distribution
# plot(tVals,seasonLength,
#      type='n',ylim=c(0,1),
#      xlab="Time",ylab="Frequency")
# abline(v=tl,col='lightgray');abline(v=tf,col='lightgray')
# for(i in 1:length(tVals)){
#   segments(x0=tVals[i],y0=0,x1=tVals[i],y1=seasonLength[i])
# }

## uncomment to plot running update of controls
 plot(tVals,uVals,type='n',ylim=c(0,1))
 abline(v=tl,col='lightgray');abline(v=tf,col='lightgray')
 lines(tVals,uVals,lty='solid')
# lines(tVals,qVals,lty='dotted')

###########################################################
# Forward backward sweep
###########################################################
counter = 0
stop_criteria = 1

while(stop_criteria > 1e-5){

  # interpolate control functions
  uFunc=approxfun(tVals,uVals,rule=2);
  beta1Func=approxfun(tVals,beta1Vals,rule=2);

  # Using current u(t) solve forward for new x(t)
  out = ode(xA,tVals,forward,parms=parmsA, atol = 1e-7, f1 = uFunc, f2 = beta1Func );
  new_xVals = out[,c(2:4)];

  # interpolate state variables
  P_Func = approxfun(tVals,new_xVals[,1],rule=2);
  V_Func = approxfun(tVals,new_xVals[,2],rule=2);
  L_Func = approxfun(tVals,new_xVals[,3],rule=2);

   # Using current u(t) and x(t), solve backwards for new lambda(t)
  y = c(0,0,0,0)
  t = rev(tVals)
  f1 = uFunc
  f2 = beta1Func
  s1=P_Func
  s2=V_Func
  s3=L_Func

  out = ode(c(0,0,0,0),rev(tVals),backward,parms=parmsA ,atol = 1e-7,  f1 = uFunc, f2 = beta1Func, s1=P_Func, s2=V_Func, s3=L_Func, method="euler");
  new_lambdaVals = list(rev(out[,2]),rev(out[,3]), rev(out[,4]) , rev(out[,5]));
  
  # update control using gradient, first p then q
  switch_u = (new_lambdaVals[[1]]+new_lambdaVals[[2]] - new_lambdaVals[[3]] - new_lambdaVals[[4]])*beta1Func(tVals)*P_Func(tVals);
  new_uVals = uVals + 0.05*switch_u;
  new_uVals[new_uVals<0]=0;
  new_uVals[new_uVals>1]=1;
  delta_u = abs(uVals - new_uVals);
  uVals = new_uVals;
  
  switch_b1 = (new_lambdaVals[[1]]+new_lambdaVals[[2]] - new_lambdaVals[[3]]- new_lambdaVals[[4]])*uFunc(tVals)*P_Func(tVals) + (new_lambdaVals[[3]] - new_lambdaVals[[1]])*P_Func(tVals);
  new_b1Vals = beta1Vals + 0.05*switch_b1;
  new_b1Vals[new_b1Vals<0]=0;
  new_b1Vals[new_b1Vals>M]=M;
  delta_b1 = abs(beta1Vals - new_b1Vals);
  beta1Vals = new_b1Vals;
  
  # check for exiting while statement
  stop_criteria = max(delta_u,delta_b1);
  
  # uncomment lines to get running update of controls
   lines(tVals,uVals,lty='solid')

  counter = counter + 1;
  stop_criteria = ifelse(counter > 15000,stop_criteria==0,stop_criteria);
}

###########################################################
# Figures
# uncomment lines to save as pdf
###########################################################

## Plot optimal controls
# pdf(file=paste0("~/Dropbox/optimalControlProject/figures/unbranched-determinate-P=",xA[1],"-V=",xA[2],",uniform(",mu-del,",",mu+del,").pdf"),width=4,height=4)
par(mfrow=c(1,2))
plot(tVals[(1:(nt-1))],uVals[(1:(nt-1))],
     type='l',ylim=c(0,1),
     xlab="Time (t)",ylab="p(t) and q(t)",
     main=paste0("P=",xA[1],", V=",xA[2],", I=",xA[3],", F=",xA[4]))
legend("topright",c("u"),lty=c('solid'),
       bty="n", inset=c(0,0),cex=0.75)
text(4.5,.75,labels=c(paste("iterations =",counter)),cex=0.5)

plot(tVals[(1:(nt-1))],beta1Vals[(1:(nt-1))],
     type='l',ylim=c(0,M),
     xlab="Time (t)",ylab="p(t) and q(t)",
     main=paste0("P=",xA[1],", V=",xA[2],", I=",xA[3],", F=",xA[4]),
     lty='solid')
lines(tVals[(1:(nt-1))],beta2Vals[(1:(nt-1))],col='red',lty='dashed')
legend("topright",c("b1","b2"),lty=c('solid','dashed'),
       bty="n", inset=c(0,0),cex=0.75)
text(4.5,.75,labels=c(paste("iterations =",counter)),cex=0.5)
#dev.off()
  
## Plot trajectories of state variables
# pdf(file=paste0("~/Dropbox/optimalControlProject/figures/unbranched-determinate-states.pdf"),width=6,height=6)
par(mfrow=c(2,2))
plot(tVals,new_xVals[,1],type='l',xlab="Time",ylab="Primary meristems (P)")
plot(tVals,new_xVals[,2],type='l',xlab="Time",ylab="Vegetative biomass (V)")
plot(tVals,new_xVals[,3],type='l',xlab="Time",ylab="Inflorescence meristems (I)")
plot(tVals,new_xVals[,4],type='l',xlab="Time",ylab="Floral meristems (F)")
# dev.off()

plot(tVals,eta,type='l',xlab="Time",ylab="Floral meristems (F)")


## Plot trajectories of adjoint variables
## NOTE: may need to ajust upper limit on ylim(c(0,x))
# pdf(file=paste0("~/Dropbox/optimalControlProject/figures/unbranched-determinate-adjoints.pdf"),width=6,height=6)
par(mfrow=c(1,1))
plot(tVals[(1:(nt-1))],new_lambdaVals[[1]][(1:(nt-1))],
     type='l',
     xlab="Time (t)",ylab="Adjoint variables",
     ylim=c(0,100))
lines(tVals[(1:(nt-1))],new_lambdaVals[[2]][(1:(nt-1))],lty='dashed',col='red')
lines(tVals[(1:(nt-1))],new_lambdaVals[[3]][(1:(nt-1))],lty='dotted',col='orange')
lines(tVals[(1:(nt-1))],new_lambdaVals[[4]][(1:(nt-1))],lty='dotdash',col='green')
legend("topright",c(expression(lambda[1]),expression(lambda[2]),expression(lambda[3]),expression(lambda[4])),
       lty=c('solid','dashed',"dotted","dotdash"), col = c("black","red","orange","green"),
       bty="n", inset=c(0,0),cex=0.75)
# dev.off()

## Summary figure with season distribution, controls, adjoints; uncomment all if needed 
# pdf(file=paste0("~/Dropbox/optimalControlProject/figures/unbranched-determinate-P=",xA[1],"-V=",xA[2],",uniform(",mu-delta,",",mu+delta,").pdf"))
# par(mfrow=c(2,2))
# plot(tVals[(1:(nt-1))],seasonLength[(1:(nt-1))],
#      type='n',ylim=c(0,1),
#      xlab="Time",ylab="Frequency",
#      main=paste0("P=",xA[1],", V=",xA[2]))
# for(i in 1:length(tVals)){
#   segments(x0=tVals[i],y0=0,x1=tVals[i],y1=seasonLength[i])
# }
# legend("topright",paste0("Uniform(",tl,",",tf,")"),
#        bty='n', inset=c(0,-0.03),cex=0.75)
# 
# plot(tVals[(1:(nt-1))],pVals[(1:(nt-1))],
#      type='l',ylim=c(0,1),
#      xlab="Time (t)",ylab="u(t)")
# lines(tVals[(1:(nt-1))],qVals[(1:(nt-1))],
#       type='l',lty='dashed')
# legend("topright",c(paste("iterations =",counter)),
#        bty="n", inset=c(0,-0.03),cex=0.75)
# 
# plot(tVals[(1:(nt-1))],new_lambdaVals[[1]][(1:(nt-1))],
#      type='l',
#      xlab="Time (t)",ylab="Adjoint variables",
#      ylim=c(0,10))
# lines(tVals[(1:(nt-1))],new_lambdaVals[[2]][(1:(nt-1))],lty='dashed',col='red')
# lines(tVals[(1:(nt-1))],new_lambdaVals[[3]][(1:(nt-1))],lty='dotted',col='orange')
# lines(tVals[(1:(nt-1))],new_lambdaVals[[4]][(1:(nt-1))],lty='dotdash',col='green')
# 
# plot(tVals[(1:(nt-1))],new_lambdaVals[[1]][(1:(nt-1))],
#      type='l',
#      xlab="Time (t)",ylab="Adjoint variables",
#      xlim=c(2.5,5),ylim=c(0,.1))
# lines(tVals[(1:(nt-1))],new_lambdaVals[[2]][(1:(nt-1))],lty='dashed',col='red')
# lines(tVals[(1:(nt-1))],new_lambdaVals[[3]][(1:(nt-1))],lty='dotted',col='orange')
# lines(tVals[(1:(nt-1))],new_lambdaVals[[4]][(1:(nt-1))],lty='dotdash',col='green')
# 
# legend("topright",
#        c(expression(lambda[1]),expression(lambda[2]),expression(lambda[3]),expression(lambda[4])),
#        lty=c("solid","dashed","dotted","dotdash"),col=c("black","red","orange","green"),
#        bty="n", inset=c(0,0))
# dev.off()
