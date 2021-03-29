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
xA = c(P=1,V=.1,I=0,L=0)

# time steps
nt = 101

## time steps for ODE
tVals=seq(0,5,length=nt) # evaluation points 

### Starting point: initial guess at the optimal control 
pVals = rep(.1,nt); 
qVals = rep(1,nt);

## parameters in ODE
parmsA=c();

###########################################################
# ODE for state variables x
###########################################################
## The control u(t) must be an externally defined function [uFunc(t)]
derivs = numeric(4)
forward <- function(t, y, parms, f1, f2, ...) {
  
  # P, V, I, L are the four entries in y (ODE)
  P = y[1]
  V = y[2]
  I = y[3]
  L = y[4]
  
  # control function calculated by f1 at different time points
  pt <- f1(t)
  qt <- f2(t)
  
  derivs[1] = 2 * (qt * V) * pt * P - (qt * V) * pt * P - (qt * V) * (1 - pt) * P
  derivs[2] = (qt * V) * pt * P + (qt * V) * (1 - pt) * P
  derivs[3] = (qt * V) * (1 - pt) * P
  derivs[4] = (qt * V) * (1 - pt) * P + ((1 - qt) * V) * I
  
  return(list(derivs))
}
forward = cmpfun(forward)

###########################################################
# ODE for adjoint variables lambda
###########################################################
## The state x(t) must be an externally defined function [xFunc(t)]
derivs = numeric(4)
backward = function(t,y,parms,f1,f2, s1, s2,s3,s4) {
  
  lambda1=y[1];
  lambda2=y[2];
  lambda3=y[3];
  lambda4=y[4];
  
  pt = f1(t);
  qt = f2(t);
  
  P = s1(t);
  V = s2(t);
  I = s3(t);
  L = s4(t); 
  
  derivs[1] = -(V*(2*lambda1-lambda3-lambda4)*pt + V*(lambda4+lambda3+lambda2-lambda1))*qt
  derivs[2] = -(P*(2*lambda1-lambda3-lambda4)*pt + (P-I)*lambda4 + P*(lambda3+lambda2-lambda1))*qt-I*lambda4
  derivs[3] = V*lambda4-qt - V*lambda4
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

plot(tVals,pVals,type='n',ylim=c(0,1))
abline(v=tl,col='lightgray');abline(v=tf,col='lightgray')
lines(tVals,pVals,lty='solid')
lines(tVals,qVals,lty='dotted')

###########################################################
# Forward backward sweep
###########################################################
counter = 0
stop_criteria = 1

while(stop_criteria > 1e-5){
  
  # Using current u(t) solve forward for new x(t)
  pFunc=splinefun(tVals,pVals); 
  qFunc=splinefun(tVals,qVals); 
  
  out = ode(xA,tVals,forward,parms=parmsA, atol = 1e-7, f1 = pFunc, f2 = qFunc); 
  new_xVals = out[,c(2:5)]; 

  # Adjust reproductive output by season length distribution
  P_Func = approxfun(tVals,new_xVals[,1]);	
  V_Func = approxfun(tVals,new_xVals[,2]);	
  I_Func = approxfun(tVals,new_xVals[,3]);	
  L_Func = approxfun(tVals,new_xVals[,4]);	
  
  # Using current u(t) and x(t), solve backwards for new lambda(t) 
  # note, ode() will solve in reverse time 
  out = ode(c(0,0,0,0),rev(tVals),backward,parms=parmsA , f1 = pFunc, f2 = qFunc, s1=P_Func, s2=V_Func, s3=I_Func, s4=L_Func);
  new_lambdaVals = list(rev(out[,2]),rev(out[,3]), rev(out[,4]), rev(out[,5]) );
  
  # update control u(t), using Maximum Principle solution 
  switch_p = new_xVals[,1]*new_xVals[,2]*(2*new_lambdaVals[[1]]-new_lambdaVals[[3]]-new_lambdaVals[[4]]);
  switch_q = (new_xVals[[1]]-new_xVals[[3]])*new_lambdaVals[[4]]+new_xVals[,1]*new_xVals[,2]*(new_lambdaVals[[3]]+new_lambdaVals[[2]]-new_lambdaVals[[1]]);
  
  # new_uVals = ifelse(switch<0,0,1)
  new_pVals = pVals + 0.001*switch_p;
  new_qVals = qVals + 0.001*switch_q;
  
  new_pVals[new_pVals<0]=0;
  new_pVals[new_pVals>1]=1;
  
  new_qVals[new_qVals<0]=0;
  new_qVals[new_qVals>1]=1;
  
  # use constraints or bounds?
  delta = c(abs(pVals - new_pVals),abs(qVals - new_qVals));

  stop_criteria = max(delta);
  pVals = new_pVals;
  qVals = new_qVals;
  
  lines(tVals,pVals,lty='solid')
  lines(tVals,qVals,lty='dotted')
  
  counter = counter + 1;
}

par(mfrow=c(1,1))
plot(tVals,pVals,type='n',ylim=c(0,1))
abline(v=tl,col='lightgray');abline(v=tf,col='lightgray')
lines(tVals,pVals,lty='solid')
lines(tVals,qVals,lty='dotted')
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