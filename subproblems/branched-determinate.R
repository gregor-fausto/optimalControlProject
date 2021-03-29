### Gregor Siegmund ###
### gs589@cornell.edu ###

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

# initial conditions
xA = c(P=.1,V=.1,I=0,L=0)

# time steps
nt = 101

## time steps for ODE
tVals=seq(0,5,length=nt) # evaluation points 

### Starting point: initial guess at the optimal control 
pVals = rep(.25,nt)
qVals = rep(.25,nt)

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
  
  derivs[1] = (qt * V) * (pt * P) - (qt * V) * ((1 - pt) * P)
  derivs[2] = (qt * V) * (pt * P) + (qt * V) * ((1 - pt) * P)
  derivs[3] = (qt * V) * ((1 - pt) * P)
  derivs[4] = ((1 - qt) * V) * I
  
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
  
  derivs[1] = -((2*V*lambda1-V*lambda3)*pt+V*lambda3+V*lambda2-V*lambda1)*qt
  derivs[2] = -((2*P*lambda1-P*lambda3)*pt-I*lambda4+P*lambda3+P*lambda2-P*lambda1)*qt-I*lambda4
  derivs[3] = V*lambda4*qt - V*lambda4
  derivs[4] = -(1/L)
  
  return(list(derivs))
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
plot(tVals,pVals,type='n',ylim=c(0,1))
abline(v=tl,col='lightgray');abline(v=tf,col='lightgray')
lines(tVals,pVals,lty='solid')
lines(tVals,qVals,lty='dotted')

###########################################################
# Forward backward sweep
###########################################################
counter = 0
stop_criteria = 1

grad_p <- grad_q <-matrix(NA,nrow=nt,ncol=100)

#while(stop_criteria > 1e-5){
  for(i in 1:100){

# interpolate control functions
pFunc=approxfun(tVals,pVals,rule=2); 
qFunc=approxfun(tVals,qVals,rule=2); 

# Using current u(t) solve forward for new x(t)
out = ode(xA,tVals,forward,parms=parmsA, atol = 1e-7, f1 = pFunc, f2 = qFunc); 
new_xVals = out[,c(2:5)]; 

# interpolate state variables
P_Func = approxfun(tVals,new_xVals[,1],rule=2);
V_Func = approxfun(tVals,new_xVals[,2],rule=2);
I_Func = approxfun(tVals,new_xVals[,3],rule=2);
L_Func = approxfun(tVals,new_xVals[,4],rule=2);

# Using current u(t) and x(t), solve backwards for new lambda(t) 
out = ode(c(0,0,0,0),rev(tVals),backward,parms=parmsA ,atol = 1e-7,  f1 = pFunc, f2 = qFunc, s1=P_Func, s2=V_Func, s3=I_Func, s4=L_Func, method="euler");
new_lambdaVals = list(rev(out[,2]),rev(out[,3]), rev(out[,4]), rev(out[,5]) );

# update control using gradient, first p then q
switch_p = (new_xVals[,1]*new_xVals[,2])*(2*new_lambdaVals[[1]]-new_lambdaVals[[3]])*qFunc(tVals);
#grad_p[,i] <- switch_p
new_pVals = pVals + 0.025*switch_p;
new_pVals[new_pVals<0]=0;
new_pVals[new_pVals>1]=1;
delta_p = abs(pVals - new_pVals);
pVals = new_pVals;

switch_q = (new_xVals[,1]*new_xVals[,2])*(2*new_xVals[[1]]-new_xVals[[3]])*pFunc(tVals) - (new_xVals[,3]*new_xVals[,2])*new_lambdaVals[[4]]+(new_xVals[,1]*new_xVals[,2])*(new_lambdaVals[[3]]+new_lambdaVals[[2]]-new_lambdaVals[[1]]);
#grad_q[,i] <- switch_q
new_qVals = qVals + 0.025*switch_q;
new_qVals[new_qVals<0]=0;
new_qVals[new_qVals>1]=1;
delta_q = abs(qVals - new_qVals);
qVals = new_qVals;

# check for exiting while statement
stop_criteria = max(delta_p,delta_q);

lines(tVals,pVals,lty='solid')
lines(tVals,qVals,lty='dotted',col='red')

counter = counter + 1;
}

###########################################################
# Figures
# uncomment lines to save as pdf
###########################################################
# 
# ## Plot optimal controls
# pdf(file=paste0("~/Dropbox/optimalControlProject/figures/branched-determinate-P=",xA[1],"-V=",xA[2],",uniform(",mu-del,",",mu+del,").pdf"),width=4,height=4)
# par(mfrow=c(1,1))
plot(tVals[(1:(nt-1))],pVals[(1:(nt-1))],
     type='l',ylim=c(0,1),
     xlab="Time (t)",ylab="p(t) and q(t)",
     main=paste0("P=",xA[1],", V=",xA[2],", I=",xA[3],", F=",xA[4]))
lines(tVals[(1:(nt-1))],qVals[(1:(nt-1))],
      type='l',lty='dashed')
legend("topright",c("p","q"),lty=c('solid','dashed'),
       bty="n", inset=c(0,0),cex=0.75)
text(4.5,.75,labels=c(paste("iterations =",counter)),cex=0.5)
# dev.off()
# 
# ## Plot trajectories of state variables
# pdf(file=paste0("~/Dropbox/optimalControlProject/figures/branched-determinate-states.pdf"),width=6,height=6)
# par(mfrow=c(2,2))
# plot(tVals,new_xVals[,1],type='l',xlab="Time",ylab="Primary meristems (P)")
# plot(tVals,new_xVals[,2],type='l',xlab="Time",ylab="Vegetative biomass (V)")
# plot(tVals,new_xVals[,3],type='l',xlab="Time",ylab="Inflorescence meristems (I)")
# plot(tVals,new_xVals[,4],type='l',xlab="Time",ylab="Floral meristems (F)")
# dev.off()
# 
# ## Plot trajectories of adjoint variables
# ## NOTE: may need to ajust upper limit on ylim(c(0,x))
# pdf(file=paste0("~/Dropbox/optimalControlProject/figures/branched-determinate-adjoints.pdf"),width=6,height=6)
# par(mfrow=c(1,1))
# plot(tVals[(1:(nt-1))],new_lambdaVals[[1]][(1:(nt-1))],
#      type='l',
#      xlab="Time (t)",ylab="Adjoint variables",
#      ylim=c(0,100))
# lines(tVals[(1:(nt-1))],new_lambdaVals[[2]][(1:(nt-1))],lty='dashed',col='red')
# lines(tVals[(1:(nt-1))],new_lambdaVals[[3]][(1:(nt-1))],lty='dotted',col='orange')
# lines(tVals[(1:(nt-1))],new_lambdaVals[[4]][(1:(nt-1))],lty='dotdash',col='green')
# legend("topright",c(expression(lambda[1]),expression(lambda[2]),expression(lambda[3]),expression(lambda[4])),
#        lty=c('solid','dashed',"dotted","dotdash"), col = c("black","red","orange","green"),
#        bty="n", inset=c(0,0),cex=0.75)
# dev.off()
# 
# ## Summary figure with season distribution, controls, adjoints; uncomment all if needed 
# # pdf(file=paste0("~/Dropbox/optimalControlProject/figures/branched-determinate-P=",xA[1],"-V=",xA[2],",uniform(",mu-delta,",",mu+delta,").pdf"))
# # par(mfrow=c(2,2))
# # plot(tVals[(1:(nt-1))],seasonLength[(1:(nt-1))],
# #      type='n',ylim=c(0,1),
# #      xlab="Time",ylab="Frequency",
# #      main=paste0("P=",xA[1],", V=",xA[2]))
# # for(i in 1:length(tVals)){
# #   segments(x0=tVals[i],y0=0,x1=tVals[i],y1=seasonLength[i])
# # }
# # legend("topright",paste0("Uniform(",tl,",",tf,")"),
# #        bty='n', inset=c(0,-0.03),cex=0.75)
# # 
# # plot(tVals[(1:(nt-1))],pVals[(1:(nt-1))],
# #      type='l',ylim=c(0,1),
# #      xlab="Time (t)",ylab="u(t)")
# # lines(tVals[(1:(nt-1))],qVals[(1:(nt-1))],
# #       type='l',lty='dashed')
# # legend("topright",c(paste("iterations =",counter)),
# #        bty="n", inset=c(0,-0.03),cex=0.75)
# # 
# # plot(tVals[(1:(nt-1))],new_lambdaVals[[1]][(1:(nt-1))],
# #      type='l',
# #      xlab="Time (t)",ylab="Adjoint variables",
# #      ylim=c(0,10))
# # lines(tVals[(1:(nt-1))],new_lambdaVals[[2]][(1:(nt-1))],lty='dashed',col='red')
# # lines(tVals[(1:(nt-1))],new_lambdaVals[[3]][(1:(nt-1))],lty='dotted',col='orange')
# # lines(tVals[(1:(nt-1))],new_lambdaVals[[4]][(1:(nt-1))],lty='dotdash',col='green')
# # 
# # plot(tVals[(1:(nt-1))],new_lambdaVals[[1]][(1:(nt-1))],
# #      type='l',
# #      xlab="Time (t)",ylab="Adjoint variables",
# #      xlim=c(2.5,5),ylim=c(0,.1))
# # lines(tVals[(1:(nt-1))],new_lambdaVals[[2]][(1:(nt-1))],lty='dashed',col='red')
# # lines(tVals[(1:(nt-1))],new_lambdaVals[[3]][(1:(nt-1))],lty='dotted',col='orange')
# # lines(tVals[(1:(nt-1))],new_lambdaVals[[4]][(1:(nt-1))],lty='dotdash',col='green')
# # 
# # legend("topright",
# #        c(expression(lambda[1]),expression(lambda[2]),expression(lambda[3]),expression(lambda[4])),
# #        lty=c("solid","dashed","dotted","dotdash"),col=c("black","red","orange","green"),
# #        bty="n", inset=c(0,0))
# # dev.off()
