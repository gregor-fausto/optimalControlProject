### Optimal control for King & Roughgarden 1982 ###
### Gregor Siegmund ###
### gs589@cornell.edu ###
### Help links
### http://www.talkstats.com/threads/constrained-optimization-constroptim.60583/
### increasing mesh size in integral helped solve problem


# clear history
# rm(list=ls(all=TRUE))

setwd("~/Dropbox/optimalControlProject/development-model") #### Edit as needed 

# load deSolve
library(deSolve)
library(compiler)
library(BB) # for projectLinear 

# for rate constraints
source("meristem-funs.R"); 

################################################## 
# Construct the constraint matrix 
##################################################
# Number of time steps
# t is also used in control() and optim_fun()
nt = 10;
#yA=c(x1=1,x2=.25); 

## Block 1
## Constraint: u-1 <= 0
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
yA=c(x1=1,x2=0,x3=0);

## time steps for ODE
seq_length = 100;
timesA=seq(0,Tf,length=seq_length);

## parameters in ODE
parmsA=c();

###########################################################
# Initial guess
###########################################################
# initial guess for control u over the interval
v_in = rep(rnorm(nt,0.1,0.001));

# check to make sure initial guess is acceptable 
# meaning does it satisfy the constraints 
# if FALSE ok to proceed
any(Amat %*% v_in - bvec <= 0)

###########################################################
# Use penalized optimization to improve the initial guess
###########################################################
penalty_factor=1000; 

fit <- optim(par=v_in, fn=penalized_fun, method="BFGS",
             control=list(maxit=2500,trace=1,REPORT=5))

# update the control u(t) 
theta=fit$par; 

# adjust the control u(t) to be greater than or equal to 0
for(j in 0:1000) {  
    theta=fit$par + (j/1000)*(v_in - fit$par) 
    z = min(Amat %*% theta - bvec)
    if(z>=0) break
}    

# plot(seq(0,Tf,length.out=nt),theta,type='l',lty='solid',ylim=c(0,1))

###########################################################
# Optimize using constrOptim 
###########################################################

# use the updated u(t) and solve the constrained optimization problem

fit<-constrOptim(theta = theta, 
            f = optim_fun, 
            grad = optim_grad,
            ui = Amat,
            ci = bvec,
            method="BFGS",
            control=list(maxit=2500,trace=1,REPORT=1),
            outer.iterations = 20,
            outer.eps = 1e-06)

# lines(seq(0,Tf,length.out=nt),fit$par,lty="dashed")

## Loop optimization
v <- c()
v[1] <- fit$value
stop_criteria = -1

while(stop_criteria < 0.0001){

  oldu = fit$par
  
cat("Starting Nelder-Mead","\n");

# use Nelder-Mead for the first solution to the optimization problem  
fit <- constrOptim(theta = fit$par,
              f = optim_fun,
              grad = optim_grad,
              ui = Amat,
              ci = bvec,
              method="Nelder-Mead",control=list(maxit=10000,trace=4),
              outer.iterations = 20,
              outer.eps = 1e-04);

cat("Starting BFGS","\n");

# use BFGS for the second solution to the optimization problem  
fit <- constrOptim(theta = fit$par,
              f = optim_fun,
              grad = optim_grad,
              ui = Amat,
              ci = bvec,
              method="BFGS",control=list(maxit=2500,trace=1,REPORT=10),
              outer.iterations = 20,
              outer.eps = 1e-06);

# update u with a convex combination of old and updated values
# as suggested by Lenhart and Workman
# fit$par = 0.5*(fit$par+oldu)

v[2] <- fit$value;
stop_criteria = abs(v[2]-v[1])/(1+abs(v[1]));

# use Lenhart and Workman test criteria for
# convergence of u(t); delta = 0.001
# stop_criteria = 0.001*sum(abs(fit$par)) - sum(abs(oldu-fit$par))

# update 
v[1] = fit$value;
}

# lines(seq(0,Tf,length.out=nt),fit$par,lty="dotted")

# c_fun<-function(yA){
#   C = as.vector(1/(yA[2]/yA[1]))
#   return(C)
# }
# u_kr <- function(t){
#   1-(1/(c_fun(yA)-t))
# }
# 
# tstar=5-2.793
# lines(seq(0,Tf,length.out=nt)[seq(0,Tf,length.out=nt)<tstar],u_kr(seq(0,Tf,length.out=nt))[seq(0,Tf,length.out=nt)<tstar],
#      ylim=c(0,1),type='l')
# 
# 
# extract pars
vals <- fit$par

# create matrix of u
d<-cbind(vals[1:nt])
# 
# ### Calculate switch point
y0 = yA
times0 = timesA
parms0 = parmsA
u = d[,1]
u_fun <- approxfun(seq(0,Tf,length.out=nt),u,rule=2);
# 
out<-ode(y=y0,
         times=times0,
         control,
         parms=parms0, atol=1e-6,
         f1=u_fun)
# 
repToVeg <- out[, 4] / out[, 2]
vegToRep <- out[, 2] / out[, 4]
tstar<-c()
tstar2<-c()
for(i in 1:length(times0)){
  tstar[i] <- ((1+repToVeg[i])*log(1+(5-times0[i])*vegToRep[i]))-(5-times0[i])
  tstar2[i] <- 5-(exp(vegToRep[i])-1)/(vegToRep[i])
}

plot(times0,tstar,type='l',ylim=c(-5,5));abline(h=0)
lines(times0,tstar2)
switchPoint<-times0[tstar > -0.01 & tstar < .01 ][1]
abline(v=switchPoint,lty='dotted')
# ####
# 
# # plot u and allocation decision
# 
# #setwd("~/Dropbox/optimalControlProject/king-roughgarden")
# #pdf(file=paste0("controlTrajectory-x1-",yA[1],"-x2-",yA[2],".pdf"), width=8, height=8)
# par(mfrow=c(2,2))
# 
# panel shows u trajectory
plot(seq(0,Tf,length.out=nt),d[,1],type="l",bty="n",col="black",
     xlab="Time (t)",
     ylab="Control u(t)",
     xlim = c(0,Tf),
     ylim=c(0,1))
# 
# # King and Roughgarden switch
# abline(v=2.207,lty='dotted');abline(v=switchPoint,lty='dashed',col="red")
# text(x=2.207, y=1, pos=4, labels=c('T*'))
# 
# # legend(.1,.1, legend=c("u"),
# #        col=c("black"), lty=c(1), cex=0.5)
# 
# title(paste0("Optimal growth and reproduction\n x1(0)=",yA[1],", x2(0)=",yA[2]),
#       cex.main=0.75)
# 
# # plot ODE
# 
derivs=numeric(3);
control <- function(times0,y,parms,f1,...) {
  
  # x1 and x2 are the two entries in y (ode)
  x1=y[1];
  x2=y[2];
  x3=y[3];
  
  # control function calculated f1 at different time points
  u <- f1(times0);
  
  derivs = c(2*u*x1,2*(1-u)*x1+x2,2*(1-u)*x1+x2) 
  return(list(derivs));
}
control=cmpfun(control);

y0 = yA
times0 = timesA
parms0 = parmsA
u = d[,1]
u_fun <- approxfun(seq(0,Tf,length.out=nt),u,rule=2);

out = ode(y=y0,
          times=times0,
          control,
          parms=parms0, atol=1e-6,
          f1=u_fun);

plot(out[,1],out[,2],type='l',
     xlab="Time (t)",
     ylab="Biomass",
     ylim=c(0,max(out[,2:4])))
lines(out[,1],out[,3],type='l',lty='dashed')
lines(out[,1],out[,4],type='l',lty='dotted',col='red')

legend(.1,100, legend=c("P","I","F"),
       col=c("black","black","red"), lty=c("solid","dashed","dotted"), cex=0.5)
# 
# # King and Roughgarden switch
# abline(v=2.207,lty='dotted');abline(v=switchPoint,lty='dashed',col="red")
# text(x=2.207, y=1, pos=4, labels=c('T*'))
# 
# title(paste0("Vegetative and reproductive \n biomass for an annual."),cex.main=.75)
# 
# 
# # plot tstar
# plot(times0,tstar,type='l',
#      xlab="Time (t)", ylab="");abline(h=0)
# abline(v=2.207,lty='dotted');abline(v=switchPoint,lty='dashed',col="red")
# 
# legend(.1,13.5, legend=c("v","r"),
#        col=c("black","black"), lty=c("solid","dashed"), cex=0.5)
# 
# title(paste0("Calculating t* from ratio of \n reproductive to vegetative weight (eqn. 26)"),cex.main=.75)
# 
# # adjoint variables
# x1 = ode(y=y0,
#          times=times0,
#          control,
#          parms=parms0, atol=1e-6,
#          f1=u_fun)[,2]
# x2 = ode(y=y0,
#          times=times0,
#          control,
#          parms=parms0, atol=1e-6,
#          f1=u_fun)[,3]
# 
# # switch point satisfying equation 24; does not include equation 26
# switchPoint<-times0[tstar > -0.01 & tstar < .01][1]
# 
# 
# 
# lambda2 = 1/x2
# x1.tstar = x1[times0==switchPoint]
# 
# lambda2 = (1/x1.tstar)*(log(x2[seq_length]/x2))
# lambda1 = (1/(x1.tstar^2))*(x2[seq_length]-x2-x2*log(x2[seq_length]/x2))
# 
# singularControl<-ifelse(times0<switchPoint,1,0)
# #singularControl=ifelse(u_fun(times0)>.1 & u_fun(times0)<0.9,1,0)
# lambda2.singular = 1/x2
# lambda2 = ifelse(singularControl==1,lambda2.singular,lambda2)
# lambda1 = ifelse(singularControl==1,lambda2.singular,lambda1)
# 
# plot(times0,lambda2,type='n',ylim=c(0,1), xlab="Time (t)", ylab="Adjoint variables")
# lines(times0,lambda1,lty='solid')
# lines(times0,lambda2,lty='dashed')
# 
# legend(4,5, legend=c("lambda1","lambda2"),
#        col=c("black","black"), lty=c("solid","dashed"), cex=0.5)
# 
# abline(v=switchPoint,lty='dashed')
# abline(v=times0[lambda2>lambda1][1])
# 
# title(paste0("Adjoint variables for \n x1(0)=",yA[1],", x2(0)=",yA[2]),
#       cex.main=0.75)
# 
# #dev.off()