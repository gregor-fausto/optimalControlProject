### Optimal control for meristem limitation with rates ###
### Gregor Siegmund ###
### gs589@cornell.edu ###
### Help links
### http://www.talkstats.com/threads/constrained-optimization-constroptim.60583/

# clear history
rm(list=ls(all=TRUE))

setwd("~/Documents/optimalControlProject/development-model/") #### Edit as needed 

# load deSolve
library(deSolve)
library(compiler)
library(BB) # for projectLinear 

# for rate constraints
 source("king-roughgarden-funs-lowerBound.R"); 

################################################## 
# Construct the constraint matrix 
##################################################
# Number of time steps
# t is also used in control(), optim_fun(), f()
t = 5;

# Values of constraints c=d=4 for rates
# c=.001; d=.001;

## Block 1
## Constraint: u1+u2+u3-1 <= 0
## u4 positive
A1 = matrix(0,nrow=t,ncol=t);
diag(A1) = -1; c1 = rep(-1,t);

## Block 2
## Constraint: all uis positive
A2 = matrix(0,nrow=t,ncol=t);
diag(A2) = 1; c2 = rep(0,t);

Amat = rbind(A1,A2);
bvec = c(c1,c2);

###########################################################
# Starting conditions for ODEs and optimizer 
###########################################################
yA=c(x1=1,x2=45.4); 
seq_length = 100;
timesA=seq(0,t-1,length=seq_length);
parmsA=c(a=1);

v_in = rep(.1,t);

# check to make sure initial guess is acceptable 
# if FALSE ok to proceed
any(Amat %*% v_in - bvec <= 0)

## testing
# theta=v_in;
# times0=timesA
# y0=yA
# parms0=parmsA

###########################################################
# Use penalized optimization to improve the initial guess
###########################################################
penalty_factor=1000; 
# http://adv-r.had.co.nz/Profiling.html
fit <- optim(par=v_in, fn=penalized_fun, method="BFGS",
             control=list(maxit=2500,trace=1,REPORT=5))

theta=fit$par; 
for(j in 0:1000) {  
    theta=fit$par + (j/1000)*(v_in - fit$par) 
    z = min(Amat %*% theta - bvec)
    if(z>0) break
}    

###########################################################
# Optimize using constrOptim 
###########################################################

fit<-constrOptim(theta = theta, 
            f = optim_fun, 
            grad = optim_grad,
            ui = Amat,
            ci = bvec,
            method="BFGS",
            control=list(maxit=2500,trace=1,REPORT=1),
            outer.iterations = 20,
            outer.eps = 1e-06)

## Loop optimization
v <- c()
v[1] <- fit$value
stop_criteria = 1

while(0.00001 < stop_criteria){

fit <- constrOptim(theta = fit$par,
              f = optim_fun,
              grad = optim_grad,
              ui = Amat,
              ci = bvec,
              method="Nelder-Mead",control=list(maxit=10000,trace=4),
              outer.iterations = 20,
              outer.eps = 1e-04);

cat("Starting BFGS","\n");

fit <- constrOptim(theta = fit$par,
              f = optim_fun,
              grad = optim_grad,
              ui = Amat,
              ci = bvec,
              method="BFGS",control=list(maxit=2500,trace=1,REPORT=10),
              outer.iterations = 20,
              outer.eps = 1e-06);


v[2] <- fit$value;
stop_criteria = abs(v[2]-v[1])/(1+abs(v[1]));
v[1] = fit$value;
}

# extract pars
vals <- fit$par

# create matrix of u
d<-cbind(vals[1:t])

# plot u and allocation decision
par(mfrow=c(1,1))

# panel shows u trajectory 
plot(0:(t-1),d[,1],type="l",bty="n",col="black",
     xlab="Time (t)",
     ylim=c(0,1))

legend(80,1, legend=c("u"),
       col=c("black"), lty=c(1), cex=0.5)

mtext(paste0("Optimal growth and reproduction for an annual."), side = 3, line = -3, outer = TRUE)
