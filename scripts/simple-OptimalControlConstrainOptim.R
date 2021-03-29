### Optimal control for meristem limitation with rates ###
### Gregor Siegmund ###
### gs589@cornell.edu ###
### Help links
### http://www.talkstats.com/threads/constrained-optimization-constroptim.60583/

setwd("~/Dropbox/optimalControlProject/scripts") #### Edit as needed 

# clear history
rm(list=ls(all=TRUE))

# load deSolve
library(deSolve)
library(compiler)
library(BB) # for projectLinear 

# for rate constraints
source("simple-optimal-funs.R"); 
# for numeric constraints
# source("quantity-optimal-funs.R"); 

################################################## 
# Construct the constraint matrix 
##################################################
# Number of time steps
# t is also used in control(), optim_fun(), f()
t = 20

# Values of constraints c=d=4 for rates
# c=.001; d=.001;

## Block 1
## Constraint: u1+u2+u3-1 <= 0
## u4 positive
A1=f(-1,-1,-1,t=t); c1 = rep(-1,t);

## Block 2
## Constraint: all uis positive
A2 = matrix(0,nrow=t*3,ncol=t*3);
diag(A2) = 1; c2 = rep(0,t*3);

## Block 3
## Constraint: u2 <= c*u1
# A3=f(c,-1,0,t=t); c3 = rep(0,t);

## Block 4
## Constraint: u3 <= d*u4
# A4=f(1,1,1+d,t=t); c4 = rep(1,t);

# Amat = rbind(A1,A2,A3,A4);
# bvec = c(c1,c2,c3,c4);

Amat = rbind(A1,A2);
bvec = c(c1,c2);

###########################################################
# Starting conditions for ODEs and optimizer 
###########################################################
yA=c(G=.5, V=0.5, Fl=0, R=0); 
seq_length = 100;
timesA=seq(0,t-1,length=seq_length);
parmsA=c(a=1, h=.1);
constrA=c(c=4, d=4);

v_in = rep(.2,t*3);

# check to make sure initial guess is acceptable 
# if FALSE ok to proceed
any(Amat %*% v_in - bvec <= 0)

###########################################################
# Use penalized optimization to improve the initial guess
###########################################################
penalty_factor=1000; 
# http://adv-r.had.co.nz/Profiling.html
fit <- optim(v_in, fn=penalized_fun, method="BFGS",
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

# create matrix of uis and calculate u4
d<-cbind(vals[1:t],vals[(t+1):(2*t)],vals[(2*t+1):(3*t)])
d<-cbind(d, 1-d[,1]-d[,2]-d[,3])

# plot uis and allocation to vegetative vs. reproductive
par(mfrow=c(1,2))

# panel 1 shows each ui independently
plot(0:(t-1),d[,1],type="l",bty="n",col="black",
     xlab="Time (t)",
     ylim=c(0,1))

lines(0:(t-1),d[,2],type="l",col="black",bty="n",lty="dashed")
lines(0:(t-1),d[,3],type="l",col="black",bty="n",lty="dotted")
lines(0:(t-1),d[,4],type="l",col="black",bty="n",lty=4)

segments(50, -.1, 50, 0);
segments(20,-.1,20,0,lty=3);
segments(80,-.1,80,0,lty=3);

legend(2,1, legend=c("V meristem", "V veg","R meristem","R veg"),
       col=c("black", "black","black", "black"), lty=c(1,2,3,4), cex=0.5)

# panel 2 plots allocation of 
# vegetative and reproductive functions as 
# sum of meristem and biomass 
plot(0:(t-1),d[,1]+d[,2],type="l",bty="n",col="black",
     xlab="Time (t)",
     ylim=c(0,1))

lines(0:(t-1),d[,3]+d[,4],type="l",col="black",bty="n",lty="dashed")


segments(50, -.1, 50, 0);
segments(20,-.1,20,0,lty=3);
segments(80,-.1,80,0,lty=3);

legend(1,1, legend=c("veg", "rep"),
       col=c("black", "black"), lty=c(1,2), cex=0.5)

mtext(paste0("Optimal growth and reproduction for an annual (c=d=",constrA[1],")."), side = 3, line = -3, outer = TRUE)

# how to interpret this graph?
# is a better starting point for explanation doing this with a 2 pool model?
# talk about optimizing this code. 