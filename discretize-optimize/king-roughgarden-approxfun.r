# solving King-Roughgarden by discretize and optimize, with
# the control constraint implemented by project and penalize. 

graphics.off(); 
require(deSolve); require(minqa); 

################
topt=seq(0,5,length=51); 

#####################################################################
## Function that computes values of derivatives in the ODE system
## for the state variables, accumulated penalty, and accumulated 
## objective function. 
## 
## Parameters vector parms specifies control variable 
#####################################################################
derivs=numeric(4); 
control <- function(t,y,parms,f1) {
  
  # x1 and x2 are the two entries in y (ode)
  x1=y[1]; x2=y[2]; 

  u=f1(t); 
  ut = max(0,min(u,1)); # adjust to satisfy the constraint 0 <= u <= 1. 
  
  ## the cumulative penalty increases in proportion to the squared constraint violation 
  derivs = c(ut*x1,(1-ut)*x1,(u-ut)^2,log(x2))
  
  return(list(derivs));
}

# testing 
control(t=0.2,y=c(1,1), parms=0,f1=approxfun(topt,runif(length(topt)))); 

##############################################################
# Objective function including penalty and objective function
##############################################################
optim_fun = function(theta){

    f1 = approxfun(topt,theta,rule=2);
    y0 = c(x1=1,x2=2,pen=0,obj=0) 
    out = ode(y=y0,times=seq(0,5,length=121),control,parms=0,f1=f1)
    
    pen = out[nrow(out),"pen"]; # integrated constraint violation penalty 
    obj = out[nrow(out),"obj"]; 
    wiggly = diff(diff(theta)); # wiggliness penalty 
    val = obj - pwt*pen - lambda*mean(wiggly^2)
    return(-val)
}

## optimize: start with a large lambda, and decrease it with each iteration. 
pwt=1; lambda=0.2; fvals = numeric(1);  
par0 = runif(length(topt));  

fit = optim(par0, fn=optim_fun, method="Nelder-Mead",control = list(maxit=5000,trace=4,REPORT=1)); 
fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
fvals[1]=fit$fval;
for(j in 2:5) {
fit = optim(fit$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=5000,trace=4,REPORT=1)); 
fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
cat("+++++++++++++++ Finished fit ", j, "   ",fit$fval,"\n");  
fvals[j]=fit$value;
lambda=lambda/2; 
}

graphics.off(); 
y0 = c(x1=1,x2=2,pen=0,obj=0) 
out = ode(y=y0,times=seq(0,5,length=200),control,parms=0,f1 = approxfun(topt,fit$par,rule=2)); 
plot(out); 

dev.new(); 
plot(topt,fit$par,type="b"); 

