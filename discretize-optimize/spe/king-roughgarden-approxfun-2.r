# solving King-Roughgarden by discretize and optimize, with
# the control constraint implemented by project and penalize. 

graphics.off(); 
require(deSolve); require(minqa); 

################
topt=seq(0,5,length=71); 

#####################################################################
## Function that computes values of derivatives in the ODE system
## for the state variables, accumulated penalty, and accumulated 
## objective function. 
## 
## Parameters vector parms specifies control variable 
#####################################################################
derivs=numeric(4); 
control <- function(t,y,parms,f1,f2) {
  
  # x1 and x2 are the two entries in y (ode)
  x1=y[1]; x2=y[2]; 

  u1=f1(t); u2=f2(t); 
  
  # apply positivity constraints, penalize if violated
  u1t = max(0,u1); bad = (u1-u1t)^2; 
  u2t = max(0,u2); bad = bad + (u2-u2t)^2; 
   
  
  # apply constraint u1 + u2 = x1, penalize if violated  
  utot= u1t + u2t; bad = bad + (utot-x1)^2; 
  u1t = x1*u1t/utot; u2t = x1*u2t/utot  
  
  ## cumulative penalty increases in proportion to squared constraint violation 
  derivs = c(u1t,u2t,bad,log(x2))
  
  return(list(derivs));
}

# testing 
control(t=0.2,y=c(1,1), parms=0,f1=approxfun(topt,runif(length(topt))),f2=approxfun(topt,runif(length(topt))) ); 

##############################################################
# Objective function including penalty and objective function
##############################################################
optim_fun = function(theta){

    tMat = matrix(theta,ncol=2); 
    f1 = approxfun(topt,tMat[,1],rule=2);
    f2 = approxfun(topt,tMat[,2],rule=2); 
    y0 = c(x1=1,x2=2,pen=0,obj=0) 
    out = ode(y=y0,times=seq(0,5,length=101),control,method=odemethod,parms=0,f1=f1,f2=f2)
    
    pen = out[nrow(out),"pen"]; # integrated constraint violation penalty 
    obj = out[nrow(out),"obj"]; 
    wiggly = diff(diff(tMat[,1])) + diff(diff(tMat[,2])); 
    val = obj - pwt*pen - lambda*mean(wiggly^2)
    return(-val)
}

## optimize: start with a large lambda, and decrease it with each iteration. 
pwt=1; lambda=0.2; fvals = numeric(5);  
par0 = rep(0.5,2*length(topt)); odemethod="rk4";  # lsoda has trouble at first, with bad parameter values 
fit = optim(par0, fn=optim_fun, method="Nelder-Mead",control = list(maxit=10000,trace=4,REPORT=1)); 
fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
fvals[1]=fit$fval;
for(j in 2:5) {
fit = optim(fit$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=10000,trace=4,REPORT=1)); 
odemethod="lsoda"; 
fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
cat("+++++++++++++++ Finished fit ", j, "   ",fit$fval,"\n");  
fvals[j]=fit$value;
lambda=lambda/2; 
}

graphics.off(); 
y0 = c(x1=1,x2=2,pen=0,obj=0) 
tMat = matrix(fit$par,ncol=2); 
f1 = approxfun(topt,tMat[,1],rule=2);
f2 = approxfun(topt,tMat[,2],rule=2); 
out = ode(y=y0,times=topt,func=control,parms=0,f1=f1,f2=f2); 
plot(out); 

dx1 = f1(topt); dx2=f2(topt); 
matplot(topt,cbind(dx1+dx2,out[,"x1"])); 

dev.new(); 
uMat = matrix(fit$par,ncol=2); u1 = uMat[,1]/apply(uMat,1,sum); 
plot(topt,u1,type="b",lty=1,col=c("red","blue")); 

