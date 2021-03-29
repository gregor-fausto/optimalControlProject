
graphics.off(); 
require(deSolve); 
require(minqa); 

################
topt=seq(0,5,length=21); 

#xA = c(1,1,0,0)
#m1=m2=2

#####################################################################
## Function that computes values of derivatives in the ODE system
## for the state variables, accumulated penalty, and accumulated 
## objective function. 
## 
## Parameters vector parms specifies control variable 
#####################################################################
derivs=numeric(6); 
control <- function(t,y,parms,f1,f2,f3) {
  
  # P, V, I, L are the four entries in y (ODE)
  P = y[1]
  V = y[2]
  I = y[3]
  L = y[4]
  
  ## upper limit of meristem division rate
  # m1: maximum rate of primary meristem division
  # m2: maximum rate of inflorescence meristem division
  m1=parms[1];m2=parms[2];
  
  ## control functions calculated at different time points
  # probability of meristem division producing primary meristems
  u <- f1(t)
  # rate of primary meristem division
  beta1 <- f2(t)
  # rate of inflorescence meristem division
  beta2 <- f3(t)
  
  # apply positivity constraints, penalize if violated
  ut = max(u,0) ; bad = (u-ut)^2; 
  beta1t = max(beta1,0); bad = bad + (beta1-beta1t)^2;
  beta2t = max(beta2,0); bad = bad + (beta2-beta2t)^2;
  
  # apply upper bound to u constraint, penalize if violated
  ut2 = min(ut,1) ; bad = bad + (ut-ut2)^2; 
  
  if (m1*P + m2*I <= V) {
    beta1t2 = min(beta1t, m1); bad = bad + (beta1t-beta1t2)^2; beta1t = beta1t2;
    beta2t2 = min(beta2t, m2); bad = bad + (beta2t-beta2t2)^2; beta2t = beta2t2;
  } else if (m1*P + m2*I > V) {
    # apply constraint beta1*P+beta2*I=V, penalize if violated  
    Vtot = beta1t*P + beta2t*I; 
    # changed to >=
    if (Vtot >= V) {
      # bad = bad + (Vtot-V)^2; 
      beta1t2 = beta1t*(V/Vtot); 
      beta2t2 = beta2t*(V/Vtot); 
      
      if(beta1t2>m1) {
        beta1t2 = m1
        beta2t2 = (V - m1*P)/I
      } else if (beta2t2>m2){
        beta2t2 = m2
        beta1t2 = (V - m2*I)/P
      }
      
    }
    
    # penalize for being above maximum
    bad = bad + (beta1t-beta1t2)^2; beta1t = beta1t2;
    bad = bad + (beta2t-beta2t2)^2; beta2t = beta2t2;
    
  }  
  
  ## cumulative penalty increases in proportion to squared constraint violation 
  derivs[1] =  - (beta1t) * ((1 - ut2) * P)
  derivs[2] = (beta1t) * ( P )
  derivs[3] = (beta1t) * ((1 - ut2) * P) - (beta2t) * ( I )
  derivs[4] = (beta2t) * I
  derivs[5] = bad
  derivs[6] = log(L)
  
  return(list(derivs));
}

# testing 
control(t=c(.5), y=c(1,.01,0,0.0001,0,0), parms=c(1,1), 
        f1=approxfun(topt,runif(length(topt))),
        f2=approxfun(topt,runif(length(topt))),
        f3=approxfun(topt,runif(length(topt)))); 

##############################################################
# Objective function including penalty and objective function
##############################################################
inits = c(P=1,V=.01,I=0,L=0.0001)
other = c(pen=0,obj=0)
mParms = c(1,1)


optim_fun = function(theta){
  
  tMat = matrix(theta,ncol=3); 
  f1 = approxfun(topt,tMat[,1],rule=2);
  f2 = approxfun(topt,tMat[,2],rule=2); 
  f3 = approxfun(topt,tMat[,3],rule=2); 
  y0 = c(inits,other) 
  out = ode(y=y0,times=seq(0,5,length=11),control,method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3);
  
  pen = out[nrow(out),"pen"]; # integrated constraint violation penalty 
  obj = out[nrow(out),"obj"]; 
  wiggly = diff(diff(tMat[,1])) + diff(diff(tMat[,2])) + diff(diff(tMat[,3])); 
  val = obj - pwt*pen - lambda*mean(wiggly^2)
  return(-val)
}


#optim_fun(par0)

## optimize: start with a large lambda, and decrease it with each iteration. 
pwt=1; lambda=0.2; fvals = numeric(5);  
par0 = rep(0.5,3*length(topt)); odemethod="rk4";  # lsoda has trouble at first, with bad parameter values 
fit = optim(par0, fn=optim_fun, method="Nelder-Mead",control = list(maxit=10000,trace=4,REPORT=1)); 
optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
fvals[1]=fit$value; # replace fit$fvals with fit$value
for(j in 2:5) {
  fit = optim(fit$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=10000,trace=4,REPORT=1)); 
  odemethod="lsoda"; 
  fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
  cat("+++++++++++++++ Finished fit ", j, "   ",fit$fval,"\n");  
  fvals[j]=fit$value; # replace fit$fvals with fit$value
  lambda=lambda/2; 
}