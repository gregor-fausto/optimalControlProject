graphics.off(); rm(list=ls(all=TRUE)); 

require(deSolve); require(minqa); 

################
topt=seq(0,5,length=11); 

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
  m1=parms[1]; m2=parms[2];
  
  ## control functions calculated at different time points
  # probability of meristem division producing primary meristems
  u <- f1(t)
  # rate of primary meristem division
  beta1 <- f2(t)
  # rate of inflorescence meristem division
  beta2 <- f3(t)
  
  # apply positivity constraints, penalize if violated
  #SPE: change power from 2 to 1.25, so small errors are reduced less but penalty is still differentiable. 
  ut = max(u,0) ; bad = abs(u-ut)^1.25; 
  beta1t = max(beta1,0); bad = bad + abs(beta1-beta1t)^1.25;
  beta2t = max(beta2,0); bad = bad + abs(beta2-beta2t)^1.25;
  
  # apply upper bound to u constraint, penalize if violated
  ut2 = min(ut,1) ; bad = bad + abs(ut-ut2); 
  
  if (m1*P + m2*I <= V) {
    beta1t2 = min(beta1t, m1); bad = bad + abs(beta1t-beta1t2)^1.25; beta1t = beta1t2;
    beta2t2 = min(beta2t, m2); bad = bad + abs(beta2t-beta2t2)^1.25; beta2t = beta2t2;
  } else {
    # apply constraint beta1*P+beta2*I=V, penalize if violated  
    Vtot = beta1t*P + beta2t*I; 
    if (Vtot >= V) {
      # bad = bad + (Vtot-V)^2; 
      beta1t2 = beta1t*(V/Vtot); 
      beta2t2 = beta2t*(V/Vtot); 
      
      if(beta1t2>m1){
        beta1t2 = m1
        beta2t2 = (V - m1*P)/I
      } 
 
      if(beta2t2>m2){
        beta2t2 = m2
        beta1t2 = (V - m2*I)/P
      }
      
    } else {
      beta2t2 = beta2t;
      beta1t2 = beta1t;
    }  
    
    # penalize for being above maximum
    bad = bad + abs(beta1t-beta1t2)^1.25; beta1t = beta1t2;
    bad = bad + abs(beta2t-beta2t2)^1.25; beta2t = beta2t2;
    
  }  
  
  ## cumulative penalty increases in proportion to constraint violation 
  derivs[1] =  - (beta1t) * ((1 - ut2) * P)
  derivs[2] = (beta1t) * ( P )
  derivs[3] = (beta1t) * ((1 - ut2) * P) - (beta2t) * ( I )
  derivs[4] = (beta2t) * I
  derivs[5] = bad
  derivs[6] = ifelse(t>=2,log(L),0); # SPE: season's end is Uniform(2,5). 
  
  return(list(derivs));
}

# testing 
control(t=c(.5), y=c(1,.01,0,0.0001,0,0), parms=c(1,1), 
    f1=approxfun(topt,runif(length(topt))),
    f2=approxfun(topt,runif(length(topt))),
    f3=approxfun(topt,runif(length(topt)))
    ); 

##############################################################
# Objective function including penalty and objective function
##############################################################
inits = c(P=.4,V=.1,I=0,L=0)
other = c(pen=0,obj=0)
mParms = c(1,1)

optim_fun = function(theta){
  tMat = matrix(theta,ncol=3); 
  f1 = approxfun(topt,tMat[,1],rule=2);
  f2 = approxfun(topt,tMat[,2],rule=2); 
  f3 = approxfun(topt,tMat[,3],rule=2); 
  y0 = c(inits,other);  
  out = ode(y=y0,times=seq(0,5,by=0.1),func=control,method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3);
  pen = out[nrow(out),"pen"]; # integrated constraint violation penalty 
  obj = out[nrow(out),"obj"]; 
  wiggly = diff(diff(tMat[,1])) + diff(diff(tMat[,2])) + diff(diff(tMat[,3])); 
  val = obj - pwt*pen - lambda*sum(wiggly^2)  ## SPE: sum instead of mean on wiggliness
  return(-val)
}

## optimize: start with a large lambda, and decrease it with each iteration. 
# SPE: large penalty weight, and large lambda at first 
pwt=10; lambda=1; fvals = numeric(10);  

# SPE: start with a 'do nothing' strategy for u and beta1, let the optimizer decide what to do
par0 = runif(2*length(topt),0.01,0.05); 
# beta2 is maxed-out at the end in the unconstrained problem (from analysis of adjoint equations)   
par0 = c(par0, mParms[2]*seq(0,1,length=length(topt))^2) 

odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 
fit = optim(par0, fn=optim_fun, method="Nelder-Mead",control = list(maxit=5000,trace=4,REPORT=1));

# SPE: This is lazy: really (1, mParms[1], mParms[2])  should be used on u, beta1, beta2 separately.
mMax = max(c(mParms,1)); fit$par[fit$par>mMax] = mMax;  

odemethod="impAdams_d"; # SPE: Adaptive, but perhaps more robust than lsoda. Try "impAdams" if it fails.  
fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 

# SPE: plotting as we go 
tMat = matrix(fit$par,ncol=3); 
f1 = approxfun(topt,tMat[,1],rule=2);
f2 = approxfun(topt,tMat[,2],rule=2); 
f3 = approxfun(topt,tMat[,3],rule=2); 
par(mfrow=c(2,2)); 
plot(f1,min(topt),max(topt),xlab="Time",ylab="u(t)",main="u");
plot(f2,min(topt),max(topt),xlab="Time",ylab="beta1(t)",main="beta 1"); 
abline(h=mParms[1],col="blue",lty=2); 
plot(f3,min(topt),max(topt),xlab="Time",ylab="beta2(t)",main="beta 2"); 
abline(h=mParms[2],col="blue",lty=2); 

fvals[1]=fit$value; # replace fit$fvals with fit$value
for(j in 2:10) {
  fit = optim(fit$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=2500,trace=4,REPORT=1)); 
  fit$par[fit$par>mMax] = mMax; 
  fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
  cat("+++++++++++++++ Finished fit ", j, "   ",fit$fval,"\n");  
  fvals[j]=fit$value; # replace fit$fvals with fit$value
  lambda=lambda/2; 
  
    tMat = matrix(fit$par,ncol=3); 
    f1 = approxfun(topt,tMat[,1],rule=2);
    f2 = approxfun(topt,tMat[,2],rule=2); 
    f3 = approxfun(topt,tMat[,3],rule=2); 
    graphics.off(); dev.new(); 
    par(mfrow=c(2,2)); 
    plot(f1,min(topt),max(topt),xlab="Time",ylab="u(t)",main="u",type="o");
    plot(f2,min(topt),max(topt),xlab="Time",ylab="beta1(t)",main="beta 1");
    abline(h=mParms[1],col="blue",lty=2); 
    plot(f3,min(topt),max(topt),xlab="Time",ylab="beta2(t)",main="beta 2"); 
    abline(h=mParms[2],col="blue",lty=2); 
    out = ode(y=c(inits,other),times=seq(0,5,length=101),func=control,method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3);
    matplot(out[,1],out[,2:5],col=c("black","red","blue","purple"),lty=1,lwd=2,type="l"); 
    legend("topleft",legend=c("P","V","I","L"),col=c("black","red","blue","purple"),lty=1)
    cat("+++++++++++++++ Plotted ", j, "   ",fit$fval,"\n");  
}
