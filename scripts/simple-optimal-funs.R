# Function that computes values of derivatives in the ODE system
# Takes the time step, system of differential equations, parameters
derivs=numeric(4); 
control <- function(t,y,parms,constr,f1,f2,f3,...) {
  
  # Fl, G, V, and R are the four entries in y (ode)
  V=y[2]; 
  
  # a, h are the parameters in the (ode)
  a=parms[1]; 
  h=parms[2];
  
  # c, d are the constraints in the (ode)
  c=constr[1];
  d=constr[2];
  
  # values calculated by the functions at different time points
  u1 <- f1(t);
  u2 <- f2(t);
  u3 <- f3(t);
  u4 = 1 - (u1 + u2 + u3);
  
  # build constraints into differential equations
  u2 = min(u2,c*u1)
  u4 = min(u4,d*u3)
  
  P = (a*V)/(1+h*V);
  
  derivs = c(u1,u2,u3,u4)*P 
  return(list(derivs));
}
control=cmpfun(control); 

# Function that takes three vectors and conditions for ODE solver
# and returns the objective function (total reproduction by end of season) 
# Uses a counter (shared with .GlobalEnv) to count function calls
optim_fun = function(theta,y0=yA,times0=timesA,parms0=parmsA,constr0=constrA){
  par=theta; 
  u1_fun <- approxfun(0:(t-1),par[1:t],rule=2);
  u2_fun <- approxfun(0:(t-1),par[(t+1):(2*t)],rule=2);
  u3_fun <- approxfun(0:(t-1),par[(2*t+1):(3*t)],rule=2);
    
  out = ode(y=y0,
            times=times0,
            control,
            parms=parms0, constr=constr0, atol=1e-7,
            f1=u1_fun,f2=u2_fun,f3=u3_fun);
  
  R_vec = out[,5]
  R_vec_plus = c(0,R_vec,max(R_vec));
  timesA_plus = c(0-t,timesA,2*t);
  f_fun <- splinefun(timesA_plus,R_vec_plus,method="monoH.FC");

  mu = 50;
  sigma = 10;
  mesh <- seq(mu-3*sigma,mu+3*sigma,length.out=100);
  
  integrand <- function(x){
    dnorm(x,mean=mu,sd=sigma)*log(f_fun(x))
  }
  
  min = if(any(f_fun(mesh)<=0)) 1000 else -1*exp(integrate(integrand,lower=mu-(3*sigma),upper=mu+(3*sigma))$value/(pnorm(mu+(3*sigma),mean=mu,sd=sigma)-pnorm(mu-(3*sigma),mean=mu,sd=sigma)))
  # 
  # integrand <- function(x){
  #   
  #   if(any(f_fun(x)<0)) rep(0,length(x)) else dnorm(x,mean=50,sd=10)*log(f_fun(x))
  #   
  # }
  # 
  # R = -1*exp(integrate(integrand,lower=0,upper=t)$value/t);
  
  return(min); 
}
optim_fun=cmpfun(optim_fun); 

# Function that takes three vectors and conditions for ODE solver
# and returns the objective function (total reproduction by end of season) 
# Uses a counter (shared with .GlobalEnv) to count function calls

penalized_fun = function(theta,y0=yA,times0=timesA,parms0=parmsA,constr0=constrA){
  
    theta.proj = projectLinear(theta,Amat,bvec,0)
    par = theta.proj;
    u1_fun <- approxfun(0:(t-1),par[1:t],rule=2);
    u2_fun <- approxfun(0:(t-1),par[(t+1):(2*t)],rule=2);
    u3_fun <- approxfun(0:(t-1),par[(2*t+1):(3*t)],rule=2);
  
    out = ode(y=y0,
            times=times0,
            control,
            parms=parms0, constr=constr0, atol=1e-7,
            f1=u1_fun,f2=u2_fun,f3=u3_fun);
    
    R_vec = out[,5];
    R_vec_plus = c(0,R_vec,max(R_vec));
    timesA_plus = c(0-t,timesA,2*t);
    f_fun <- splinefun(timesA_plus,R_vec_plus);
    
    mu = 50;
    sigma = 10;
    mesh <- seq(mu-3*sigma,mu+3*sigma,length.out=100);
    
    integrand <- function(x){
      dnorm(x,mean=mu,sd=sigma)*log(f_fun(x))
    }
    
    min = if(any(f_fun(mesh)<=0)) 1000 else -1*exp(integrate(integrand,lower=mu-(3*sigma),upper=mu+(3*sigma))$value/(pnorm(mu+(3*sigma),mean=mu,sd=sigma)-pnorm(mu-(3*sigma),mean=mu,sd=sigma)))
    #integrand <- function(x){
      
    #  if(any(f_fun(x)<0)) rep(0,length(x)) else dnorm(x,mean=50,sd=10)*log(f_fun(x))

    #}
    
    #R = -1*exp(integrate(integrand,lower=0,upper=t)$value/t);
  
    return(min); 
}
penalized_fun=cmpfun(penalized_fun); 

# Function that takes three vectors and conditions for ODE solver
# and returns the gradient of the objective function by centered difference
# with increment eps (default eps=0.0001). 
optim_grad = function(theta,y0=yA,times0=timesA,parms0=parmsA,eps=10^(-3)){
        df = numeric(length(theta)); 
        for(j in 1:length(theta)){
            new=theta; new[j]=new[j]+eps;
            up=optim_fun(new,y0=yA,times0=timesA,parms0=parmsA)
            new[j]=new[j]-2*eps;
            down=optim_fun(new,y0=yA,times0=timesA,parms0=parmsA)
            df[j] = (up-down)/(2*eps); 
        }
        return(df)
}        
optim_grad=cmpfun(optim_grad);   
  
# Utility function for linear constraints. 
# This function builds matrices A1, A3, A4
f = function(diag1=1,diag2=1,diag3=1,t=9){

  A_rep1 = matrix(0,nrow=t,ncol=t);
  A_rep2 = matrix(0,nrow=t,ncol=t);
  A_rep3 = matrix(0,nrow=t,ncol=t);

  diag(A_rep1) = diag1;
  diag(A_rep2) = diag2;
  diag(A_rep3) = diag3;

  A1 = cbind(A_rep1,A_rep2,A_rep3);
  return(A1);

}

