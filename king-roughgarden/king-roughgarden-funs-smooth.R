## Equation 1 & 2 in King & Roughgarden (TPB 1982)
## Function that computes values of derivatives in the ODE system
## Takes the time step, system of differential equations, parameters
derivs=numeric(2); 
control <- function(times0,y,parms,f1,...) {
  
  # x1 and x2 are the two entries in y (ode)
  x1=y[1]; 

  # control function calculated f1 at different time points
  u <- f1(times0);
  
  derivs = c(u*x1,(1-u)*x1) 
  return(list(derivs));
}
## Compiles the function control()
control=cmpfun(control); 

# Function that takes three vectors and conditions for ODE solver
# and returns the objective function (total reproduction by end of season) 
# Uses a counter (shared with .GlobalEnv) to count function calls
optim_fun = function(theta,y0=yA,times0=timesA,parms0=parmsA){
  
  par_raw = theta;
  
  # unsmoothed function
  u_fun1 <- approxfun( seq(0 ,Tf ,length.out=nt ) , par[1:nt], rule=2 , yright=0);
  
  # smoothed function
  loess_fun <- loess(  par[1:nt] ~ seq(0 ,Tf ,length.out=nt ) , control=loess.control(surface = "direct"));
  u_fun2 <- function(x){
    temp <- predict(loess_fun, newdata=x )
    return(temp)
  }
  par_smooth = u_fun2( seq(0 ,Tf ,length.out=nt ) )
  
  # ode with unsmoothed values
  odeSolution1 = ode(y=y0,
                     times=times0,
                     func=control,
                     parms=parms0, 
                     atol=1e-7,
                     f1=u_fun1);
  
  # ode with smoothed values
  odeSolution2 = ode(y=y0,
                     times=times0,
                     func=control,
                     parms=parms0, 
                     atol=1e-7,
                     f1=u_fun2);
  
  # raw values
  Rep_vec = odeSolution1[,3];
  Rep_vec_augmented = c(0,Rep_vec,max(Rep_vec));
  timesA_augmented = c(0-Tf,times0,2*Tf);
  f_fun <- splinefun(timesA_augmented,Rep_vec_augmented);
  
  mesh <- seq(0.0001, Tf ,length.out=1000);
  
  integrand <- function(x){
    log(f_fun(x))
  }
  
  # the penalty factor here is 1000
  min1 = if(any(f_fun(mesh)<=0)) 1000 else -1*((integrate(integrand,lower=0.00001,upper=Tf)$value)/(Tf))
  # min = -1*(integrate(integrand,lower=0,Tf)$value)
  
  # smooth values
  Rep_vec = odeSolution2[,3];
  Rep_vec_augmented = c(0,Rep_vec,max(Rep_vec));
  timesA_augmented = c(0-Tf,times0,2*Tf);
  f_fun <- splinefun(timesA_augmented,Rep_vec_augmented);
  
  mesh <- seq(0.0001, Tf ,length.out=1000);
  
  integrand <- function(x){
    log(f_fun(x))
  }
  
  # the penalty factor here is 1000
  min2 = if(any(f_fun(mesh)<=0)) 1000 else -1*((integrate(integrand,lower=0.00001,upper=Tf)$value)/(Tf))
  # min = -1*(integrate(integrand,lower=0,Tf)$value)
  
  # keep smaller objective function and associated parameters
  min = ifelse(min1<min2, min1, min2);
  
  par = ifelse(min1<min2,par_raw,par_smooth);
  
  return(min); 
}
optim_fun=cmpfun(optim_fun); 

# Function that takes three vectors and conditions for ODE solver
# and returns the objective function (total reproduction by end of season) 
# Uses a counter (shared with .GlobalEnv) to count function calls

penalized_fun = function(theta ,y0=yA ,times0=timesA ,parms0=parmsA, penaltyFactor = penalty_factor ){
  
  theta.proj = projectLinear(theta, Amat, bvec ,0 )
  par = theta.proj;
  #u_fun <- approxfun( seq(0 ,Tf ,length.out=nt ) , par[1:nt], rule=2 ,yright=0);
  u_fun <- splinefun(seq(0,Tf,length.out=nt),par[1:nt], method = "monoH.FC");
  
  
  odeSolution = ode(y=y0,
                    times=times0,
                    func=control,
                    parms=parms0, 
                    atol=1e-7,
                    f1=u_fun);
  
  Rep_vec = odeSolution[,3];
  Rep_vec_augmented = c(0,Rep_vec,max(Rep_vec));
  timesA_augmented = c(0-Tf,times0,2*Tf);
  f_fun <- splinefun(timesA_augmented,Rep_vec_augmented);
  
  mesh <- seq(0.0001, Tf ,length.out=1000);
  
  integrand <- function(x){
    log(f_fun(x))
  }
  
  # the penalty factor here is 1000
  min = if(any(f_fun(mesh)<=0)) 1000 else -1*((integrate(integrand,lower=0.00001,upper=Tf)$value)/(Tf))
  # min = -1*(integrate(integrand,lower=0,Tf)$value)
  
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

