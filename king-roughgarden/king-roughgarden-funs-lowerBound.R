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
  
  par=theta; 
  #u_fun <- approxfun(seq(0,Tf,length.out=nt),par[1:nt],rule=2,yright=0);
  u_fun <- splinefun(seq(0,Tf,length.out=nt),par[1:nt], method = "monoH.FC");
  
  odeSolution = ode(y=y0,
            times=times0,
            control,
            parms=parms0, atol=1e-7,
            f1=u_fun);
  
  Rep_vec = odeSolution[,3]
  Rep_vec_augmented = c(0,Rep_vec,max(Rep_vec));
  timesA_augmented = c(0-Tf,timesA,2*Tf);
  f_fun <- splinefun(timesA_augmented,Rep_vec_augmented,method="monoH.FC");

  mesh <- seq( 0.0001, Tf, length.out = 10000);
  
  integrand <- function(x){
    log(f_fun(x))
  }
  
  min = if(any(f_fun(mesh)<=0)) 1000 else -1*((integrate(integrand,lower=Tlo,upper=Tf)$value)/(Tf-Tlo));
  
  return(min); 
}
optim_fun=cmpfun(optim_fun); 

# Function that takes three vectors and conditions for ODE solver
# and returns the objective function (total reproduction by end of season) 
# Uses a counter (shared with .GlobalEnv) to count function calls

penalized_fun = function(theta ,y0=yA ,times0=timesA ,parms0=parmsA, penaltyFactor = penalty_factor){
  
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
    
    mesh <- seq( 0.0001, Tf, length.out = 10000)
    
    integrand <- function(x){
      log(f_fun(x))
    }
    
    min = if(any(f_fun(mesh)<=0)) 1000 else -1*((integrate(integrand,lower=Tlo,upper=Tf)$value)/(Tf-Tlo))
    
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

