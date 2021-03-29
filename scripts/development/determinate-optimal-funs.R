# Function that computes values of derivatives in the ODE system Takes the time
# step, system of differential equations, parameters
derivs = numeric(4)
control <- function(times0, y, parms, f1, ...) {
  
  # P, V, I, L are the four entries in y (ODE)
  P = y[1]
  V = y[2]
  I = y[3]
  L = y[4]
  
  # control function calculated by f1 at different time points
  pt <- f1(times0)
  
  derivs[1] = 2 * pt * P - pt * P - (1 - pt) * P
  derivs[2] = pt * P + (1 - pt) * P
  derivs[3] = (1 - pt) * P
  derivs[4] = (1 - pt) * P + I
  
  ## other parameters beta1 = parms[1] beta2 = parms[2]
  
  # derivs[1]=2*beta1*pt*P - beta1*pt*P - (1-pt)*beta1*P; derivs[2]=beta1*pt*P +
  # (1-pt)*beta1*P; derivs[3]=(1-pt)*beta1*P; derivs[4]=(1-pt)*beta1*P + beta2*I;
  
  return(list(derivs))
}
control = cmpfun(control)

# Function that takes three vectors and conditions for ODE solver and returns the
# objective function (total reproduction by end of season) Uses a counter (shared
# with .GlobalEnv) to count function calls
optim_fun = function(theta, y0 = yA, times0 = timesA, parms0 = parmsA, constr0 = constrA) {
  
  par=theta; 
  #u1_fun <- approxfun(seq(0,Tf,length.out=nt),par[1:nt],rule=2,yright=0);
  u1_fun <- splinefun(seq(0,Tf,length.out=nt),par[1:nt], method = "monoH.FC");
  
  odeSolution = ode(y=y0,
                    times=times0,
                    control,
                    parms=parms0, atol=1e-7,
                    f1=u1_fun);
  
  Rep_vec = odeSolution[,5]
  Rep_vec_augmented = c(0,Rep_vec,max(Rep_vec));
  timesA_augmented = c(0-Tf,timesA,2*Tf);
  f_fun <- splinefun(timesA_augmented,Rep_vec_augmented,method="monoH.FC");
  
  mesh <- seq(0.0001, Tf ,length.out=1000);
  
  integrand <- function(x){
    log(f_fun(x))
  }
  
  # the penalty factor here is 1000
  min = if(any(f_fun(mesh)<=0)) 1000 else -1*((integrate(integrand,lower=0.00001,upper=Tf)$value)/(Tf))
  # min = -1*(integrate(integrand,lower=0,Tf)$value)
  return(min)
}
optim_fun = cmpfun(optim_fun)

# Function that takes three vectors and conditions for ODE solver and returns the
# objective function (total reproduction by end of season) Uses a counter (shared
# with .GlobalEnv) to count function calls

penalized_fun = function(theta, y0 = yA, times0 = timesA, parms0 = parmsA, constr0 = constrA) {
  
  theta.proj = projectLinear(theta, Amat, bvec, 0)
  par = theta.proj
  u1_fun <- approxfun( seq(0 ,Tf ,length.out=nt ) , par[1:nt], rule=2 ,yright=0);
  #u1_fun <- splinefun(seq(0,Tf,length.out=nt),par[1:nt], method = "monoH.FC");
  
  odeSolution = ode(y=y0,
                    times=times0,
                    func=control,
                    parms=parms0, 
                    atol=1e-7,
                    f1=u1_fun);
  
  Rep_vec = odeSolution[,5];
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
  
  return(min)
}
penalized_fun = cmpfun(penalized_fun)

# Function that takes three vectors and conditions for ODE solver and returns the
# gradient of the objective function by centered difference with increment eps
# (default eps=0.0001).
optim_grad = function(theta, y0 = yA, times0 = timesA, parms0 = parmsA, eps = 10^(-3)) {
  df = numeric(length(theta))
  for (j in 1:length(theta)) {
    new = theta
    new[j] = new[j] + eps
    up = optim_fun(new, y0 = yA, times0 = timesA, parms0 = parmsA)
    new[j] = new[j] - 2 * eps
    down = optim_fun(new, y0 = yA, times0 = timesA, parms0 = parmsA)
    df[j] = (up - down)/(2 * eps)
  }
  return(df)
}
optim_grad = cmpfun(optim_grad)
