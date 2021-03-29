loss_fun = function(theta,y0=yA,times0=timesA,parms0=parmsA){
  
  par=theta; 
  u_fun <- approxfun(0:(t-1),par[1:t],rule=2);
  
  out = ode(y=y0,
            times=times0,
            control,
            parms=parms0, atol=1e-7,
            f1=u_fun);
  
  R_vec = out[,3]
  R_vec_plus = c(0,R_vec,max(R_vec));
  timesA_plus = c(0-t,timesA,2*t);
  f_fun <- splinefun(timesA_plus,R_vec_plus,method="monoH.FC");
  
  mesh <- seq(0.00001, t ,length.out=1000);
  
  integrand <- function(x){
    log(f_fun(x))
  }
  
  min = if(any(f_fun(mesh)<=0)) 1000 else -1*((integrate(integrand,lower=0.00001,upper=t)$value)/(t-1))
  # min = -1*(integrate(integrand,lower=0,Tf)$value)
  
  return(min); 
}
penalized_fun=cmpfun(penalized_fun); 

# Creating inequality constraint function (this is much faster than my_ui %*% P - my_ci)
hin <- function(x){
  
  h = rep(NA,2)
  h[1] = 1 - x
  h[2] = x
  h
  
}

library(alabama)

P_init = c(rep(0.2, 25)) #Initial solution (theta)

aug_res <- auglag(P_init, loss_fun, hin = hin, 
                  gr = optim_grad, 
                  control.optim = list(maxit=2500,trace=1,REPORT=5))
