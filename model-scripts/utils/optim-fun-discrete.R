optim_fun = function(theta){
  
  tMat = matrix(theta,ncol=3); 
  f1 = approxfun(topt,tMat[,1],rule=2);
  f2 = approxfun(topt,tMat[,2],rule=2); 
  f3 = approxfun(topt,tMat[,3],rule=2); 
  y0 = c(inits,other) 
  
  out = ode(y=y0,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3,maxsteps = 1e5);
  
  pen = out[nrow(out),"pen"]; # integrated constraint violation penalty 
  obj = out[nrow(out),"obj"]; 
  
  wiggly = sum(diff(diff(tMat[,1])), diff(diff(tMat[,2])), diff(diff(tMat[,3]))); 

  val = obj - pwt*pen - lambda*sum(wiggly^2)  ## SPE: sum instead of mean on wiggliness
  
  return(-val)
}