#####################################################################
## Script for optimization
#####################################################################

graphics.off(); 
require(deSolve); 
require(minqa); 

#####################################################################
## Time points for optimization
#####################################################################
topt=seq(0,5,length=11); 

#####################################################################
## Function that computes values of derivatives in the ODE system
## for the state variables, accumulated penalty, and accumulated 
## objective function. 
## 
## Parameters vector parms specifies control variable 
#####################################################################
derivs=numeric(6); 

# source control()
source(controlFile)

##############################################################
# Value function including penalty and objective function
##############################################################

# source optim_fun()
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/optim-fun.R")

##############################################################
# Initial conditions for control problem
# State variables, meristem constraints, penalty/objective 
##############################################################
source(initsFile)

##############################################################
# Objects to track control and solution
##############################################################
tMat.list <- list();
u.list <- list();
beta1.list <- list();
beta2.list <- list();
fvals <- c();
penVec <- c(); 
objVec <- c();

##############################################################
# Initial penalties for constraint violations and roughness
##############################################################
pwt=1; lambda=1; 

##############################################################
# Initial guesses for controls
##############################################################
# SPE: start with a 'do nothing' strategy for u and beta1, let the optimizer decide what to do
par0 = runif(2*length(topt),0.01,0.05); 
# beta2 is maxed-out at the end in the unconstrained problem (from analysis of adjoint equations)   
# so start with the max first
par0 = c(par0, mParms[2]*seq(0,1,length=length(topt))^2) 

##############################################################
# Optimization algorithm: step 1
##############################################################
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 
# increased maxstep size in optim_fun to 1e5
fit = optim(par0, fn=optim_fun, method="Nelder-Mead",control = list(maxit=5000,trace=4,REPORT=1));

mMin = rep(c(0,0,0),each=length(topt))
fit$par[fit$par<mMin] = mMin[fit$par<mMin]

mMax = rep(c(1),each=length(topt))
fit$par[1:length(topt)][(fit$par[1:length(topt)])>mMax] = mMax[(fit$par[1:length(topt)])>mMax]

# odemethod="impAdams_d"; # SPE: Adaptive, but perhaps more robust than lsoda. Try "impAdams" if it fails. 
odemethod="impAdams_d"; 
fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 

# Save first fit
tMat.list[[1]] = tMat = matrix(fit$par,ncol=3); 
u.list[[1]] = f1 = approxfun(topt,tMat[,1],rule=2);
beta1.list[[1]] = f2 = approxfun(topt,tMat[,2],rule=2); 
beta2.list[[1]] = f3 = approxfun(topt,tMat[,3],rule=2); 
fvals[1]=fit$value; 
outMat = ode(y=c(inits,other),times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3);
penVec[1] = outMat[nrow(outMat),"pen"]; # integrated constraint violation penalty 
objVec[1] = outMat[nrow(outMat),"obj"]; 

##############################################################
# Optimization algorithm: step 2
##############################################################
pwt=1; lambda=1; 

for(j in 2:5) {
  
  fit = optim(fit$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=2500,trace=4,REPORT=1));

  # minimum on u and betas
  fit$par[fit$par<mMin] = mMin[fit$par<mMin]
  # maximum on u
  fit$par[1:length(topt)][(fit$par[1:length(topt)])>mMax] = mMax[(fit$par[1:length(topt)])>mMax]
  
  fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
  cat("+++++++++++++++ Finished fit ", j, "   ",fit$fval,"\n");  
  
  lambda=lambda/2; 
  
  tMat.list[[j]] = tMat = matrix(fit$par,ncol=3); 
  u.list[[j]] = f1 = approxfun(topt,tMat[,1],rule=2);
  beta1.list[[j]] = f2 = approxfun(topt,tMat[,2],rule=2); 
  beta2.list[[j]] = f3 = approxfun(topt,tMat[,3],rule=2); 
  fvals[j]=fit$value; # replace fit$fvals with fit$value
  outMat = ode(y=c(inits,other),times=seq(0,5,by=0.1), control, method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3);
  penVec[j] = outMat[nrow(outMat),"pen"]; # integrated constraint violation penalty 
  objVec[j] = outMat[nrow(outMat),"obj"]; 
}


##############################################################
# Optimization algorithm: step 3
##############################################################
pwt=10; lambda=1; 

for(j in 6:10) {
  fit = optim(fit$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=2500,trace=4,REPORT=1));

  # minimum on u and betas
  fit$par[fit$par<mMin] = mMin[fit$par<mMin]
  # maximum on u
  fit$par[1:length(topt)][(fit$par[1:length(topt)])>mMax] = mMax[(fit$par[1:length(topt)])>mMax]
  
  # # check corner
  # tMat = matrix(fit$par,ncol=3); 
  # f1 = approxfun(topt,tMat[,1],rule=2);
  # f2 = approxfun(topt,tMat[,2],rule=2); 
  # f3 = approxfun(topt,tMat[,3],rule=2); 
	# outMat=ode(y=c(inits,other),times=topt,control,method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3)
  
  # binary = (mParms[3]*outMat[,3]-(f2(*outMat[,2]))/outMat[,4]>mParms[2]

  	
  # Vtot = (1/mParms[3])*(fit$par[12:22]*outMat[,2] + fit$par[23:33]*outMat[,4]); 
  # outMat[,3]-Vtot
  
  fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
  cat("+++++++++++++++ Finished fit ", j, "   ",fit$fval,"\n");  

  lambda=lambda/2
  
  tMat.list[[j]] = tMat = matrix(fit$par,ncol=3); 
  u.list[[j]] = f1 = approxfun(topt,tMat[,1],rule=2);
  beta1.list[[j]] = f2 = approxfun(topt,tMat[,2],rule=2); 
  beta2.list[[j]] = f3 = approxfun(topt,tMat[,3],rule=2); 
  fvals[j]=fit$value; # replace fit$fvals with fit$value
  outMat = ode(y=c(inits,other),times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3);
  penVec[j] = outMat[nrow(outMat),"pen"]; # integrated constraint violation penalty 
  objVec[j] = outMat[nrow(outMat),"obj"]; 
}

##############################################################
# Write out object
##############################################################
tmp<-strsplit(controlFile,c("-"))
controlName <- strsplit(tmp[[1]][3],"[.]")[[1]][1]

tmp<-strsplit(initsFile,c("-"))
tmp=paste(tmp[[1]][3:5],collapse="-")
initsName <- strsplit(tmp,"[.]")[[1]][1]

summaryObject <- list(fit=fit,fvals=fvals,
                      penVec=penVec,objVec=objVec,
                       u.list=u.list, 
                       beta1.list=beta1.list, 
                       beta2.list=beta2.list,
                      inits=c(inits,mParms),
                      season = ifelse(exists("mu"),c(mu=mu,sigma=sigma),NA),
                      topt = topt,
                      iterations=length(fvals),
                      model=controlName ,
                      initsName = initsName )

saveRDS(summaryObject,paste0("~/Dropbox/optimalControlProject/output/",controlName,"-",initsName,".RDS"))

