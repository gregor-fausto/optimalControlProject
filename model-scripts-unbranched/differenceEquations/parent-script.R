
analysisName = c("analysisOne")

####################################
## Write inits into table of parameters
####################################

library(tidyverse)

model = c('determinate')
Ve = c(1)
Le = c(1)
In = c(0)
Fl = c(0)
m = c(1)
alpha = c(1)
seasonDistribution = c("uniform")
mu = c(NA)
sd = c(NA)
max = c(60)
min = c(30)
gamma = c(0)

df<-data.frame(expand.grid(model=model,
                           Ve=Ve,Le=Le,In=In,Fl=Fl,
                           m1=m,alpha=alpha,
                           seasonDistribution=seasonDistribution,
                           mu=mu,sd=sd,max=max,min=min,gamma=gamma)) %>%
  dplyr::mutate(m2=m1) %>%
  dplyr::select(model,Ve,Le,In,Fl,m1,alpha,seasonDistribution,mu,sd,max,min,m2,gamma)

####################################
## Build inits object
####################################

# function builds a list of initial conditions
buildInits <-function(tmp){
  inits = c(Ve=tmp$Ve,Le=tmp$Le,In=tmp$In,Fl=tmp$Fl)
  other = c(pen=0,obj=0)
  mParms = c(m1=tmp$m1,m2=tmp$m2,alpha=tmp$alpha,gamma=tmp$gamma)
  dist = c(distribution=as.character(tmp$seasonDistribution))
  seasonParms = c(max=tmp$max,min=tmp$min)
  return(list(inits=inits,other=other,mParms=mParms,distribution=dist,seasonParms=seasonParms))
}

#####################################################################
## Script for optimization
#####################################################################

graphics.off(); 
require(deSolve); 
require(minqa); 

seasonEnd = buildInits(tmp)$seasonParms["max"]

#####################################################################
## Time points for optimization
#####################################################################
topt=seq(0,seasonEnd,by=1); 

#####################################################################
## Function that computes values of derivatives in the ODE system
## for the state variables, accumulated penalty, and accumulated 
## objective function. 
## 
## Parameters vector parms specifies control variable 
#####################################################################
derivs=numeric(6); 

# source control()
  controlFile = paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/differenceEquations/control-",tmp$model,".R")
  source(controlFile)

##############################################################
# Value function including penalty and objective function
##############################################################

# source optim_fun()
source("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/differenceEquations/optim-fun.R")

##############################################################
# Initial conditions for control problem
# State variables, meristem constraints, penalty/objective 
##############################################################
#source(initsFile)
initsList = buildInits(tmp)
inits=initsList[[1]]
other=initsList[[2]]
mParms=initsList[[3]]

##############################################################
# Objects to track control and solution
##############################################################
tSwitch.list <- list();
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
# par0 = runif(2*length(topt),0.01,0.99); 
# par0 = c(sample(c(0,1),121,replace=TRUE)
# beta2 is maxed-out at the end in the unconstrained problem (from analysis of adjoint equations)   
# so start with the max first

t_switch=runif(1,0,seasonEnd)
#t_switch = 30

par0 = c(t_switch,
         runif(length(topt),0.01,0.99),
         mParms[2]*seq(0,1,length=length(topt))^2)  

##############################################################
# Optimization algorithm: step 1
##############################################################
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

theta=par0

# increased maxstep size in optim_fun to 1e5
fit = optim(par0, fn=optim_fun, method="Nelder-Mead",control = list(maxit=5000,trace=4,REPORT=1));

mMin = c(0,rep(c(0,0),each=length(topt)))
fit$par[fit$par<mMin] = mMin[fit$par<mMin]

mMax = c(seasonEnd,rep(c(mParms[1:2]),each=length(topt)))
fit$par[fit$par>mMax] = mMax[fit$par>mMax]

# odemethod="impAdams_d"; # SPE: Adaptive, but perhaps more robust than lsoda. Try "impAdams" if it fails. 
odemethod="impAdams_d"; 
fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 

# Save first fit
tMat.list[[1]] = tMat = matrix(fit$par[2:length(par0)],ncol=2); 
tSwitch.list[[1]] = t_switch =  fit$par[1]
beta1.list[[1]] = f2 = approxfun(topt,tMat[,1],rule=2); 
beta2.list[[1]] = f3 = approxfun(topt,tMat[,2],rule=2); 
fvals[1]=fit$value; 
outMat = ode(y=c(inits,other),times=0:60,control,method=odemethod,parms=mParms,t_switch=t_switch,f2=f2,f3=f3);
penVec[1] = outMat[nrow(outMat),"pen"]; # integrated constraint violation penalty 
objVec[1] = outMat[nrow(outMat),"obj"]; 

par(mfrow=c(2,2))
plot(outMat[,1],outMat[,2],type='l')
plot(outMat[,1],outMat[,3],type='l')
plot(outMat[,1],outMat[,4],type='l')
plot(outMat[,1],outMat[,5],type='l')
##############################################################
# Optimization algorithm: step 2
##############################################################
pwt=1; lambda=1;

for(j in 2) {

  fit = optim(fit$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=2500,trace=4,REPORT=1));

  # minimum on u and betas
  fit$par[fit$par<mMin] = mMin[fit$par<mMin]
  # maximum on u and betas
  fit$par[fit$par>mMax] = mMax[fit$par>mMax]

  fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1));
  cat("+++++++++++++++ Finished fit ", j, "   ",fit$fval,"\n");

  lambda=lambda/2;

  tMat.list[[j]] = tMat = matrix(fit$par[2:length(fit$par)],ncol=2);
  tSwitch.list[[j]] = t_switch =  fit$par[1]
  beta1.list[[j]] = f2 = approxfun(topt,tMat[,1],rule=2);
  beta2.list[[j]] = f3 = approxfun(topt,tMat[,2],rule=2);
  fvals[j]=fit$value; # replace fit$fvals with fit$value
  outMat = ode(y=c(inits,other),times=seq(0,seasonEnd,by=0.1), control, method=odemethod,parms=mParms,t_switch=t_switch,f2=f2,f3=f3);
  penVec[j] = outMat[nrow(outMat),"pen"]; # integrated constraint violation penalty
  objVec[j] = outMat[nrow(outMat),"obj"];
}


##############################################################
# Optimization algorithm: step 3
##############################################################
pwt=10; lambda=1;

for(j in 3) {

  fit = optim(fit$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=2500,trace=4,REPORT=1));

  # minimum on u and betas
  fit$par[fit$par<mMin] = mMin[fit$par<mMin]
  # maximum on u and betas
  fit$par[fit$par>mMax] = mMax[fit$par>mMax]

  # check corner
  tMat = matrix(fit$par[2:length(fit$par)],ncol=2);
  t_switch = fit$par[1]
  f2 = approxfun(topt,tMat[,1],rule=2);
  f3 = approxfun(topt,tMat[,2],rule=2);
  outMat=ode(y=c(inits,other),times=topt,control,method=odemethod,parms=mParms,t_switch=t_switch,f2=f2,f3=f3)

	test = (mParms[3]*outMat[,3]-(mParms[1]*outMat[,2]))/outMat[,4]
	test2 = (mParms[3]*outMat[,3])/outMat[,2]

   binary = is.na(test>mParms[2])[test2>mParms[1]]

   fit$par[2:(length(topt)+1)][binary]=mMax[2:(length(topt)+1)][binary]
   fit$par[(length(topt)+2):(2*length(topt)+1)][binary]=mMax[(length(topt)+2):(2*length(topt)+1)][binary]

  # Vtot = (1/mParms[3])*(fit$par[12:22]*outMat[,2] + fit$par[23:33]*outMat[,4]);
  # outMat[,3]-Vtot

  fit = optim(fit$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1));
  cat("+++++++++++++++ Finished fit ", j, "   ",fit$fval,"\n");

  lambda=lambda/2

  tMat.list[[j]] = tMat = matrix(fit$par[2:length(fit$par)],ncol=2);
  tSwitch.list[[j]] = t_switch = fit$par[1]
  beta1.list[[j]] = f2 = approxfun(topt,tMat[,1],rule=2);
  beta2.list[[j]] = f3 = approxfun(topt,tMat[,2],rule=2);
  fvals[j]=fit$value; # replace fit$fvals with fit$value
  outMat = ode(y=c(inits,other),times=seq(0,seasonEnd,by=0.1),control,method=odemethod,parms=mParms,t_switch=t_switch,f2=f2,f3=f3);
  penVec[j] = outMat[nrow(outMat),"pen"]; # integrated constraint violation penalty
  objVec[j] = outMat[nrow(outMat),"obj"];
}
#
# ##############################################################
# # Write out object
# ##############################################################
# controlVars<-strsplit(controlFile,c("-"))
# controlName <- strsplit(controlVars[[1]][3],"[.]")[[1]][1]
# 
# gParm=ifelse(is.na(initsList[["mParms"]]["gamma"]),"",paste0("-",initsList[["mParms"]]["gamma"]))
# 
# #tmp<-strsplit(initsFile,c("-"))
# #tmp=paste(tmp[[1]][3:5],collapse="-")
# initsName <- paste0(initsList[["distribution"]],"-",initsList[["seasonParms"]]["max"],gParm,"-",initsList[["mParms"]]["m1"],"-",initsList[["mParms"]]["alpha"])
# #initsName <- strsplit(tmp,"[.]")[[1]][1]
# 
# summaryObject <- list(fit=fit,fvals=fvals,
#                       penVec=penVec,objVec=objVec,
#                        u.list=u.list,
#                        beta1.list=beta1.list,
#                        beta2.list=beta2.list,
#                       inits=c(inits,mParms),
#                       season = if(exists("mu")){
#                         c(mu,sigma)
#                         } else if(exists("max")){
#                           c(initsList[["seasonParms"]]["max"],initsList[["seasonParms"]]["min"])
#                         } else c(NA),
#                       topt = topt,
#                       iterations=length(fvals),
#                       model=controlName ,
#                       initsName = initsName )
# 
# 
# dir.create(file.path(paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"/")), showWarnings=FALSE)
# saveRDS(summaryObject,paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"/",initsName,".RDS"))
# #
# # ##############################################################
# # # Sensitivity analysis: reduce alpha
# # ##############################################################
# # pwt=1; lambda=1; 
# # 
# # initsList = buildInits(tmp)
# # inits=initsList[[1]]
# # other=initsList[[2]]
# # mParms=initsList[[3]]
# # 
# # mParms[["alpha"]]=mParms[["alpha"]]/.9
# # 
# # mMax = rep(c(1,mParms[1:2]),each=length(topt))
# # fit$par[fit$par>mMax] = mMax[fit$par>mMax]
# # 
# # for(j in 2) {
# #   
# #   fit.sens = optim(fit$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=2500,trace=4,REPORT=1));
# #   
# #   # minimum on u and betas
# #   fit.sens$par[fit.sens$par<mMin] = mMin[fit.sens$par<mMin]
# #   # maximum on u and betas
# #   fit.sens$par[fit.sens$par>mMax] = mMax[fit.sens$par>mMax]
# #   
# #   fit.sens = optim(fit.sens$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
# #   cat("+++++++++++++++ Finished fit ", j, "   ",fit.sens$fval,"\n");  
# #   
# #   lambda=lambda/2; 
# #   
# #   tMat.list[[j]] = tMat = matrix(fit.sens$par,ncol=3); 
# #   u.list[[j]] = f1 = approxfun(topt,tMat[,1],rule=2);
# #   beta1.list[[j]] = f2 = approxfun(topt,tMat[,2],rule=2); 
# #   beta2.list[[j]] = f3 = approxfun(topt,tMat[,3],rule=2); 
# #   fvals[j]=fit.sens$value; # replace fit$fvals with fit$value
# #   outMat = ode(y=c(inits,other),times=seq(0,seasonEnd,by=0.1), control, method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3);
# #   penVec[j] = outMat[nrow(outMat),"pen"]; # integrated constraint violation penalty 
# #   objVec[j] = outMat[nrow(outMat),"obj"]; 
# # }
# # 
# # 
# # # step 2
# # pwt=10; lambda=1; 
# # 
# # for(j in 3) {
# #   
# #   fit.sens = optim(fit.sens$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=2500,trace=4,REPORT=1));
# #   
# #   # minimum on u and betas
# #   fit.sens$par[fit.sens$par<mMin] = mMin[fit.sens$par<mMin]
# #   # maximum on u and betas
# #   fit.sens$par[fit.sens$par>mMax] = mMax[fit.sens$par>mMax]
# #   
# #   # check corner
# #   tMat = matrix(fit.sens$par,ncol=3);
# #   f1 = approxfun(topt,tMat[,1],rule=2);
# #   f2 = approxfun(topt,tMat[,2],rule=2);
# #   f3 = approxfun(topt,tMat[,3],rule=2);
# #   outMat=ode(y=c(inits,other),times=topt,control,method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3)
# #   
# #   test = (mParms[3]*outMat[,3]-(mParms[1]*outMat[,2]))/outMat[,4]
# #   test2 = (mParms[3]*outMat[,3])/outMat[,2]
# #   
# #   binary = is.na(test>mParms[2])[test2>mParms[1]]
# #   fit.sens$par[(length(topt)+1):(2*length(topt))][binary]=mMax[(length(topt)+1):(2*length(topt))][binary]
# #   fit.sens$par[(2*length(topt)+1):(3*length(topt))][binary]=mMax[(2*length(topt)+1):(3*length(topt))][binary]
# #
# #   # Vtot = (1/mParms[3])*(fit$par[12:22]*outMat[,2] + fit$par[23:33]*outMat[,4]); 
# #   # outMat[,3]-Vtot
# #   
# #   fit.sens = optim(fit.sens$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
# #   cat("+++++++++++++++ Finished fit ", j, "   ",fit.sens$fval,"\n");  
# #   
# #   lambda=lambda/2
# #   
# #   tMat.list[[j]] = tMat = matrix(fit.sens$par,ncol=3); 
# #   u.list[[j]] = f1 = approxfun(topt,tMat[,1],rule=2);
# #   beta1.list[[j]] = f2 = approxfun(topt,tMat[,2],rule=2); 
# #   beta2.list[[j]] = f3 = approxfun(topt,tMat[,3],rule=2); 
# #   fvals[j]=fit.sens$value; # replace fit$fvals with fit$value
# #   outMat = ode(y=c(inits,other),times=seq(0,seasonEnd,by=0.1),control,method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3);
# #   penVec[j] = outMat[nrow(outMat),"pen"]; # integrated constraint violation penalty 
# #   objVec[j] = outMat[nrow(outMat),"obj"]; 
# # }
# # 
# # ##############################################################
# # # Write out object
# # ##############################################################
# # controlVars<-strsplit(controlFile,c("-"))
# # controlName <- strsplit(controlVars[[1]][3],"[.]")[[1]][1]
# # 
# # gParm=ifelse(is.na(initsList[["mParms"]]["gamma"]),"",paste0("-",initsList[["mParms"]]["gamma"]))
# # 
# # initsName <- paste0(initsList[["distribution"]],"-",initsList[["seasonParms"]]["max"],gParm,"-",initsList[["mParms"]]["m1"],"-",initsList[["mParms"]]["alpha"])
# # 
# # summaryObject <- list(fit=fit.sens,fvals=fvals,
# #                       penVec=penVec,objVec=objVec,
# #                       u.list=u.list, 
# #                       beta1.list=beta1.list, 
# #                       beta2.list=beta2.list,
# #                       inits=c(inits,mParms),
# #                       season = if(exists("mu")){ 
# #                         c(mu,sigma) 
# #                       } else if(exists("max")){
# #                         c(initsList[["seasonParms"]]["max"],initsList[["seasonParms"]]["min"])
# #                       } else c(NA),
# #                       topt = topt,
# #                       iterations=length(fvals),
# #                       model=controlName ,
# #                       initsName = initsName )
# # 
# # dir.create(file.path(paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"/")), showWarnings=FALSE)
# # saveRDS(summaryObject,paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"/",controlName,"-",initsName,"-relaxAlpha.RDS"))
# # 
# # ##############################################################
# # # Sensitivity analysis: relax m
# # ##############################################################
# # pwt=1; lambda=1; 
# # 
# # initsList = buildInits(tmp)
# # inits=initsList[[1]]
# # other=initsList[[2]]
# # mParms=initsList[[3]]
# # 
# # mParms[["m1"]]=mParms[["m1"]]/.9
# # mParms[["m2"]]=mParms[["m2"]]/.9
# # 
# # mMax = rep(c(1,mParms[1:2]),each=length(topt))
# # fit$par[fit$par>mMax] = mMax[fit$par>mMax]
# # 
# # for(j in 2) {
# #   
# #   fit.sens = optim(fit$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=2500,trace=4,REPORT=1));
# #   
# #   # minimum on u and betas
# #   fit.sens$par[fit.sens$par<mMin] = mMin[fit.sens$par<mMin]
# #   # maximum on u and betas
# #   fit.sens$par[fit.sens$par>mMax] = mMax[fit.sens$par>mMax]
# #   
# #   fit.sens = optim(fit.sens$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
# #   cat("+++++++++++++++ Finished fit ", j, "   ",fit.sens$fval,"\n");  
# #   
# #   lambda=lambda/2; 
# #   
# #   tMat.list[[j]] = tMat = matrix(fit.sens$par,ncol=3); 
# #   u.list[[j]] = f1 = approxfun(topt,tMat[,1],rule=2);
# #   beta1.list[[j]] = f2 = approxfun(topt,tMat[,2],rule=2); 
# #   beta2.list[[j]] = f3 = approxfun(topt,tMat[,3],rule=2); 
# #   fvals[j]=fit.sens$value; # replace fit$fvals with fit$value
# #   outMat = ode(y=c(inits,other),times=seq(0,seasonEnd,by=0.1), control, method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3);
# #   penVec[j] = outMat[nrow(outMat),"pen"]; # integrated constraint violation penalty 
# #   objVec[j] = outMat[nrow(outMat),"obj"]; 
# # }
# # 
# # pwt=10; lambda=1; 
# # 
# # for(j in 3) {
# #   
# #   fit.sens = optim(fit.sens$par, fn=optim_fun, method="Nelder-Mead",control = list(maxit=2500,trace=4,REPORT=1));
# #   
# #   # minimum on u and betas
# #   fit.sens$par[fit.sens$par<mMin] = mMin[fit.sens$par<mMin]
# #   # maximum on u and betas
# #   fit.sens$par[fit.sens$par>mMax] = mMax[fit.sens$par>mMax]
# #   
# #   # check corner
# #   tMat = matrix(fit.sens$par,ncol=3);
# #   f1 = approxfun(topt,tMat[,1],rule=2);
# #   f2 = approxfun(topt,tMat[,2],rule=2);
# #   f3 = approxfun(topt,tMat[,3],rule=2);
# #   outMat=ode(y=c(inits,other),times=topt,control,method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3)
# #   
# #   test = (mParms[3]*outMat[,3]-(mParms[1]*outMat[,2]))/outMat[,4]
# #   test2 = (mParms[3]*outMat[,3])/outMat[,2]
# #   
# #   binary = is.na(test>mParms[2])[test2>mParms[1]]
# #   fit.sens$par[(length(topt)+1):(2*length(topt))][binary]=mMax[(length(topt)+1):(2*length(topt))][binary]
# #   fit.sens$par[(2*length(topt)+1):(3*length(topt))][binary]=mMax[(2*length(topt)+1):(3*length(topt))][binary]
# #
# #   # Vtot = (1/mParms[3])*(fit$par[12:22]*outMat[,2] + fit$par[23:33]*outMat[,4]); 
# #   # outMat[,3]-Vtot
# #   
# #   fit.sens = optim(fit.sens$par, fn=optim_fun, method="BFGS",control = list(maxit=1000,trace=4,REPORT=1)); 
# #   cat("+++++++++++++++ Finished fit ", j, "   ",fit.sens$fval,"\n");  
# #   
# #   lambda=lambda/2
# #   
# #   tMat.list[[j]] = tMat = matrix(fit.sens$par,ncol=3); 
# #   u.list[[j]] = f1 = approxfun(topt,tMat[,1],rule=2);
# #   beta1.list[[j]] = f2 = approxfun(topt,tMat[,2],rule=2); 
# #   beta2.list[[j]] = f3 = approxfun(topt,tMat[,3],rule=2); 
# #   fvals[j]=fit.sens$value; # replace fit$fvals with fit$value
# #   outMat = ode(y=c(inits,other),times=seq(0,seasonEnd,by=0.1),control,method=odemethod,parms=mParms,f1=f1,f2=f2,f3=f3);
# #   penVec[j] = outMat[nrow(outMat),"pen"]; # integrated constraint violation penalty 
# #   objVec[j] = outMat[nrow(outMat),"obj"]; 
# # }
# # 
# # ##############################################################
# # # Write out object
# # ##############################################################
# # controlVars<-strsplit(controlFile,c("-"))
# # controlName <- strsplit(controlVars[[1]][3],"[.]")[[1]][1]
# # 
# # gParm=ifelse(is.na(initsList[["mParms"]]["gamma"]),"",paste0("-",initsList[["mParms"]]["gamma"]))
# # 
# # initsName <- paste0(initsList[["distribution"]],"-",initsList[["seasonParms"]]["max"],gParm,"-",initsList[["mParms"]]["m1"],"-",initsList[["mParms"]]["alpha"])
# # #initsName <- strsplit(tmp,"[.]")[[1]][1]
# # 
# # summaryObject <- list(fit=fit.sens,fvals=fvals,
# #                       penVec=penVec,objVec=objVec,
# #                       u.list=u.list, 
# #                       beta1.list=beta1.list, 
# #                       beta2.list=beta2.list,
# #                       inits=c(inits,mParms),
# #                       season = if(exists("mu")){ 
# #                         c(mu,sigma) 
# #                       } else if(exists("max")){
# #                         c(initsList[["seasonParms"]]["max"],initsList[["seasonParms"]]["min"])
# #                       } else c(NA),
# #                       topt = topt,
# #                       iterations=length(fvals),
# #                       model=controlName ,
# #                       initsName = initsName )
# # 
# # 
# # dir.create(file.path(paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"/")), showWarnings=FALSE)
# # saveRDS(summaryObject,paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"/",controlName,"-",initsName,"-relaxMeristem.RDS"))
# # 
