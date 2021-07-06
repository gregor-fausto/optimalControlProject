# libraries
library(deSolve)

# time derivative of leaf biomass
leaf.deriv <- function(t,y,parms){
  
  # leaf biomass
  Le = y[1];
  
  # parameters are meristem division rate and alpha
  m0 = parms[1];
  alpha = parms[2];
  
  # calculate rate at each instant in time
  derivs[1] = min(alpha*Le,m0);
  
  return(list(derivs));
  
}

# leafODE = function(theta,alpha,mParms,L_0){
# 
#   return(L_theta)
# }

# value function
# solve ODE forward in time to theta
# then get value of leaf biomass at theta
# calculate tau1 and tau2
# add to theta and calculate the value
optim_fun = function(theta){
  
  out = ode(y=L_0,times=seq(0,theta,by=0.01),leaf.deriv,
            method="rk4",parms=c(mParms[1],alpha),maxsteps = 1e5,rtol = 1e-2);
  
  L_theta = max(out[,2])
  
  tau_1 = 1/(min(mParms[2],alpha*L_theta))
  tau_2 = 1/(min(mParms[3],alpha*L_theta))
  
  val = theta+tau_1+tau_2
  
  return(val)
}

# pass to the time derivative of leaf biomass
derivs=numeric(1); 
# 
# # Test the function
# mParms= c(1,1,1)
# alpha = 1
# 
# switch = 2.1
# L_0 = 0.02
# 
# # pass test to value function
# optim_fun(theta=switch)
# 
# # calculate value function over time
# t=seq(1,5,by=.1)
# 
# vec=vec2=vec3=c()
# for(i in 1:length(t)){
#   alpha=1
#   mParms=c(1,1,1)
#   L_0=.2
#   vec[i]=optim_fun(theta=t[i])
#   alpha=.1
#   mParms=c(.5,.5,.5)
#   vec2[i]=optim_fun(theta=t[i])
#   alpha=1
#   mParms=c(1.5,1.5,1.5)
#   vec3[i]=optim_fun(theta=t[i])
# }
# 
# par(mfrow=c(1,2))
# plot(t,vec,type='l',xlim=c(0,100),ylim=c(0,100))
# lines(t,vec2,type='l')
# lines(t,vec3,type='l')
# 
# t=seq(1,10,by=.05)
# 
# vec=vec2=vec3=c()
# for(i in 1:length(t)){
#   alpha=1
#   mParms=c(1,1,1)
#   L_0=.02
#   vec[i]=optim_fun(theta=t[i])
#   mParms=c(1,.1,1)
#   vec2[i]=optim_fun(theta=t[i])
#   mParms=c(1,1,.1)
#   vec3[i]=optim_fun(theta=t[i])
# }
# 
# plot(t,vec,type='l')
# lines(t,vec2,type='l',lty='dashed')
# lines(t,vec3,type='l',lty='dotted',col='red')
# 
# alpha=1; mParms=c(1,.5,.5); L_0 = 0.02
# optimize(f=optim_fun, interval=c(0,20)  )
# 
# 
# alpha.vec = seq(.5,1.5,by=.1)
# optimOutput = optimOutput2 = optimOutput3 = list()
# for(i in 1:length(alpha.vec)){
#   alpha=alpha.vec[i]
#   L_0 = .02
#   mParms=c(.5,.5,.5)
#   optimOutput[[i]] = optimize(f=optim_fun, interval=c(0,100)  )
#   
#   mParms=c(.75,.75,.75)
#   optimOutput2[[i]] = optimize(f=optim_fun, interval=c(0,100))
#   
#   mParms=c(1,1,1)
#   optimOutput3[[i]] = optimize(f=optim_fun, interval=c(0,100))
# }
# 
# par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
# plot(alpha.vec,unlist(lapply(optimOutput, `[[`, 1)),type='l',
#      xlab="Conversion rate of standing biomass\n (alpha)",
#      ylab="Optimal switch time (theta)")
# lines(alpha.vec,unlist(lapply(optimOutput2, `[[`, 1)),lty='dashed')
# lines(alpha.vec,unlist(lapply(optimOutput3, `[[`, 1)),lty='dotted')
# 
# thetaVals = unlist(lapply(optimOutput, `[[`, 1))
# thetaVals2 = unlist(lapply(optimOutput2, `[[`, 1))
# thetaVals3 = unlist(lapply(optimOutput3, `[[`, 1))
# 
# out=out2=out3=c()
# for(i in 1:length(alpha.vec)){
#    out[i] = leafODE(theta=thetaVals[i],alpha=alpha.vec[i],mParms=c(.5,.5,.5),L_0=0.02)
#    out2[i] = leafODE(theta=thetaVals2[i],alpha=alpha.vec[i],mParms=c(.75,.75,.75),L_0=0.02)
#    out3[i] = leafODE(theta=thetaVals3[i],alpha=alpha.vec[i],mParms=c(1,1,1),L_0=0.02)
# }
# 
# plot(thetaVals, out,type='l',
#      ylab="Leaf biomass at theta",
#      xlab="Optimal switch time (theta)",ylim=c(0,2),xlim=c(1,10))
# lines(thetaVals2,out2,lty='dashed')
# lines(thetaVals3,out3,lty='dotted')
# 
# mtext("Resource and meristem constraint", outer = TRUE, cex = 1.25, side =3)
# #dev.off()
# 

# grid
mVals = seq(.1,1,by=.05)
alphaVals = seq(.1,1,by=.05)
vars=expand.grid(m=mVals,alpha=alphaVals)

# to optimize the switch time with the ODE for leaf biomass
# optimize() should have a tolerance greater than the time steps of the ODE
# in other words: the 'tol' argument in optimize() > the time step in ode()
# if not, the function throws an error

listOut = list()
for(i in 1:nrow(vars)){
  alpha=vars$alpha[i]
  mParms=c(vars$m[i]/2,vars$m[i],vars$m[i])
  L_0 = 1
  listOut[[i]] <- optimize(f=optim_fun, interval=c(0,50), tol = 0.1 )
}

theta=unlist(lapply(listOut, `[[`, 1))
vars=cbind(vars[1:length(theta),],theta)

# library(ggplot2)
# ggplot(vars) +
#   geom_line(aes(x=alpha,y=theta,group=m))

vars=cbind(vars,theta=unlist(lapply(listOut, `[[`, 1)))

dev.off()
plot(vars[dplyr::near(vars$m,0.8),]$alpha,
     vars[dplyr::near(vars$m,0.8),]$theta,
     type='l')

thetaMat=matrix(vars$theta,nrow=length(alphaVals))

matrix.image=function(A, x=NULL, y=NULL, col=rainbow(100,start=0.67,end=0),
                      bw=FALSE, do.contour=FALSE, do.legend=TRUE,...) {
  if(do.legend) layout(mat=cbind(matrix(1,5,5),rep(2,5)));
  par(mar=c(6,5,3,2)); 
  if(is.null(x)) x=1:ncol(A);
  if(is.null(y)) y=1:nrow(A); 
  nx=length(x); ny=length(y); 
  x1=c(1.5*x[1]-0.5*x[2],1.5*x[nx]-0.5*x[nx-1]); 
  y1=c(1.5*y[1]-0.5*y[2],1.5*y[ny]-0.5*y[ny-1]); 
  if(bw) col=grey( (200:50)/200 ); 
  # comment out this line to reverse the direction of the plot
  #image(list(x=x,y=y,z=t(A)),xlim=x1,ylim=rev(y1),col=col,cex.axis=1.5,cex.lab=1.5,bty="u",...);
  image(list(x=x,y=y,z=t(A)),xlim=x1,ylim=(y1),col=col,cex.axis=1.5,cex.lab=1.5,bty="u",...);
  abline(a=0,b=1,col='white');
  abline(v=range(x1)); abline(h=range(y1)); 
  if(do.contour) contour(x,y,t(A),nlevels=5,labcex=1.2,add=TRUE);   
  
  if(do.legend) {
    l.y=seq(min(A),max(A),length=100);  
    par(mar=c(6,2,3,1))
    image(list(x=1:2,y=l.y,z=rbind(l.y,l.y)),col=col,bty="o",xaxt="n",yaxt="n"); 
    axis(side=2,cex.axis=1.5,at=pretty(seq(min(A),max(A),length=10))); 
  } 
}

pdf(paste0("~/Documents/optimalControlProject/products/figures/1d-",L_0,"-zoom.pdf"),height=6,width=6)
matrix.image(thetaMat,alphaVals,mVals,
             xlab="Conversion rate of standing biomass (alpha)",
             ylab="Per capita meristem division rate (m)",
             main=paste0("Optimal switch time for L_0=",L_0))



library(RColorBrewer)
#par(mar=c(3,4,2,2))
#display.brewer.all()
#colors=brewer.pal(length(mVals),"PuRd")
colors <- colorRampPalette(brewer.pal(9, "PuRd"))(length(mVals))

par(mfrow=c(1,1))
plot(NA,xlim=c(0,1),ylim=c(0,max(vars$theta)),
     xlab="Conversion rate of standing biomass (alpha)",
     ylab="Optimal switch time (theta)")
for(i in 1:length(mVals)){
  mSet=unique(vars$m)
  varsSub=vars[vars$m==mSet[i],]
  lines(varsSub$alpha,varsSub$theta,col=colors[i])
}

legend(x = 1.5, y = 30,
       title = "Meristem division rate",
       legend = c(min(mVals),median(mVals),max(mVals)),
       fill = colorRampPalette(brewer.pal(9, "PuRd"))(3),
       border = NA,
       y.intersp = 1,
       cex = 1, text.font = 1)

dev.off()
