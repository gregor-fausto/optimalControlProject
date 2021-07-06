
# function to minimize  
f <- function(theta,alpha,L_0){
  tot = theta+(2/(alpha*L_0*exp(alpha*theta)))
  return(tot)
}

# RESOURCE CONSTRAINT ONLY
optim_fun = function(theta,alpha,L_0){
  
  val = theta+(2/(alpha*L_0*exp(alpha*theta)))
  
  return(val)
}

# plots
t=seq(0,20,by=.01)
par(mfrow=c(1,2))
alpha.vec=seq(0,2,by=0.01)
vec = optim_fun(t,alpha=alpha.vec[100],L_0=.02)
plot(t,vec,type='l')
points(t[which.min(vec)],min(vec),pch=16)
text(x=10,y=60,paste0("t=",signif(t[which.min(vec)],2),"; val=",signif(min(vec),2)))
optim.out=optimize(f=optim_fun, interval=c(0,1000), alpha=alpha.vec[100], L_0 = 0.02 )
text(x=10,y=50,paste0("min=",signif(optim.out$minimum,2),"; obj=",signif(optim.out$objective,2)))

# optimize across values of alpha
alpha.vec = seq(0.01,2,by=.01)
optimOutput = optimOutput2 = optimOutput3 = list()
for(i in 1:length(alpha.vec)){
  optimOutput[[i]] = optimize(f=optim_fun, interval=c(0,1000), alpha=alpha.vec[i], L_0 = 0.02 ); 
  optimOutput2[[i]] = optimize(f=optim_fun, interval=c(0,1000), alpha=alpha.vec[i], L_0 = 0.2 ); 
  optimOutput3[[i]] = optimize(f=optim_fun, interval=c(0,1000), alpha=alpha.vec[i], L_0 = 1.0 ); 
}

optimMin=unlist(lapply(optimOutput, `[[`, 1))
optimObj=unlist(lapply(optimOutput, `[[`, 2))

# Plots
pdf("~/Dropbox/optimalControlProject/products/figures/1d-resource-only.pdf",height=4,width=8)
par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
plot(alpha.vec,optimMin,type='l',
     xlab="Conversion rate of standing biomass\n (alpha)",
     ylab="Optimal switch time (theta)",ylim=c(0,20))
lines(alpha.vec,unlist(lapply(optimOutput2, `[[`, 1)),lty='dashed')
lines(alpha.vec,unlist(lapply(optimOutput3, `[[`, 1)),lty='dotted')

legend(x=1,y=20,c("L_0=0.02","L_0=0.2","L_0=1.0"),
       lty=c("solid","dashed","dotted"),cex=.75)

plot(alpha.vec,optimObj,type='l',
     xlab="Conversion rate of standing biomass\n (alpha)",
     ylab="Time to produce 1 flower (theta+tau1+tau2)",ylim=c(0,20))
lines(alpha.vec,unlist(lapply(optimOutput2, `[[`, 2)),lty='dashed')
lines(alpha.vec,unlist(lapply(optimOutput3, `[[`, 2)),lty='dotted')
mtext("Resource constraint only", outer = TRUE, cex = 1.25, side =3)
dev.off()
# 
# # MERISTEM AND RESOURCE CONSTRAINT
# leaf.deriv <- function(t,y,parms){
#   
#   # leaf biomass
#   Le = y[1];
#   
#   # parameters are meristem division rate and alpha
#   m0 = parms[1];
#   alpha = parms[2];
#   
#   # calculate rate at each instant in time
#   derivs[1] = min(alpha*Le,m0);
#   
#   return(list(derivs));
#   
# }
# 
# derivs=numeric(1); 
# 
# library(deSolve)
# optim_fun = function(theta,alpha,mParms,L_0){
#   
#   out = ode(y=L_0,times=seq(0,theta,by=0.1),leaf.deriv,
#               method="rk4",parms=c(mParms[1],alpha),maxsteps = 1e5);
#   
#   L_theta = out[,2][out[,1]==theta]
#   
#   tau_1 = 1/(min(mParms[2],alpha*L_theta))
#   tau_2 = 1/(min(mParms[3],alpha*L_theta))
#   
#   val = theta+tau_1+tau_2
#   
#   return(val)
# }
# 
# # plots
# t=seq(1,20,by=.01)
# par(mfrow=c(1,2))
# 
# vec=c()
# for(i in 1:length(t)){
#   vec[i]=optim_fun(theta=t[i],alpha=1,mParms=c(1,1,1),L_0=.02)
# }
# 
# plot(t,vec,type='p')
# points(t[which.min(vec)],min(vec),pch=16)
# text(x=10,y=60,paste0("t=",signif(t[which.min(vec)],2),"; val=",signif(min(vec),2)))
# 
# optim.out=optimize(f=optim_fun, interval=c(0,200), alpha=1, mParms=c(1,1,1), L_0 = 0.02 )
# text(x=10,y=50,paste0("min=",signif(optim.out$minimum,2),"; obj=",signif(optim.out$objective,2)))
# 
# # optimize across values of alpha
# alpha.vec = seq(0.01,2,by=.25)
# optimOutput = optimOutput2 = optimOutput3 = list()
# for(i in 1:length(alpha.vec)){
#   optimOutput[[i]] = optimize(f=optim_fun, interval=c(0,200), alpha=alpha.vec[i], mParms=c(1,1,1), L_0 = 0.02 ); 
#   optimOutput2[[i]] = optimize(f=optim_fun, interval=c(0,200), alpha=alpha.vec[i], mParms=c(1,1,1), L_0 = 0.2 ); 
#   optimOutput3[[i]] = optimize(f=optim_fun, interval=c(0,200), alpha=alpha.vec[i], mParms=c(1,1,1), L_0 = 1.0 ); 
# }
# 
# optimMin=unlist(lapply(optimOutput, `[[`, 1))
# optimObj=unlist(lapply(optimOutput, `[[`, 2))
# 
# # Plots
# #pdf("~/Dropbox/optimalControlProject/products/figures/1d-resource-only.pdf",height=4,width=8)
# par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
# plot(alpha.vec,optimMin,type='l',
#      xlab="Conversion rate of standing biomass\n (alpha)",
#      ylab="Optimal switch time (theta)",ylim=c(0,400))
# lines(alpha.vec,unlist(lapply(optimOutput2, `[[`, 1)),lty='dashed',col='red')
# lines(alpha.vec,unlist(lapply(optimOutput3, `[[`, 1)),lty='dotted',col='blue')
# 
# legend(x=0,y=400,c("L_0=0.02","L_0=0.2","L_0=1.0"),
#        lty=c("solid","dashed","dotted"),cex=.75)
# 
# plot(optimMin,optimObj,type='l',
#      ylab="Leaf biomass at theta",
#      xlab="Optimal switch time (theta)")
# lines(unlist(lapply(optimOutput2, `[[`, 1)),
#       unlist(lapply(optimOutput2, `[[`, 2)),lty='dashed',col='red')
# lines(unlist(lapply(optimOutput3, `[[`, 1)),
#       unlist(lapply(optimOutput3, `[[`, 2)),lty='dotted',col='blue')
# 
# mtext("Resource constraint only", outer = TRUE, cex = 1.25, side =3)
# #dev.off()
# 
# 
# # MERISTEM CONSTRAINT ONLY
# 
# alpha = seq(0,2,by=.1);
# L_0 = seq(0.02,.02,by=.01)
# vars=expand.grid(alpha=alpha,L_0=L_0)
# t=seq(0,5,.1)
# response = matrix(NA,nrow=dim(vars)[1],ncol=2)
# for(i in 1:dim(vars)[1]){
#   tot_time=f(t,vars$alpha[i],vars$L_0[i])
#   index=which.min(tot_time)
#   response[i,1] = index
#   response[i,2] = tot_time[index]
# }
# 
# response = cbind(L_0=vars$L_0,alpha=vars$alpha,response)
# response=data.frame(response)
# names(response)[3:4] = c("index","time")
# 
# ggplot(response) +
#   geom_line(aes(x=L_0,y=time,group=alpha,color=alpha)) +
#   geom_point(aes(x=1,y=2.3)) +
#   theme_bw() +
#   facet_wrap(~alpha,scales="free_y")
# 
# 
# # at low initial leaf biomass, increasing alpha immediately lowers switch
# # as initial leaf biomass 
# 
# # 
# # plot(t,f(t,1,1),type='l',ylim=c(0,5))
# # 
# # lines(t,f(t,.75,1),type='l')
# # lines(t,f(t,1.25,1),type='l')
# # lines(t,f(t,2,1),type='l')
# # 
# #      
# 
# # 
# alpha = seq(0,10,by=.1);
# L_0 = .02
# vars=expand.grid(alpha=alpha,L_0=L_0)
# t=seq(0,5,.01)
# response = matrix(NA,nrow=dim(vars)[1],ncol=3)
# times = list()
# for(i in 1:dim(vars)[1]){
#   tot_time=f(t,vars$alpha[i],vars$L_0[i])
#   index=which.min(tot_time)
#   L_theta = f2(tot_time[index],vars$alpha[i],vars$L_0[i])
#   response[i,1] = index
#   response[i,2] = tot_time[index]
#   response[i,3] = L_theta
#   times[[i]]=tot_time
# }
# 
# response = cbind(L_0=vars$L_0,alpha=vars$alpha,response)
# response=data.frame(response)
# names(response)[3:5] = c("index","time","L_theta")
# 
# plot(NA,xlim=c(0,5),ylim=c(0,100))
# for(i in 1:dim(vars)[1]){
#   lines(x=t,y=times[[i]])
#   index = which.min(times[[i]])
#   points(t[index],times[[i]][index],pch=16,col='red')
# }
# 
# 
# f2 <- function(t, alpha, L_0){
#   tot = (L_0*exp(alpha*t))
#   return(tot)
# }
# 
# alpha = seq(0,4,by=.01);
# L_0 = seq(0.02,2,by=.1)
# vars=expand.grid(alpha=alpha,L_0=L_0)
# t=seq(0,1,.01)
# response = matrix(NA,nrow=dim(vars)[1],ncol=2)
# for(i in 1:dim(vars)[1]){
#   tot_time=f(t,vars$alpha[i],vars$L_0[i])
#   index=which.min(tot_time)
#   response[i,1] = index
#   response[i,2] = tot_time[index]
# }
# 
# # following lindh 2016
# plot(t,f2(t,4,.02),type='l')
# lines(t,f2(t,2.9,.02),type='l')
# lines(t,f2(t,2.1,.02),type='l')
# 
# tot_time=f(t,4,.02)
# index=which.min(tot_time)
# 
# plot(f2(t,4,.02)[index],tot_time[index],xlim=c(0,4),ylim=c(0,15))
# 
# tot_time=f(t,2.9,.02)
# index=which.min(tot_time)
# points(f2(t,2.9,.02)[index],tot_time[index])
# 
# tot_time=f(t,2.1,.02)
# index=which.min(tot_time)
# points(f2(t,2.1,.02)[index],tot_time[index])
# 
# response = cbind(L_0=vars$L_0,alpha=vars$alpha,response)
# response=data.frame(response)
# names(response)[3:4] = c("index","time")
# 
# 
# library(ggplot2)
# ggplot(response) +
#   geom_line(aes(x=alpha,y=time,group=L_0,color=L_0)) +
#   theme_bw() +
#   facet_wrap(~L_0,scales="free_y")
# 
# # at low initial leaf biomass, increasing alpha immediately lowers switch
# # as initial leaf biomass 