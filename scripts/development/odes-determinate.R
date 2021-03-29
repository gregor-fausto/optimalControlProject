## Determinate inflorescence
rm(list=ls(all=TRUE))

# load deSolve
library(deSolve)

# Vector to hold the derivatives
derivs=numeric(4); 

# write ODEs
control=function(t,y,parms) {
  P=y[1]; V=y[2]; I=y[3]; L=y[4];  
  
  beta1=parms[1]; 
  beta2=parms[2];
  
  if ( t <= 4)
    pt <- 1
  else
    pt <- 0
  
  derivs[1]=2*beta1*pt*P - beta1*pt*P - (1-pt)*beta1*P;
  derivs[2]=beta1*pt*P + (1-pt)*beta1*P;
  derivs[3]=(1-pt)*beta1*P;
  derivs[4]=(1-pt)*beta1*P + beta2*I;
  return(list(derivs));
}

# starting values of both state variables
y0=c(P=0.5,V=0,I=0,L=0); 
# time sequence
times=seq(0,8,length=200)
# controls
parms=c( beta1=1, 
         beta2=1);
out=ode(y0,times,control,parms);

out = data.frame(out)

par(mfrow=c(1,1))
layout(matrix(c(1,2,3), 3, 1, byrow = TRUE),
       widths=c(2), heights=c(1,2,2))


plot(out$time,ifelse(out$time<4,1,0),type="l",col="black",bty="n",
     main="Optimal growth and reproduction for an annual\n with a determinate inflorescence.",
     xlab="Time (t)",
     ylab="Control",
     ylim=c(0,1))

plot(out$time,out$P,type="l",col="red",bty="n",
     main="Optimal growth and reproduction for an annual\n with a determinate inflorescence.",
     xlab="Time (t)",
     ylab="Available meristems",
     ylim=c(min(out[,2],out[,4]),max(out[,2],out[,4]))  )

lines(out$time,out$I,col="blue")
legend(x = 0, y = max(out[,2],out[,4]), 
       legend = c("Primary meristems (P)", "Inflorescence meristems (I)"), 
       col = c('red', 'blue') ,
       lty = c(1,1), 
       cex = .75)


plot(out$time,out$V,type="l",col="red",bty="n",
     main="Optimal growth and reproduction for an annual\n with a determinate inflorescence.",
     xlab="Time (t)",
     ylab="Biomass ",
     ylim=c(0,max(out[,3],out[,5])) )

lines(out$time,out$L,col="blue")
legend(x = 0, y = max(out[,3],out[,5]) ,
       legend = c("Vegetative biomass (V)", "Reproductive biomass (F)"),
       col = c('red', 'blue') ,
       lty = c(1,1),
       cex = .75)

# par(mfrow=c(1,1))
# plot(out$L)
