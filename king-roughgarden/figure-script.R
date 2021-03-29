# Script to produce figures 

setwd("~/Dropbox/optimalControlProject/king-roughgarden")
pdf(file=paste0("controlTrajectory-x1-",yA[1],"-x2-",yA[2],".pdf"), width=8, height=4)
par(mfrow=c(1,2))

# panel shows u trajectory 
plot(seq(0,Tf,length.out=nt),d[,1],type="l",bty="n",col="black",
     xlab="Time (t)",
     ylab="Control u(t)",
     xlim = c(0,Tf),
     ylim=c(0,1))

# King and Roughgarden switch
abline(v=2.207,lty='dotted')
text(x=2.207, y=1, pos=4, labels=c('T*'))

# title
title(paste0("Optimal growth and reproduction\n x1(0)=",yA[1],", x2(0)=",yA[2]),
      cex.main=0.75)

# plot ODE
out = ode(y=y0,
          times=times0,
          control,
          parms=parms0, atol=1e-6,
          f1=u_fun); 

plot(out[,1],out[,2],type='l',
     xlab="Time (t)",
     ylab="Biomass",
     ylim=c(0,max(out[,2:3])))
lines(out[,1],out[,3],type='l',lty='dashed')

legend(.1,13.5, legend=c("v","r"),
       col=c("black","black"), lty=c("solid","dashed"), cex=0.5)

# King and Roughgarden switch
abline(v=2.207,lty='dotted')
text(x=2.207, y=1, pos=4, labels=c('T*'))

title(paste0("Vegetative and reproductive \n biomass for an annual."),cex.main=.75)
dev.off()