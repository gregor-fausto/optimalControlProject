mu = 50;
sigma = 1;
mesh <- seq(mu-3*sigma,mu+3*sigma,length.out=100);
###

x = 0:100
a = 1
b = 1
y = a + b*x

f_fun <- splinefun(x,y);

integrand <- function(x){
  dnorm(x,mean=mu,sd=sigma)*log(f_fun(x))
}


u = 0:100
temp = c()
for(i in 1:length(u)){
  temp[i] <- integrate(integrand,lower=mu-(3*sigma),upper=u[i])$value/(pnorm(mu+(3*sigma),mean=mu,sd=sigma)-pnorm(mu-(3*sigma),mean=mu,sd=sigma))
}

f_fun <- splinefun(x,y);

par(mfrow=c(3,3))
plot(x,f_fun(x),type='l')
plot(dnorm(0:100,50,10),type='l')
plot(0:100,temp,type='l')

x = 0:100
a = .01
b = .1
y = 1/(a+exp(-b*x))

f_fun <- splinefun(x,y);

integrand <- function(x){
  dnorm(x,mean=mu,sd=sigma)*log(f_fun(x))
}

u = 0:100
temp1 = c()
for(i in 1:length(u)){
  temp1[i] <- integrate(integrand,lower=mu-(3*sigma),upper=u[i])$value/(pnorm(mu+(3*sigma),mean=mu,sd=sigma)-pnorm(mu-(3*sigma),mean=mu,sd=sigma))
}
temp1

#par(mfrow=c(1,3))
plot(x,f_fun(x),type='l')
plot(dnorm(0:100,50,10),type='l')
plot(0:100,temp1,type='l')


x = 0:100
a = .01
b = .1
y = 1/(a+10*exp(-b*x))

f_fun <- splinefun(x,y);

integrand <- function(x){
  dnorm(x,mean=mu,sd=sigma)*log(f_fun(x))
}

u = 0:100
temp2 = c()
for(i in 1:length(u)){
  temp2[i] <- integrate(integrand,lower=mu-(3*sigma),upper=u[i])$value/(pnorm(mu+(3*sigma),mean=mu,sd=sigma)-pnorm(mu-(3*sigma),mean=mu,sd=sigma))
}
temp2

#par(mfrow=c(1,3))
plot(x,f_fun(x),type='l')
plot(dnorm(0:100,50,10),type='l')
plot(0:100,temp2,type='l')

par(mfrow=c(1,1))
plot(0:100,temp,type='l',ylim=c(0,10))
lines(0:100,temp1,col="red")
lines(0:100,temp2,col="blue")

### 



