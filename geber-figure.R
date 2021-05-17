#devtools::install_github("tpoisot/digitize")
library(digitize)
mydata <- digitize("~/Dropbox/optimalControlProject/fig-1.jpeg")
mydata2 <-digitize.graph("~/Dropbox/optimalControlProject/fig-1.jpeg",log='x',
               x1=0,x2=12,y1=0,y2=1000)
plot(mydata$x,mydata$y)

df=cbind(age=c(1:12,1:12),veg=c(rep(0,12),rep(1,12)),mydata)
df

df.v=df[df$veg==1,]
df.r=df[df$veg==0,]

plot(df.v$age,df.v$y,pch=1,ylim=c(0,1200))

points(df.r$age,df.r$y,pch=16)

diff(df.v$y)

df.1=read.csv("~/Dropbox/optimalControlProject/fig-1.csv",header=FALSE)
df.1$V3=df.1$V2
plot(1:12,df.1$V2)

plot(1:11,diff(df.1$V2),xlab="Age (weeks)",ylab="New meristems since last week")
plot(1:11,diff(df.1$V2)/7,xlab="Age (weeks)",ylab="New meristems per day")


df.2=read.csv("~/Dropbox/optimalControlProject/fig-2.csv",header=FALSE)
df.2$V3=df.2$V2/7
plot(1:12,df.2$V2)

plot(1:11,diff(df.2$V2),xlab="Age (weeks)",ylab="New meristems since last week")
plot(1:11,diff(df.2$V2)/7,xlab="Age (weeks)",ylab="New meristems per day",ylim=c(0,100))


plot(1:11/5,diff(df.2$V2)/7,xlab="Age (weeks)",ylab="New meristems per day")
points(1:11/5,diff(df.1$V2)/7,xlab="Age (weeks)",ylab="New meristems per day",pch=16)
