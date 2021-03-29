######################
# Load packages
######################
library(deSolve)
library(RColorBrewer)
library(tidyverse)
######################
# Identify settings
######################
# what ode method to use?
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

######################
# Write function to get state variable trajectory
######################

directory = "discretize-optimize/analysis/"

######################
# Get combination of development, meristem constraint, resource constraint
######################

files <- list.files(paste0("~/Dropbox/optimalControlProject/",directory))
files<-files[c(grep("determinate-uniform-",files,ignore.case=TRUE))]

df.list <- list()

# state <- function(development="unbranchedDeterminate",m=1,alpha=1,gamma=1){
#   
#   fileName <- list.files(paste0("~/Dropbox/optimalControlProject/",directory))
#   fileName <- fileName[c(grep("RDS",fileName))]
  n=length(files)
df <- data.frame(model=rep("model",n),
                 m1=rep(NA,n),
                 m2=rep(NA,n),
                 alpha=rep(NA,n),
                 gamma=rep(NA,n),
                 fitness=rep(NA,n),
                 stringsAsFactors=FALSE)

  for(j in 1:length(files)){
    
  tmp = readRDS(paste0("~/Dropbox/optimalControlProject/",directory,files[j]))
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",tmp$model,".R"))
  derivs=numeric(6); 
  
  n=length(tmp$u.list)
  initVals = c(tmp$inits[c("P","V","I","L")],c(pen=0,obj=0))
  mParms = tmp$inits[c("m1","m2","alpha","gamma")]
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=tmp$u.list[[n]],f2=tmp$beta1.list[[n]],f3=tmp$beta2.list[[n]]);

  df[j,]=data.frame(tmp$model,mParms[1],mParms[2],mParms[3],mParms[4],outMat[51,5],stringsAsFactors = FALSE)
}

df = df %>%
  dplyr::mutate(gamma=ifelse(model=="branchedDeterminate",1,
                             ifelse(model=="unbranchedDeterminate",0,gamma))) %>%
  dplyr::mutate(m = m1)



ggplot(df) +
  geom_point(aes(x=alpha,y=m)) +
  facet_wrap(~model) +
  theme_bw()

######################
# Calculate end of season fitness
######################


######################
# Make some plots!
######################
# 
# # remove filter if more sims are run
# ggplot(df %>% dplyr::filter(model!="determinate")) +
#   geom_line(aes(x=alpha,y=fitness,color=as.factor(m),linetype=model)) + 
#   #geom_point(aes(x=alpha,y=fitness,color=as.factor(m),shape=model)) +
#   facet_wrap(~model,scales='free_y') +
#   theme_bw() +
#   xlab("Resource constraint (alpha)") +
#   ylab("End-of-season fitness") +
#   labs(colour="Meristem constraint (m)")
# 
# ggplot(df %>% dplyr::filter(model!="determinate")) +
#   geom_line(aes(x=alpha,y=fitness,color=as.factor(m),linetype=model)) + 
#   # geom_point(aes(x=alpha,y=fitness,color=as.factor(m),shape=model)) +
#   theme_bw() +
#   xlab("Resource constraint (alpha)") +
#   ylab("End-of-season fitness") +
#   labs(colour="Meristem constraint (m)")
# 
# ggplot(df %>% dplyr::filter(model!="determinate")) +
#   geom_line(aes(x=alpha,y=fitness,color=as.factor(m),linetype=model)) + 
#   # geom_point(aes(x=alpha,y=fitness,color=as.factor(m),shape=model)) +
#   theme_bw() +
#   xlab("Resource constraint (alpha)") +
#   ylab("End-of-season fitness") +
#   labs(colour="Meristem constraint (m)")
# 
# ## resource constraint reduces end of season fitness
# ## effect of meristem constraint is strongest at low resource constraint
# ## which you can see by comparing end of season fitness at low alpha (no difference) vs. high alpha (strong meristem constraint reduces fitness)
# 
# ggplot(df %>% dplyr::filter(model!="determinate")) +
#   geom_line(aes(x=m,y=fitness,group=alpha,color=as.factor(alpha))) +
#   #geom_point(aes(x=m,y=fitness,group=alpha,color=as.factor(alpha))) +
#   facet_wrap(~model) +
#   theme_bw() +
#   xlab("Meristem constraint (m)") +
#   ylab("End-of-season fitness") +
#   labs(colour="Resource constraint (alpha)")
# 
# ggplot(df %>% dplyr::filter(model!="determinate")) +
#   geom_line(aes(x=m,y=fitness,color=as.factor(alpha),linetype=model)) +
#   #geom_point(aes(x=m,y=fitness,group=alpha,color=as.factor(alpha),shape=model)) +
#   theme_bw() +
#   xlab("Meristem constraint (m)") +
#   ylab("End-of-season fitness") +
#   labs(colour="Resource constraint (alpha)")

## meristem constraint reduces end of season fitness
## effect of resource constraint is strongest at low meristem constraint
## which you can see by comparing end of season fitness at low m (no difference) vs. high m (strong resource constraint reduces fitness)

dfPlot <- df

ggplot(dfPlot ) +
  geom_line(aes(x=gamma,y=fitness,group=m,color=m)) +
  # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=gamma,y=fitness,group=m,color=m)) +
  facet_wrap(~alpha,nrow=1,scale="free_y") +
  theme_bw() +
  xlab("Probability of branching") +
  ylab("End-of-season fitness") +
  labs(colour="Meristem constraint (m)") +
  scale_color_gradient(high="green", low="#542788")

ggplot(dfPlot ) +
  geom_line(aes(x=gamma,y=fitness,group=m,color=m)) +
  # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=gamma,y=fitness,group=m,color=m)) +
  facet_grid(m~alpha,scale='free_y') +
  theme_bw() +
  xlab("Probability of branching") +
  ylab("End-of-season fitness") +
  labs(colour="Meristem constraint (m)") +
  scale_color_gradient(high="green", low="#542788")

# at strong resource constraint, meristem constraint is irrelevant
# as resource constraint weakens, meristem constraint becomes more important
# in determining fitness

ggplot(dfPlot ) +
  geom_line(aes(x=gamma,y=fitness,group=alpha,color=alpha)) +
  geom_point(aes(x=gamma,y=fitness,color=alpha)) +
  facet_wrap(~m,nrow=1) +
  theme_bw() +
  xlab("Probability of branching") +
  ylab("End-of-season fitness") +
  labs(colour="Resource constraint (alpha)") +
  scale_color_gradient(high="orange", low="purple")

ggplot(dfPlot ) +
  geom_line(aes(x=m,y=fitness,group=gamma,color=gamma)) +
   geom_point(aes(x=m,y=fitness,color=gamma)) +
  facet_grid(gamma~alpha,scale='free') +
  theme_bw() +
  xlab("Meristem constraint (m)") +
  ylab("End-of-season fitness") +
  labs(colour="Probability of branching") +
  scale_color_gradient(high="orange", low="purple")


ggplot(dfPlot ) +
  geom_line(aes(x=alpha,y=fitness,group=gamma,color=gamma)) +
  geom_point(aes(x=alpha,y=fitness,color=gamma)) +
  facet_grid(m~gamma,scale='free') +
  theme_bw() +
  xlab("Resource constraint (alpha)") +
  ylab("End-of-season fitness") +
  labs(colour="Probability of branching") +
  scale_color_gradient(high="green", low="orange")

ggplot(df %>% dplyr::filter(m>.5) ) +
  geom_line(aes(x=alpha,y=fitness,group=gamma,color=gamma)) +
  geom_point(aes(x=alpha,y=fitness,color=gamma)) +
  facet_grid(gamma~m,scale='free') +
  theme_bw() +
  xlab("Resource constraint (alpha)") +
  ylab("End-of-season fitness") +
  labs(colour="Probability of branching") +
  scale_color_gradient(high="green", low="orange")

ggplot(dfPlot %>% dplyr::filter(m>.5&m %in% c(.75,1.25,1.5,2)) ) +
  geom_line(aes(x=alpha,y=fitness,group=gamma,color=gamma)) +
  geom_point(aes(x=alpha,y=fitness,color=gamma)) +
  facet_wrap(~m,scale='free',nrow=1) +
  theme_bw() +
  xlab("Resource constraint (alpha)") +
  ylab("End-of-season fitness") +
  labs(colour="Probability of branching") +
  scale_color_gradient(high="green", low="orange")



ggplot(dfPlot %>% dplyr::filter(m %in% c(.75,1.25,1.5,2)) ) +
  geom_line(aes(x=gamma,y=fitness,color=m,group=m)) +
  geom_point(aes(x=gamma,y=fitness,color=m)) +
  facet_wrap(~alpha,scale='free',nrow=1) +
  theme_bw() +
  xlab("Probability of branching") +
  ylab("End-of-season fitness") +
  labs(colour="Resource constraint") +
  scale_color_gradient(high="green", low="orange")

dfPlot2 = dfPlot %>% dplyr::filter(c(m1==1.5)) 
ggplot(dfPlot2) +
  geom_line(aes(x=alpha,y=fitness,group=gamma,color=gamma),size=1) +
  geom_point(aes(x=alpha,y=fitness,color=gamma),size=2) +
  theme_classic() +
  #xlim(c(0.2,1.3)) +
  xlab("Resource use efficiency") +
  ylab("End-of-season fitness") +
  labs(colour="Probability of branching") +
  scale_color_gradient(high="orange", low="forestgreen") +
  theme(text = element_text(size=20)) 
  



dfPlot2 = dfPlot %>% dplyr::filter(c(m1==1.5)) %>%
  dplyr::mutate(prob = as.factor(gamma))

par(mfrow=c(1,1),mar=c(5,6,4,2)+.1)
plot(NA, NA, axes = FALSE, type = 'n',xlim=c(0,1.5),ylim=c(0,35),
     xlab=expression("Efficiency of resource use"), ylab=expression("End-of-season fitness"),
     xaxs="i", yaxs="i",cex.lab=2)

axis(side=1, at = seq(0,1.25,.25),cex.axis=1.25)
axis(side=2, at = seq(0,35,5),cex.axis=1.2,labels=FALSE)

colors <- colorRampPalette(brewer.pal(5, "Spectral"))(5)
points(dfPlot2$alpha,dfPlot2$fitness,pch=16,col=colors)

branch=c(0,.25,.5,.75,1)
for(i in 1:5){
  dfPlotFiltered <- dfPlot2 %>% dplyr::filter(gamma == branch[i])
lines(dfPlotFiltered$alpha,dfPlotFiltered$fitness,pch=16,col=colors[i])
}

x = seq(0,1,by=.01)
#x = runif(100,0,1)
library(RColorBrewer)

colfunc <- colorRampPalette(c("purple", "orange","purple"))
colfunc(100)

df <-data.frame(x,f(x))

points(x,f(x),pch=16,cex=1,col=colfunc(100))

dfPlot <- dfPlot %>% 
  dplyr::filter(m %in% c(1,1.5))
test<-dfPlot[!duplicated(interaction(dfPlot$alpha,dfPlot$gamma,dfPlot$m)),]

ggplot( test) +
  geom_line(aes(x=alpha,y=fitness,group=gamma,color=gamma),size=1) +
  geom_point(aes(x=alpha,y=fitness,color=gamma),size=2) +
  facet_wrap(~m,nrow=1,scales='free_y') +
  theme_classic() +
  #xlim(c(0.2,1.3)) +
  xlab("Resource use efficiency") +
  ylab("End-of-season fitness") +
  labs(colour="Probability of branching") +
  scale_color_gradient(high="orange", low="forestgreen") +
  theme(text = element_text(size=20),legend.position='none') 

theme_classic() +
  #xlim(c(0.2,1.3)) +
  xlab("Resource use efficiency") +
  ylab("End-of-season fitness") +
  labs(colour="Probability of branching") +
  scale_color_gradient(high="orange", low="forestgreen") +
  theme(text = element_text(size=20)) 
