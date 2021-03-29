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

directory = "discretize-optimize/analysisSeven/"

######################
# Get combination of development, meristem constraint, resource constraint
######################

files <- list.files(paste0(directory))
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
    
  tmp = readRDS(paste0(directory,files[j]))
  source(paste0("discretize-optimize/scriptsBatch/control-",tmp$model,".R"))
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
  facet_wrap(~gamma) +
  theme_bw()

######################
# Make some plots!
######################

dfPlot <- df

ggplot(dfPlot ) +
  geom_abline(slope=1,intercept=0,linetype='dotted',color='gray') +
  geom_point(aes(x=alpha,y=m,color=fitness)) +
  facet_wrap(~gamma,nrow=1,scale="free_y") +
  theme_bw() +
  xlim(c(0,1)) + ylim(c(0,1)) +
  xlab("alpha") +
  ylab("m") +
  labs(colour="Fitness") +
  scale_color_gradient(high="red", low="yellow")

ggplot(dfPlot ) +
  geom_abline(slope=c(.5,1,2),intercept=0,linetype='dotted',color='gray') +
  geom_abline(slope=1,intercept=0,linetype='dotted',color='gray') +
  geom_contour(aes(x=alpha,y=m,z=fitness,color=fitness),bins=20) +
  facet_wrap(~gamma,nrow=1,scale="free_y") +
  theme_classic() +
  xlim(c(0,1)) + ylim(c(0,1)) +
  xlab("alpha") +
  ylab("m") 

ggplot(dfPlot ) +
  geom_abline(slope=1,intercept=0,linetype='dotted') +
  geom_contour(aes(x=alpha,y=m,z=fitness,group=gamma,color=as.factor(gamma))) +
  theme_classic() +
  xlim(c(0,1)) + ylim(c(0,1)) +
  xlab("alpha") +
  ylab("m") 

ggplot(dfPlot ) +
  geom_abline(slope=1,intercept=0,linetype='dotted',color='gray') +
  geom_tile(aes(x=alpha,y=m,fill=fitness)) +
  facet_wrap(~gamma,nrow=1,scale="free_y") +
  theme_classic() +
  xlim(c(0,1)) + ylim(c(0,1)) +
  xlab("alpha") +
  ylab("m")  +
  scale_fill_gradient(high="red", low="yellow")

######################
# Along diagonal
######################

ggplot(dfPlot ) +
  geom_abline(slope=c(.5,1,2),intercept=0,linetype='dotted',color='gray') +
  geom_point(aes(x=alpha,y=m,color=fitness)) +
  facet_wrap(~gamma,nrow=1,scale="free_y") +
  theme_bw() +
  xlim(c(0,1)) + ylim(c(0,1)) +
  xlab("alpha") +
  ylab("m") +
  labs(colour="Fitness") +
  scale_color_gradient(high="red", low="yellow")

ggplot(dfPlot %>% dplyr::filter(alpha==m)) +
  geom_point(aes(x=alpha,y=fitness,group=gamma,color=gamma)) +
  geom_line(aes(x=alpha,y=fitness,group=gamma,color=gamma)) +
  theme_bw() +
  xlab("alpha/m") +
  ylab("fitness") +
  labs(colour="Fitness") +
  scale_color_gradient(high="red", low="yellow")

ggplot(dfPlot %>% dplyr::filter(alpha==.5*m)) +
  geom_point(aes(x=alpha,y=fitness,group=gamma,color=gamma)) +
  geom_line(aes(x=alpha,y=fitness,group=gamma,color=gamma)) +
  theme_bw() +
  xlab("alpha/m") +
  ylab("fitness") +
  labs(colour="Fitness") +
  scale_color_gradient(high="red", low="yellow")

ggplot(dfPlot %>% dplyr::filter(.5*alpha==m)) +
  geom_point(aes(x=alpha,y=fitness,group=gamma,color=gamma)) +
  geom_line(aes(x=alpha,y=fitness,group=gamma,color=gamma)) +
  theme_bw() +
  xlab("alpha*.5, m") +
  ylab("fitness") +
  labs(colour="Fitness") +
  scale_color_gradient(high="red", low="yellow")

ggplot(dfPlot) +
  geom_point(aes(x=alpha,y=fitness,group=gamma,color=gamma)) +
  geom_line(aes(x=alpha,y=fitness,group=gamma,color=gamma)) +
  facet_wrap(~m,nrow=1,scales='free') +
  theme_bw() +
  xlab("alpha*.5, m") +
  ylab("fitness") +
  labs(colour="Fitness") +
  scale_color_gradient(high="red", low="yellow")


######################
# Along diagonal
######################
ggplot(dfPlot %>% 
         dplyr::filter(alpha==m) %>% 
         dplyr::select(-c(m1,m2)) %>%
         tidyr::pivot_wider(names_from=c(gamma),values_from=fitness) %>%
         dplyr::mutate(del1=`1`-`0`,del2=`0.5`-`0`)) +
  geom_point(aes(x=alpha,y=del1)) +
    theme_bw() +
  xlab("alpha/m") +
  ylab("fitness") +
  labs(colour="Fitness") +
  scale_color_gradient(high="red", low="yellow")
