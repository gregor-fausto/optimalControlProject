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

directory = "discretize-optimize/analysisSix/"

######################
# Get combination of development, meristem constraint, resource constraint
######################

files <- list.files(paste0("~/Dropbox/optimalControlProject/",directory))
files<-files[c(grep("determinate-",files,ignore.case=TRUE))]

dfSigma.list <- list()

# state <- function(development="unbranchedDeterminate",m=1,alpha=1,gamma=1){
#   
#   fileName <- list.files(paste0("~/Dropbox/optimalControlProject/",directory))
#   fileName <- fileName[c(grep("RDS",fileName))]
  n=length(files)
dfSigma <- data.frame(model=rep("model",n),
                 ma=rep(NA,n),
                 mi=rep(NA,n),
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
  seasonDistribution = c(as.numeric(strsplit(tmp$initsName,"-")[[1]][2]),0)
  
  outMat = ode(y=initVals,times=seq( seasonDistribution[2], seasonDistribution[1],by=0.1),control,method=odemethod,parms=mParms,f1=tmp$u.list[[n]],f2=tmp$beta1.list[[n]],f3=tmp$beta2.list[[n]]);
  
  
  dfSigma[j,]=data.frame(tmp$model,
                    seasonDistribution[1],seasonDistribution[2],
                    mParms[1],mParms[2],mParms[3],mParms[4],
                    outMat[dim(outMat)[1],5],stringsAsFactors = FALSE)
}

dfSigma = dfSigma %>%
  dplyr::mutate(gamma=ifelse(model=="branchedDeterminate",1,
                             ifelse(model=="unbranchedDeterminate",0,gamma))) %>%
  dplyr::mutate(m = m1)



# ggplot(dfSigma) +
#   geom_point(aes(x=alpha,y=m)) +
#   facet_wrap(~model) +
#   theme_bw()

######################
# Calculate end of season fitness
######################


######################
# Make some plots!
######################
# 
# # remove filter if more sims are run
# ggplot(dfSigma %>% dplyr::filter(model!="determinate")) +
#   geom_line(aes(x=alpha,y=fitness,color=as.factor(m),linetype=model)) + 
#   #geom_point(aes(x=alpha,y=fitness,color=as.factor(m),shape=model)) +
#   facet_wrap(~model,scales='free_y') +
#   theme_bw() +
#   xlab("Resource constraint (alpha)") +
#   ylab("End-of-season fitness") +
#   labs(colour="Meristem constraint (m)")
# 
# ggplot(dfSigma %>% dplyr::filter(model!="determinate")) +
#   geom_line(aes(x=alpha,y=fitness,color=as.factor(m),linetype=model)) + 
#   # geom_point(aes(x=alpha,y=fitness,color=as.factor(m),shape=model)) +
#   theme_bw() +
#   xlab("Resource constraint (alpha)") +
#   ylab("End-of-season fitness") +
#   labs(colour="Meristem constraint (m)")
# 
# ggplot(dfSigma %>% dplyr::filter(model!="determinate")) +
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
# ggplot(dfSigma %>% dplyr::filter(model!="determinate")) +
#   geom_line(aes(x=m,y=fitness,group=alpha,color=as.factor(alpha))) +
#   #geom_point(aes(x=m,y=fitness,group=alpha,color=as.factor(alpha))) +
#   facet_wrap(~model) +
#   theme_bw() +
#   xlab("Meristem constraint (m)") +
#   ylab("End-of-season fitness") +
#   labs(colour="Resource constraint (alpha)")
# 
# ggplot(dfSigma %>% dplyr::filter(model!="determinate")) +
#   geom_line(aes(x=m,y=fitness,color=as.factor(alpha),linetype=model)) +
#   #geom_point(aes(x=m,y=fitness,group=alpha,color=as.factor(alpha),shape=model)) +
#   theme_bw() +
#   xlab("Meristem constraint (m)") +
#   ylab("End-of-season fitness") +
#   labs(colour="Resource constraint (alpha)")

## meristem constraint reduces end of season fitness
## effect of resource constraint is strongest at low meristem constraint
## which you can see by comparing end of season fitness at low m (no difference) vs. high m (strong resource constraint reduces fitness)

# ggplot(dfSigma ) +
#   geom_line(aes(x=gamma,y=fitness,group=sigma,color=sigma)) +
#   # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
# #  geom_point(aes(x=gamma,y=fitness,group=sigma,color=sigma)) +
#  # facet_wrap(~alpha,nrow=1,scale="free_y") +
#   theme_bw() +
#   xlab("Probability of branching") +
#   ylab("End-of-season fitness") +
#   labs(colour="Standard deviation") +
#   scale_color_gradient(high="green", low="#542788")

ggplot(dfSigma  ) +
  geom_line(aes(x=ma,y=fitness,group=gamma,color=gamma) )+
  # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=ma,y=fitness)) +
  facet_grid(m~alpha,scale='free') +
  theme_bw() +
  xlab("Season length") +
  ylab("End-of-season fitness") +
  labs(colour="gamma") +
  scale_color_gradient(high="green", low="#542788")

ggplot(dfSigma %>% dplyr::filter(m %in%c(.9,1.1))  ) +
  geom_line(aes(x=ma,y=fitness,group=gamma,color=gamma) )+
  # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=ma,y=fitness)) +
  facet_grid(m~alpha,scale='free') +
  theme_bw() +
  xlab("Season length") +
  ylab("End-of-season fitness") +
  labs(colour="gamma") +
  scale_color_gradient(high="green", low="#542788")

ggplot(dfSigma %>% dplyr::filter(m==0.05) ) +
  geom_line(aes(x=ma,y=fitness,group=interaction(alpha,gamma),color=gamma) )+
  # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=ma,y=fitness)) +
  facet_grid(m~alpha,scale='free') +
  theme_bw() +
  xlab("Season length") +
  ylab("End-of-season fitness") +
  labs(colour="gamma") +
  scale_color_gradient(high="green", low="#542788")

ggplot(dfSigma %>% dplyr::filter(m==1.5 & alpha==0.05) ) +
  geom_line(aes(x=ma,y=fitness,group=interaction(alpha,gamma),color=gamma) )+
  # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=ma,y=fitness)) +
    facet_wrap(~alpha,scale='free_y') + ylim(c(0,2)) +
  theme_bw() +
  xlab("Season length") +
  ylab("End-of-season fitness") +
  labs(colour="gamma") +
  scale_color_gradient(high="green", low="#542788")

ggplot(dfSigma %>% dplyr::filter(m==.05&alpha==.05) ) +
  geom_line(aes(x=ma,y=fitness,linetype=as.factor(gamma)) )+
  # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=ma,y=fitness)) +
  # facet_grid(m~alpha,scale='free_y') +
  theme_bw() +
  xlab("Season length") +
  ylab("End-of-season fitness") 

ggplot(dfSigma %>% dplyr::filter(m==.05&alpha==1.5) ) +
  geom_line(aes(x=ma,y=fitness,linetype=as.factor(gamma)) )+
  # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=ma,y=fitness)) +
  # facet_grid(m~alpha,scale='free_y') +
  theme_bw() +
  xlab("Season length") +
  ylab("End-of-season fitness") 

ggplot(dfSigma %>% dplyr::filter(m==1.5&alpha==.05) ) +
  geom_line(aes(x=ma,y=fitness,linetype=as.factor(gamma)) )+
  # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=ma,y=fitness)) +
  # facet_grid(m~alpha,scale='free_y') +
  theme_bw() +
  xlab("Season length") +
  ylab("End-of-season fitness") 

ggplot(dfSigma %>% dplyr::filter(m==1.5&alpha==1.5) ) +
  geom_line(aes(x=ma,y=fitness,linetype=as.factor(gamma)) )+
  # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=ma,y=fitness)) +
 # facet_grid(m~alpha,scale='free_y') +
  theme_bw() +
  xlab("Season length") +
  ylab("End-of-season fitness")



 # labs(colour="Season length\n standard deviation") +
#  scale_color_gradient(high="green", low="#542788")

ggplot(dfSigma %>% dplyr::filter(m==1.5)) +
  geom_line(aes(x=gamma,y=fitness,group=sigma,color=sigma)) +
  # geom_point(aes(x=gamma,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=gamma,y=fitness,group=sigma,color=sigma)) +
  facet_wrap(~alpha,nrow=1) +
  theme_bw() +
  xlab("Probability of branching") +
  ylab("End-of-season fitness") +
  labs(colour="Season length\n standard deviation") +
  scale_color_gradient(high="green", low="#542788")

# at strong resource constraint, meristem constraint is irrelevant
# as resource constraint weakens, meristem constraint becomes more important
# in determining fitness

ggplot(dfSigma ) +
  geom_line(aes(x=gamma,y=fitness,group=sigma,color=sigma)) +
  geom_point(aes(x=gamma,y=fitness,color=sigma)) +
  facet_grid(alpha~m,scale='free_y') +
  theme_bw() +
  xlab("Probability of branching") +
  ylab("End-of-season fitness") +
  labs(colour="Standard deviation") +
  scale_color_gradient(high="orange", low="purple")

ggplot(dfSigma %>%dplyr::filter(gamma==0)) +
  geom_line(aes(x=alpha,y=fitness,group=sigma,color=sigma)) +
  geom_point(aes(x=m,y=fitness,color=sigma)) +
  facet_grid(sigma~alpha,scale='free_y') +
  theme_bw() +
  xlab("Meristem constraint (m)") +
  ylab("End-of-season fitness") +
  labs(colour="Standard deviation") +
  scale_color_gradient(high="orange", low="purple")

ggplot(dfSigma %>%dplyr::filter(gamma==1)) +
  geom_line(aes(x=m,y=fitness,group=sigma,color=sigma)) +
  geom_point(aes(x=m,y=fitness,color=sigma)) +
  facet_grid(sigma~alpha,scale='free_y') +
  theme_bw() +
  xlab("Meristem constraint (m)") +
  ylab("End-of-season fitness") +
  labs(colour="Standard deviation") +
  scale_color_gradient(high="orange", low="purple")

ggplot(dfSigma ) +
  geom_line(aes(x=sigma,y=fitness,group=gamma,color=gamma,linetype=as.factor(gamma))) +
  geom_point(aes(x=sigma,y=fitness)) +
  facet_grid(m~alpha,scales='free') +
  theme_bw() +
  xlab("Standard deviation") +
  ylab("End-of-season fitness") +
  labs(colour="P(branching)") +
  scale_color_gradient(high="orange", low="purple")

ggplot(dfSigma %>% dplyr::filter(alpha==1.5)) +
  geom_line(aes(x=sigma,y=fitness,group=gamma,linetype=as.factor(gamma))) +
  geom_point(aes(x=sigma,y=fitness)) +
  facet_wrap(~m,scales='free_y',nrow=1) +
  theme_bw() +
  xlab("Standard deviation") +
  ylab("End-of-season fitness") +
  labs(linetype="Probability of branching")

ggplot(dfSigma %>% dplyr::filter(alpha==0.05)) +
  geom_line(aes(x=sigma,y=fitness,group=gamma,linetype=as.factor(gamma))) +
  geom_point(aes(x=sigma,y=fitness)) +
  facet_wrap(~m,nrow=1) +
  theme_bw() +
  xlab("Standard deviation\n of season length") +
  ylab("End-of-season fitness") +
  labs(linetype="Probability of branching")
  
 
ggplot(dfSigma %>% dplyr::filter(alpha==1.5&m==0.05)) +
  geom_line(aes(x=sigma,y=fitness,group=gamma,linetype=as.factor(gamma))) +
  geom_point(aes(x=sigma,y=fitness)) +
  #facet_grid(m~alpha,scales='free') +
  theme_bw() +
  xlab("Standard deviation") +
  ylab("End-of-season fitness")

ggplot(dfSigma %>% dplyr::filter(alpha==1.5&m==.5)) +
  geom_line(aes(x=sigma,y=fitness,group=gamma,linetype=as.factor(gamma))) +
  geom_point(aes(x=sigma,y=fitness)) +
  #facet_grid(m~alpha,scales='free') +
  theme_bw() +
  xlab("Standard deviation") +
  ylab("End-of-season fitness")

ggplot(dfSigma %>% dplyr::filter(alpha==1.5&m==1)) +
  geom_line(aes(x=sigma,y=fitness,group=gamma,linetype=as.factor(gamma))) +
  geom_point(aes(x=sigma,y=fitness)) +
  #facet_grid(m~alpha,scales='free') +
  theme_bw() +
  xlab("Standard deviation") +
  ylab("End-of-season fitness")

ggplot(dfSigma %>% dplyr::filter(alpha==1.5&m==1.5)) +
  geom_line(aes(x=sigma,y=fitness,group=gamma,linetype=as.factor(gamma))) +
  geom_point(aes(x=sigma,y=fitness)) +
  #facet_grid(m~alpha,scales='free') +
  theme_bw() +
  xlab("Standard deviation") +
  ylab("End-of-season fitness")

ggplot(dfSigma %>% dplyr::filter(m==.5&alpha==1.5)) +
  geom_line(aes(x=sigma,y=fitness,group=gamma,color=gamma)) +
   geom_point(aes(x=sigma,y=fitness,color=gamma)) +
  theme_bw() +
  xlab("Standard deviation") +
  ylab("End-of-season fitness") +
  labs(colour="Probability of branching") +
  scale_color_gradient(high="orange", low="purple")

ggplot(dfSigma %>% dplyr::filter(m==1.5&alpha==.05)) +
  geom_line(aes(x=sigma,y=fitness,group=gamma,color=gamma)) +
  geom_point(aes(x=sigma,y=fitness,color=gamma)) +
  theme_bw() +
  xlab("Standard deviation") +
  ylab("End-of-season fitness") +
  labs(colour="Probability of branching") +
  scale_color_gradient(high="orange", low="purple")


ggplot(dfSigma %>% dplyr::filter(m==1.5) ) +
  geom_line(aes(x=gamma,y=fitness,group=sigma,color=sigma)) +
  geom_point(aes(x=gamma,y=fitness)) +
  facet_wrap(~alpha,scales='free',nrow=1) +
  theme_bw() +
  xlab("P(branching)") +
  ylab("End-of-season fitness") +
  labs(colour="SD") +
  scale_color_gradient(high="orange", low="purple")

dfPlot <- dfSigma %>% dplyr::filter(m %in%c(.9) & alpha==.9)
ggplot( dfPlot ) +
  geom_line(aes(x=ma,y=fitness,group=gamma,color=gamma) ,size=1)+
  geom_point(aes(x=ma,y=fitness,color=gamma),size=2) +
  theme_classic() +
  xlab("Season length") +
  ylab("End-of-season fitness") +
  labs(colour="gamma") +
  scale_color_gradient(high="orange", low="forestgreen") +
  theme(text = element_text(size=20),legend.position='none') 

theme_classic() +
  xlab("Probability of branching") +
  ylab("End-of-season fitness") +
  labs(colour="Resource use efficiency") +
  scale_color_gradient(high="orange", low="purple") +
  theme(text = element_text(size=20),legend.position='none') 
