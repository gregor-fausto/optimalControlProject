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

state <- function(development="unbranchedDeterminate",m=1,alpha=1,gamma=1){
  
  fileName <- list.files(paste0("~/Dropbox/optimalControlProject/",directory))
  fileName <- fileName[c(grep("RDS",fileName))]
  
  for(j in 1:length(fileName)){
    tmp <- readRDS(paste0("~/Dropbox/optimalControlProject/",directory,fileName[j]))

    if(tmp$model!=development) next
    if(tmp$inits["m1"]!=m) next
    if(is.na(tmp$inits["alpha"])|tmp$inits["alpha"]!=alpha) next
    if(!as.vector(is.na(tmp$inits["gamma"]))) next
    if(ifelse(is.na(tmp$inits["gamma"]),FALSE,tmp$inits["gamma"]!=gamma)) next

    obj=print(j)
    # tmp <- readRDS(paste0("~/Dropbox/optimalControlProject/",directory,fileName[j]))
    # vec<-c(model=tmp$model!=development,
    #   m=tmp$inits["m1"]!=m,
    #   alpha=is.na(tmp$inits["alpha"])|tmp$inits["alpha"]!=alpha,
    #   gammaBinary=!as.vector(is.na(tmp$inits["gamma"])),
    #   gamma=ifelse(is.na(tmp$inits["gamma"]),FALSE,tmp$inits["gamma"]!=gamma))
    # if(all(vec)){
    #   obj=print(j)
    #   }
  }
    
  tmp = readRDS(paste0("~/Dropbox/optimalControlProject/",directory,fileName[obj]))
  source(paste0("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-",tmp$model,".R"))
  derivs=numeric(6); 
  
  n=length(tmp$u.list)
  initVals = c(tmp$inits[c("P","V","I","L")],c(pen=0,obj=0))
  mParms = tmp$inits[c("m1","m2","alpha","gamma")]
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=tmp$u.list[[n]],f2=tmp$beta1.list[[n]],f3=tmp$beta2.list[[n]]);
  return(outMat[51,5])
}

######################
# Get combination of development, meristem constraint, resource constraint
######################

files <- list.files(paste0("~/Dropbox/optimalControlProject/",directory))
files<-files[c(grep("determinate-uniform-",files,ignore.case=TRUE))]

n = length(files)
modelName = c()
alphaName = c()
mName = c()
gName = c()

for(i in 1:n){
  tmp <- readRDS(paste0("~/Dropbox/optimalControlProject/",directory,files[i]))
  parms = tmp$inits[names(tmp$inits) %in% c("m1","m2","alpha","gamma")]
  modelName[i] = tmp$model
  alphaName[i] = parms["alpha"]
  mName[i] = parms["m1"]
  gName[i] = parms["gamma"]
}

df = data.frame(model=modelName,m=mName,alpha=alphaName,gamma=gName) %>%
  dplyr::mutate(gamma=ifelse(model=="branchedDeterminate",1,
                             ifelse(model=="unbranchedDeterminate",0,gamma)))

derivs=numeric(6); 
for(i in 1:dim(df)[1]){
  state(development=df$model[i],m=df$m[i],alpha=df$alpha[i],gamma=df$gamma[i])
}

ggplot(df) +
  geom_point(aes(x=alpha,y=m)) +
  facet_wrap(~model) +
  theme_bw()

######################
# Calculate end of season fitness
######################

df=df %>%
  dplyr::filter(model!="determinate") %>%
  rowwise() %>%
  dplyr::mutate(fitness=state(development=model,m=m,alpha=alpha,gamma=gamma))

# df=df %>%
#   dplyr::filter(model=="determinate") %>%
#   rowwise() %>%
#   dplyr::mutate(fitness=state(development=model,m=m,alpha=alpha,gamma=gamma)) 


######################
# Make some plots!
######################

# remove filter if more sims are run
ggplot(df ) +
   geom_line(aes(x=alpha,y=fitness,color=as.factor(m),linetype=model)) + 
  #geom_point(aes(x=alpha,y=fitness,color=as.factor(m),shape=model)) +
  facet_wrap(~model) +
  theme_bw() +
  xlab("Resource constraint (alpha)") +
  ylab("End-of-season fitness") +
  labs(colour="Meristem constraint (m)")

ggplot(df) +
  geom_line(aes(x=alpha,y=fitness,color=as.factor(m),linetype=model)) + 
 # geom_point(aes(x=alpha,y=fitness,color=as.factor(m),shape=model)) +
  theme_bw() +
  xlab("Resource constraint (alpha)") +
  ylab("End-of-season fitness") +
  labs(colour="Meristem constraint (m)")

ggplot(df) +
  geom_line(aes(x=alpha,y=fitness,color=as.factor(m),linetype=model)) + 
  # geom_point(aes(x=alpha,y=fitness,color=as.factor(m),shape=model)) +
  theme_bw() +
  xlab("Resource constraint (alpha)") +
  ylab("End-of-season fitness") +
  labs(colour="Meristem constraint (m)")

## resource constraint reduces end of season fitness
## effect of meristem constraint is strongest at low resource constraint
## which you can see by comparing end of season fitness at low alpha (no difference) vs. high alpha (strong meristem constraint reduces fitness)

ggplot(df) +
  geom_line(aes(x=m,y=fitness,group=alpha,color=as.factor(alpha))) +
  #geom_point(aes(x=m,y=fitness,group=alpha,color=as.factor(alpha))) +
  facet_wrap(~model) +
  theme_bw() +
  xlab("Meristem constraint (m)") +
  ylab("End-of-season fitness") +
  labs(colour="Resource constraint (alpha)")

ggplot(df) +
  geom_line(aes(x=m,y=fitness,color=as.factor(alpha),linetype=model)) +
  #geom_point(aes(x=m,y=fitness,group=alpha,color=as.factor(alpha),shape=model)) +
  theme_bw() +
  xlab("Meristem constraint (m)") +
  ylab("End-of-season fitness") +
  labs(colour="Resource constraint (alpha)")

## meristem constraint reduces end of season fitness
## effect of resource constraint is strongest at low meristem constraint
## which you can see by comparing end of season fitness at low m (no difference) vs. high m (strong resource constraint reduces fitness)


df=df %>%
  dplyr::mutate(p_branch = ifelse(model=="unbranchedDeterminate",0,1))

ggplot(df ) +
  geom_line(aes(x=p_branch,y=fitness,group=m,color=as.factor(m))) +
  geom_point(aes(x=p_branch,y=fitness,group=m,color=as.factor(m))) +
  facet_wrap(~alpha,nrow=1) +
  theme_bw() +
  xlab("Probability of branching") +
  ylab("End-of-season fitness") +
  labs(colour="Meristem constraint (m)")

# at strong resource constraint, meristem constraint is irrelevant
# as resource constraint weakens, meristem constraint becomes more important
# in determining fitness

ggplot(df) +
  geom_line(aes(x=p_branch,y=fitness,color=as.factor(alpha))) +
  geom_point(aes(x=p_branch,y=fitness,color=as.factor(alpha))) +
  facet_wrap(~m,nrow=1) +
  theme_bw() +
  xlab("Probability of branching") +
  ylab("End-of-season fitness") +
  labs(colour="Resource constraint (alpha)")
