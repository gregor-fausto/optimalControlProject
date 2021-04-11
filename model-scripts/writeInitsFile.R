####################################
## Define analysis
####################################

analysisName = c("analysisX")

####################################
## Write inits into table of parameters
####################################

library(tidyverse)

model = c('determinate')
P = c(1)
V = c(1)
I = c(0)
L = c(0)
m = seq(.5)
alpha = seq(.1,1,length.out=8)
seasonDistribution = c("uniform")
mu = c(NA)
sd = c(NA)
max = c(5)
min = c(2.5)
gamma = c(0)

df<-data.frame(expand.grid(model=model,
                       P=P,V=V,I=I,L=L,
                       m1=m,alpha=alpha,
                       seasonDistribution=seasonDistribution,
                       mu=mu,sd=sd,max=max,min=min,gamma=gamma)) %>%
  dplyr::mutate(m2=m1) %>%
  dplyr::select(model,P,V,I,L,m1,alpha,seasonDistribution,mu,sd,max,min,m2,gamma)

# ggplot(df) +
  # geom_point(aes(x=alpha,y=m1)) +
  # facet_wrap(~model) +
  # theme_bw()

# write out file with all parameters
saveRDS(df,paste0("model-scripts/",analysisName,"-parameterFile.RDS"))

write.csv(df,paste0("model-scripts/",analysisName,"-parameterTable.csv"),row.names=F)
write.csv(df[-c(1:(dim(df)[1])),],paste0("model-scripts/",analysisName,"-exitTable.csv"),row.names=F)

####################################
## Build inits object
####################################

# function builds a list of initial conditions
buildInits <-function(tmp){
  inits = c(P=tmp$P,V=tmp$V,I=tmp$I,L=tmp$L)
  other = c(pen=0,obj=0)
  mParms = c(m1=tmp$m1,m2=tmp$m2,alpha=tmp$alpha,gamma=tmp$gamma)
  dist = c(distribution=as.character(tmp$seasonDistribution))
  seasonParms = c(max=tmp$max,min=tmp$min)
  return(list(inits=inits,other=other,mParms=mParms,distribution=dist,seasonParms=seasonParms))
}

# function runs optimization on a row of the parameter table
runOptimization <- function(tmp){
  controlFile = paste0("model-scripts/models/control-",tmp$model,".R")
  source(controlFile)
  initsList=buildInits(tmp)
  source("model-scripts/models/generic-script-all.R")
}

for(i in 1:dim(df)[1]){
  a1=read.csv(paste0("model-scripts/",analysisName,"-parameterTable.csv"))
  a2=read.csv(paste0("model-scripts/",analysisName,"-exitTable.csv"))
  a2$model = as.factor(a2$model)
  a2$seasonDistribution = as.factor(a2$seasonDistribution)
  subDF = setdiff(a1,a2)
  tmp=subDF[1,]
  write.table( tmp,  
               file=paste0("model-scripts/",analysisName,"-exitTable.csv"), 
               append = T, 
               sep=',', 
               row.names=F, 
               col.names=F )
  runOptimization(tmp)
}
