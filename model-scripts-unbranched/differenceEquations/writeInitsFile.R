####################################
## Define analysis
####################################

analysisName = c("analysisOne")

####################################
## Write inits into table of parameters
####################################

library(tidyverse)

model = c('determinate')
Ve = c(1)
Le = c(1)
In = c(0)
Fl = c(0)
m = c(1)
alpha = c(1)
seasonDistribution = c("uniform")
mu = c(NA)
sd = c(NA)
max = c(60)
min = c(30)
gamma = c(0)

df<-data.frame(expand.grid(model=model,
                       Ve=Ve,Le=Le,In=In,Fl=Fl,
                       m1=m,alpha=alpha,
                       seasonDistribution=seasonDistribution,
                       mu=mu,sd=sd,max=max,min=min,gamma=gamma)) %>%
  dplyr::mutate(m2=m1) %>%
  dplyr::select(model,Ve,Le,In,Fl,m1,alpha,seasonDistribution,mu,sd,max,min,m2,gamma)

# write out file with all parameters
saveRDS(df,paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"-parameterFile.RDS"))

write.csv(df,paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"-parameterTable.csv"),row.names=F)
write.csv(df[-c(1:(dim(df)[1])),],paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"-exitTable.csv"),row.names=F)

####################################
## Build inits object
####################################

# function builds a list of initial conditions
buildInits <-function(tmp){
  inits = c(Ve=tmp$Ve,Le=tmp$Le,In=tmp$In,Fl=tmp$Fl)
  other = c(pen=0,obj=0)
  mParms = c(m1=tmp$m1,m2=tmp$m2,alpha=tmp$alpha,gamma=tmp$gamma)
  dist = c(distribution=as.character(tmp$seasonDistribution))
  seasonParms = c(max=tmp$max,min=tmp$min)
  return(list(inits=inits,other=other,mParms=mParms,distribution=dist,seasonParms=seasonParms))
}

# function runs optimization on a row of the parameter table
runOptimization <- function(tmp){
  controlFile = paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/models/control-",tmp$model,".R")
  source(controlFile)
  initsList=buildInits(tmp)
  source("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/models/generic-script-all.R")
}

for(i in 1:dim(df)[1]){
  a1=read.csv(paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"-parameterTable.csv"))
  a2=read.csv(paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"-exitTable.csv"))
  a2$model = as.factor(a2$model)
  a2$seasonDistribution = as.factor(a2$seasonDistribution)
  subDF = setdiff(a1,a2)
  tmp=subDF[1,]
  write.table( tmp,  
               file=paste0("/Users/gregor/Documents/optimalControlProject/model-scripts-unbranched/",analysisName,"-exitTable.csv"), 
               append = T, 
               sep=',', 
               row.names=F, 
               col.names=F )
  runOptimization(tmp)
}

par(mfrow=c(2,2))
plot(outMat[,1],outMat[,2],type='l')
plot(outMat[,1],outMat[,3],type='l')
plot(outMat[,1],outMat[,4],type='l')
plot(outMat[,1],outMat[,5],type='l')
#lines(outMat[,1],outMat[,5],col='purple')
