### Optimal control for King & Roughgarden 1982 ###
### Gregor Siegmund ###
### gs589@cornell.edu ###
### Help links
### http://www.talkstats.com/threads/constrained-optimization-constroptim.60583/
### increasing mesh size in integral helped solve problem

write.table(cbind(nt=NA,as.matrix(t(proc.time()))),file=paste0("function.time",".csv"),append=TRUE,col.names=TRUE,sep=',')

# Figure 1
rm(list=ls(all=TRUE))

setwd("~/Dropbox/optimalControlProject/king-roughgarden") #### Edit as needed 

nt = 50;
yA=c(x1=1,x2=.25); 

ptm <- proc.time()
source("king-roughgarden-v2.R"); 
write.table(cbind(nt=nt,as.matrix(t(proc.time() - ptm))),file="function.time.csv",append=TRUE,col.names=FALSE,sep=',')

source("figure-script.R")


# Figure 3
rm(list=ls(all=TRUE))

setwd("~/Dropbox/optimalControlProject/king-roughgarden") #### Edit as needed 

nt = 50;
yA=c(x1=1,x2=0); 

ptm <- proc.time()
source("king-roughgarden-v2.R"); 
write.table(cbind(nt=nt,as.matrix(t(proc.time() - ptm))),file="function.time.csv",append=TRUE,col.names=FALSE,sep=',')

source("figure-script.R")

# Figure 4
rm(list=ls(all=TRUE))

setwd("~/Dropbox/optimalControlProject/king-roughgarden") #### Edit as needed 

nt = 50;
yA=c(x1=1,x2=2); 

ptm <- proc.time()
source("king-roughgarden-v2.R"); 
write.table(cbind(nt=nt,as.matrix(t(proc.time() - ptm))),file="function.time.csv",append=TRUE,col.names=FALSE,sep=',')

source("figure-script.R")

# Figure 5
rm(list=ls(all=TRUE))

setwd("~/Dropbox/optimalControlProject/king-roughgarden") #### Edit as needed 

nt = 50;
yA=c(x1=1,x2=45.4); 

ptm <- proc.time()
source("king-roughgarden-v2.R"); 
write.table(cbind(nt=nt,as.matrix(t(proc.time() - ptm))),file="function.time.csv",append=TRUE,col.names=FALSE,sep=',')

source("figure-script.R")