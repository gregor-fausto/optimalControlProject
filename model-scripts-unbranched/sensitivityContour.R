# I optimized fitness for a combination of initial conditions and constraints
# I then relaxed the resource constraint by taking alpha/.9
# I then relaxed the meristem constraint by taking m1/.9 and m2/.9

# Load libraries 
library(deSolve)
library(RColorBrewer)

# ------------------------------------------------------------------------------
# Function
# ------------------------------------------------------------------------------
f = function(x=filename){
  source(paste0("model-scripts/models/control-",x$model,".R"))
  derivs=numeric(6); 
  
  inits=x$inits[1:4]
  mParms=x$inits[5:8]
  other=c(pen=0,obj=0)
  dist = c(distribution=as.character(x$seasonDistribution))
  seasonParms = c(max=x$max,min=x$min)
  
  initVals = c(inits,other) 
  outMat = ode(y=initVals,times=seq(0,5,by=0.1),control,method=odemethod,parms=mParms,f1=x$u.list[[1]],f2=x$beta1.list[[2]],f3=x$beta2.list[[3]]);
  outDataFrame = data.frame(time=seq(0,5,by=0.1),alpha=x$inits[7],m1=x$inits[5],L=data.frame(outMat)$L)
  return(outDataFrame)
}

# ------------------------------------------------------------------------------
# Get files for analysis
# ------------------------------------------------------------------------------
# Set ODE method to use
odemethod=rkMethod("rk4");  # should be safe to use with bad parameter values 

# Read in list of files
outputVec <- list.files("model-scripts/analysisThree")
outputVec <- outputVec[grep("determinate-uniform-5-0-",outputVec)]

outputAlpha = outputVec[grep('relaxAlpha',outputVec)]
outputMeristem = outputVec[grep('relaxMeristem',outputVec)]

outputBaseline=gsub("-relaxAlpha", "",outputVec)
outputBaseline=gsub("-relaxMeristem", "",outputBaseline)
outputBaseline=unique(outputBaseline)

# ------------------------------------------------------------------------------
# Put files in list
# ------------------------------------------------------------------------------
# Number of files
n = length(outputMeristem)

# Create list of outputs
runListBaseline <- list()
runListMeristem <- list()
runListAlpha <- list()
for(i in 1:n){
  runListBaseline[[i]] <- readRDS(paste0("model-scripts/analysisThree/",outputBaseline[[i]]))
  runListAlpha[[i]] <- readRDS(paste0("model-scripts/analysisThree/",outputAlpha[[i]]))
  runListMeristem[[i]] <- readRDS(paste0("model-scripts/analysisThree/",outputMeristem[[i]]))
}


# ------------------------------------------------------------------------------
# Run analysis
# ------------------------------------------------------------------------------
derivs=numeric(6); 

baseline = lapply(runListBaseline,f)
relaxAlpha = lapply(runListAlpha,f)
relaxMeristem = lapply(runListMeristem,f)

library(tidyverse)

baselineFinal = do.call(rbind,baseline) %>% dplyr::filter(time==5)
alphaFinal = do.call(rbind,relaxAlpha) %>% dplyr::filter(time==5)
meristemFinal = do.call(rbind,relaxMeristem) %>% dplyr::filter(time==5)

baselineFinal$L.base = baselineFinal$L
baselineFinal$L.alpha = alphaFinal$L
baselineFinal$L.meristem = meristemFinal$L


# ------------------------------------------------------------------------------
# Relative fitness at end
# ------------------------------------------------------------------------------
baselineFinal = baselineFinal %>%
  dplyr::mutate(resourceConstraint = L.alpha/L.base,
                meristemConstraint = L.meristem/L.base)

# library(plotly)
# plot_ly(data=baselineFinal, x=~alpha, y=~m1, z=~L.base, type = 'contour',colorscale='Viridis')
# plot_ly(data=baselineFinal, x=~alpha, y=~m1, z=~resourceConstraint, type = 'contour',colorscale='Viridis')
# plot_ly(data=baselineFinal, x=~alpha, y=~m1, z=~meristemConstraint, type = 'contour',colorscale='Viridis')

baselineFinal=baselineFinal[with(baselineFinal,order(alpha,m1)),]

# contour(x=baselineFinal$alpha,y=baselineFinal$m1,z=baselineFinal$L.base)

library(ggplot2)



g1 = ggplot(data=baselineFinal%>% dplyr::filter(m1%in%c(1,2,4,10)),aes(x=alpha,y=m1,z=L.base)) +
  geom_contour_filled(color='white') +
  geom_point(aes(x=alpha,y=m1),alpha=.5) +
  theme_bw() + guides(fill = guide_legend(title = "Fitnes") ) +
  ggtitle("Fitness; P=1, V=1; Uniform season length [2.5,5]")

g2 = ggplot(data=baselineFinal %>% dplyr::filter(m1%in%c(1,2,4,10)),aes(x=alpha,y=m1,z=meristemConstraint)) +
  geom_contour_filled(color='white') +
  geom_point(aes(x=alpha,y=m1),alpha=.5) +
  theme_bw() + guides(fill = guide_legend(title = "Increase in fitness") ) +
  ggtitle("Meristem constraint")

g3 = ggplot(data=baselineFinal,aes(x=alpha,y=m1,z=resourceConstraint)) +
  geom_contour_filled(color='white') +
  geom_point(aes(x=alpha,y=m1),alpha=.5) +
  theme_bw() + guides(fill = guide_legend(title = "Increase in fitness") ) +
  ggtitle("Resource constraint")

# #pdf(file="/Users/gregor/Documents/optimalControlProject/products/figures/Ppt5-V2-gammaZero.pdf",width=6,height=6)
# g1
# g2
# g3
# #dev.off()

matrix.image=function(A, x=NULL, y=NULL, col=rainbow(100,start=0.67,end=0),
                      bw=FALSE, do.contour=FALSE, do.legend=TRUE,...) {
  if(do.legend) layout(mat=cbind(matrix(1,5,5),rep(2,5)));
  par(mar=c(6,5,3,2)); 
  if(is.null(x)) x=1:ncol(A);
  if(is.null(y)) y=1:nrow(A); 
  nx=length(x); ny=length(y); 
  x1=c(1.5*x[1]-0.5*x[2],1.5*x[nx]-0.5*x[nx-1]); 
  y1=c(1.5*y[1]-0.5*y[2],1.5*y[ny]-0.5*y[ny-1]); 
  if(bw) col=grey( (200:50)/200 ); 
  # comment out this line to reverse the direction of the plot
  #image(list(x=x,y=y,z=t(A)),xlim=x1,ylim=rev(y1),col=col,cex.axis=1.5,cex.lab=1.5,bty="u",...);
  image(list(x=x,y=y,z=t(A)),xlim=x1,ylim=(y1),col=col,cex.axis=1.5,cex.lab=1.5,bty="u",...);
  abline(a=0,b=1,col='white');
  abline(v=range(x1)); abline(h=range(y1)); 
  if(do.contour) contour(x,y,t(A),nlevels=5,labcex=1.2,add=TRUE);   
  
  if(do.legend) {
    l.y=seq(min(A),max(A),length=100);  
    par(mar=c(6,2,3,1))
    image(list(x=1:2,y=l.y,z=rbind(l.y,l.y)),col=col,bty="o",xaxt="n",yaxt="n"); 
    axis(side=2,cex.axis=1.5,at=pretty(seq(min(A),max(A),length=10))); 
  } 
}



mat.fitness=matrix(baselineFinal$L,nrow=11)
mat.resourceConstraint=matrix(baselineFinal$resourceConstraint,nrow=11)
mat.meristemConstraint=matrix(baselineFinal$meristemConstraint,nrow=11)
mat.ratio=mat.meristemConstraint/mat.resourceConstraint

head(baselineFinal)

px = unique(baselineFinal$alpha)
py = baselineFinal$m1[order(unique(baselineFinal$m1))]

#matrix.image(A,px,px,xlab="Size t",ylab="Size t+1",do.contour=TRUE,do.legend=TRUE); 

## Matrix image from Data-driven models for structured populations 
## prints from top to bottom
#pdf(file="/Users/gregor/Documents/optimalControlProject/products/figures/P1-V1-gamma05.pdf",width=6,height=6)
matrix.image(mat.fitness,px,px,xlab="alpha (divisions/unit biomass)",ylab="m1 (divisions/meristem)",main="Fitness",do.contour=TRUE,do.legend=TRUE);
matrix.image(mat.meristemConstraint,px,px,xlab="alpha (divisions/unit biomass)",ylab="m1 (divisions/meristem)",main="Meristem constraint (relax m1)",do.contour=TRUE,do.legend=TRUE);
matrix.image(mat.resourceConstraint,px,px,xlab="alpha (divisions/unit biomass)",ylab="m1 (divisions/meristem)",main="Resource constraint (relax alpha)",do.contour=TRUE,do.legend=TRUE);
matrix.image(mat.ratio,px,px,xlab="alpha (divisions/unit biomass)",ylab="m1 (divisions/meristem)",main="Ratio of meristem constraint:resource constraint",do.contour=TRUE,do.legend=TRUE);

#dev.off()
