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
  source(paste0("models/control-",x$model,".R"))
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
outputVec <- list.files("analysisTwo")
outputVec <- outputVec[grep("determinate-uniform-5-1",outputVec)]

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
  runListBaseline[[i]] <- readRDS(paste0("analysisTwo/",outputBaseline[[i]]))
  runListAlpha[[i]] <- readRDS(paste0("analysisTwo/",outputAlpha[[i]]))
  runListMeristem[[i]] <- readRDS(paste0("analysisTwo/",outputMeristem[[i]]))
}


# ------------------------------------------------------------------------------
# Run analysis
# ------------------------------------------------------------------------------
derivs=numeric(6); 

baseline = lapply(runListBaseline,f)
relaxAlpha = lapply(runListAlpha,f)
relaxMeristem = lapply(runListMeristem,f)

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

library(plotly)
# plot_ly(data=baselineFinal, x=~alpha, y=~m1, z=~L.base, type = 'contour',colorscale='Viridis')
# plot_ly(data=baselineFinal, x=~alpha, y=~m1, z=~resourceConstraint, type = 'contour',colorscale='Viridis')
# plot_ly(data=baselineFinal, x=~alpha, y=~m1, z=~meristemConstraint, type = 'contour',colorscale='Viridis')

baselineFinal=baselineFinal[with(baselineFinal,order(alpha,m1)),]

contour(x=baselineFinal$alpha,y=baselineFinal$m1,z=baselineFinal$L.base)

library(ggplot2)



g1 = ggplot(data=baselineFinal,aes(x=alpha,y=m1,z=L.base)) +
  geom_contour_filled(color='white') +
  geom_point(aes(x=alpha,y=m1),alpha=.5) +
  theme_bw() + guides(fill = guide_legend(title = "Fitnes") ) +
  ggtitle("Fitness; P=1, V=1; Uniform season length [2.5,5]")

g2 = ggplot(data=baselineFinal,aes(x=alpha,y=m1,z=meristemConstraint)) +
  geom_contour_filled(color='white') +
  geom_point(aes(x=alpha,y=m1),alpha=.5) +
  theme_bw() + guides(fill = guide_legend(title = "Increase in fitness") ) +
  ggtitle("Meristem constraint")

g3 = ggplot(data=baselineFinal,aes(x=alpha,y=m1,z=resourceConstraint)) +
  geom_contour_filled(color='white') +
  geom_point(aes(x=alpha,y=m1),alpha=.5) +
  theme_bw() + guides(fill = guide_legend(title = "Increase in fitness") ) +
  ggtitle("Resource constraint")

pdf(file="/Users/gregor/Documents/optimalControlProject/products/figures/gammaOne-2.pdf",width=6,height=6)
g1
g2
g3
dev.off()



