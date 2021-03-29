####################################
## Scripts for optimization of unbranched, determinate plant
## under resource constraints only
## with varying strength of resource constraint
####################################

## Resource constraint only
# unbranched
rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-.1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-10.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-.1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-10.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

