####################################
## Scripts for optimization of unbranched, determinate plant
## under resource constraints only
## with varying strength of resource constraint
####################################

## Resource constraint only
# unbranched

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-resource.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-resource.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-4.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-resource.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-resource.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point75.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-resource.R")

## Resource constraint only
# branched

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-resource.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-resource.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-4.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-resource.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-resource.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point75.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-resource.R")


