####################################
## Scripts for optimization of unbranched, determinate plant
## under resource constraints only
## under meristem constraints only
## under resource and meristem constraints jointly
####################################

## Resource constraint only
rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

## Meristem constraint only
rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

## Resource+meristem constraint
rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")


## Conditions 2


## Resource constraint only
rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateResource.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

## Meristem constraint only
rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

## Resource+meristem constraint
rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script.R")