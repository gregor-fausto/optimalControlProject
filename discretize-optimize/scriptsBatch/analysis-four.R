####################################
## Scripts for optimization of unbranched, determinate plant
## under meristem constraints only
####################################

## TEST

## Meristem constraint only
# unbranched

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-meristem.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point75.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-meristem.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-meristem.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-meristem.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-meristem.R")

## Resource constraint only
# branched

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-meristem.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point75.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-meristem.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-meristem.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-meristem.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminateMeristem.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-meristem.R")





## Resource and meristem constraint 
# unbranched

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point75.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

## Resource constraint only
# branched

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point75.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-1point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")




## vary meristem constraint

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point25-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point5-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1point25-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-2-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point25-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point5-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1point25-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-2-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")


## NEW
rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point05-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point05-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point05-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point05-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")


rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point25-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point25-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")


rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point5-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point5-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")


rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point75-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point75-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

## stronger resource constraint

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point25-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point25-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")


rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point5-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point5-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")


rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point75-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-point75-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")


rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")


rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1point5-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1point5-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

## more initial V (1:1)

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1-point05.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1-1point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point25-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point5-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point75-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1point5-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point25-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point5-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point75-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1point5-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

# branched

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1-point05.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1-1point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point25-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point5-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point75-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1point5-1.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point25-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point5-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-point75-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")

rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-branchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits2-uniform-1point5-point5.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")



rm(list=ls(all=TRUE)) # clear R environment
controlFile = "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/control-unbranchedDeterminate.R"
initsFile <- "~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform-1-2.R"
source("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/generic-script-all.R")


