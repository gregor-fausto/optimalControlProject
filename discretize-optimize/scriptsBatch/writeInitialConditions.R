fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-0normalpt25.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=.1,m2=1)",
             "mu = 2.5; sigma = 0.25"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-0normalpt5.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
              "other = c(pen=0,obj=0)",
              "mParms = c(m1=.1,m2=1)",
             "mu = 2.5; sigma = 0.5"),
            fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-0normal1.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=.1,m2=1)",
             "mu = 2.5; sigma = 1"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-0normal2.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=.1,m2=1)",
             "mu = 2.5; sigma = 2"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-0normal5.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=.1,m2=1)",
             "mu = 2.5; sigma = 5"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-1normalpt25.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=.9,m2=1)",
             "mu = 2.5; sigma = 0.25"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-1normalpt5.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=.9,m2=1)",
             "mu = 2.5; sigma = 0.5"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-1normal1.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=.9,m2=1)",
             "mu = 2.5; sigma = 1"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-1normal2.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=.9,m2=1)",
             "mu = 2.5; sigma = 2"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-1normal5.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=.9,m2=1)",
             "mu = 2.5; sigma = 5"),
           fileConn)
close(fileConn)


fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-2normalpt25.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=1,m2=.1)",
             "mu = 2.5; sigma = 0.25"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-2normalpt5.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=1,m2=.1)",
             "mu = 2.5; sigma = 0.5"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-2normal1.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=1,m2=.1)",
             "mu = 2.5; sigma = 1"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-2normal2.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=1,m2=.1)",
             "mu = 2.5; sigma = 2"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-2normal5.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=1,m2=.1)",
             "mu = 2.5; sigma = 5"),
           fileConn)
close(fileConn)


# UNIFORM

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniformpt1.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=.1,m2=.1)"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform1.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=1,m2=1)"),
           fileConn)
close(fileConn)

fileConn<-file("~/Dropbox/optimalControlProject/discretize-optimize/scriptsBatch/initialConditions/inits-uniform10.R")
writeLines(c("inits = c(P=1,V=.1,I=0,L=0.0001)",
             "other = c(pen=0,obj=0)",
             "mParms = c(m1=10,m2=10)"),
           fileConn)
close(fileConn)