
## FIGURE 1
xA = c(x1=1,x2=0.25)
mu = 2.5; sigma = 5;
source("~/Dropbox/optimalControlProject/king-roughgarden/king-roughgarden-forward-backward-normal.R") #### Edit as needed 

xA = c(x1=1,x2=0.25)
mu = 2.5; sigma = 4;
source("~/Dropbox/optimalControlProject/king-roughgarden/king-roughgarden-forward-backward-normal.R") #### Edit as needed 

xA = c(x1=1,x2=0.25)
mu = 2.5; sigma = 3;
source("~/Dropbox/optimalControlProject/king-roughgarden/king-roughgarden-forward-backward-normal.R") #### Edit as needed 

xA = c(x1=1,x2=0.25)
mu = 2.5; sigma = 2;
source("~/Dropbox/optimalControlProject/king-roughgarden/king-roughgarden-forward-backward-normal.R") #### Edit as needed 

xA = c(x1=1,x2=0.25)
mu = 2.5; sigma = 1;
source("~/Dropbox/optimalControlProject/king-roughgarden/king-roughgarden-forward-backward-normal.R") #### Edit as needed 

xA = c(x1=1,x2=0.25)
mu = 2.5; sigma = 0.5;
source("~/Dropbox/optimalControlProject/king-roughgarden/king-roughgarden-forward-backward-normal.R") #### Edit as needed 

# read rds
nt=101
tVals = seq(0,5,length.out=nt)

directory = "~/Dropbox/optimalControlProject/solutions/"
solutionFiles <- paste0(directory,list.files(directory))

sigma5 <- readRDS(solutionFiles[[12]])
sigma4 <- readRDS(solutionFiles[[11]])
sigma3 <- readRDS(solutionFiles[[10]])
sigma2 <- readRDS(solutionFiles[[9]])
sigma1 <- readRDS(solutionFiles[[8]])
sigma.5 <- readRDS(solutionFiles[[7]])


pdf(file=paste0("~/Dropbox/optimalControlProject/figures/kingRoughgardenNormalSummary.pdf"),height=4,width=8)

par(mfrow=c(1,2))
plot(tVals[(1:(nt-1))],sigma5[(1:(nt-1))],
     type='n',ylim=c(0,1),
     xlab="Time (t)",ylab="u(t)")

lines(tVals[(1:(nt-1))],sigma5[(1:(nt-1))], lty = 'solid', col = "#f7f7f7")
lines(tVals[(1:(nt-1))],sigma4[(1:(nt-1))], lty = 'solid', col = "#d9d9d9")
lines(tVals[(1:(nt-1))],sigma3[(1:(nt-1))], lty = 'solid', col = "#bdbdbd")
lines(tVals[(1:(nt-1))],sigma2[(1:(nt-1))], lty = 'solid', col = "#969696")
lines(tVals[(1:(nt-1))],sigma1[(1:(nt-1))], lty = 'solid', col = "#636363")
lines(tVals[(1:(nt-1))],sigma.5[(1:(nt-1))], lty = 'solid', col = "#252525")

legend("topright",
       c("5","4","3","2","1","0.5"),
       col = c('#f7f7f7','#d9d9d9','#bdbdbd','#969696','#636363','#252525'),
       lty=c("solid"),
       bty="n", inset=c(0,0),cex=0.75)

f <- function(x) {
        out <- tVals[x>.25&x<1]
        return(max(out)-min(out))
}

controls = list(sigma5,sigma4,sigma3,sigma2,sigma1,sigma.5)
plot(c(5,4,3,2,1,.5),unlist(lapply(controls,f))/5,
     type='b',pch=16, xlim= c(0,5), ylim = c(0,0.5),
     xlab = "Standard deviation of normal distribution",
     ylab = "Proportion of time control is singular")

dev.off()

