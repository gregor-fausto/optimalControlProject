library(tidyverse)
library(ggtree)

# just primary division
t0 <- "(P,P);"

f <- function(x){
  read.tree(text=x)
}
tree<-f(t0)

# create the basic plot
p <- ggtree(tree)

# add node points
p0 = p + geom_nodepoint(color=rep(c('red'),1),size=4) +
  geom_tippoint(color=rep(c('dark green'),2),size=4) +
  geom_tiplab(hjust=-1) 

# just primary division
t1 <- "((P,P),(P,P));"

f <- function(x){
  read.tree(text=x)
}
tree<-f(t1)

# create the basic plot
p <- ggtree(tree)

# add node points
p1 = p + geom_nodepoint(color=rep(c('red'),3),size=4) +
  geom_tippoint(color=rep(c('dark green'),4),size=4) +
  geom_tiplab(hjust=-1) 


# just primary division
t2 <- "(((I,F),(I,F)),((I,F),(I,F)));"


f <- function(x){
  read.tree(text=x)
}
tree<-f(t2)

# create the basic plot
p <- ggtree(tree)

# add node points
p2 = p + geom_nodepoint(color=rep(c('red'),7),size=4) +
  geom_tippoint(color=rep(c('green','orange'),4),size=4) +
  geom_tiplab(hjust=-1) 

# just primary division
t3 <- "((((I,F),F),((I,F),F)),(((I,F),F),((I,F),F)));"


f <- function(x){
  read.tree(text=x)
}
tree<-f(t3)

# create the basic plot
p <- ggtree(tree)

# add node points
p3 = p + geom_nodepoint(color=c('red','red','red','green','red','green','red','red','green','red','green'),size=4) +
  geom_tippoint(color=ifelse(tree$tip.label=="I","green","orange"),size=4) +
  geom_tiplab(hjust=-1) 

gridExtra::grid.arrange(p0,p1,p2,p3,ncol=1)


f(t1)$edge

tree.sim = list(matrix(c(1,2),nrow=1),)

