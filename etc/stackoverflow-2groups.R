y <- rnorm(100, 10)
y2 <- rnorm(100, 20)
x <- 1:100
treatment <- rep(c("one group","another"),each=length(x))
df <- data.frame(x=c(x,x),y=c(y,y2),treatment)
library(lattice)
p <- xyplot(y~x,df,groups=treatment,type="l")
if(!require(directlabels)){
  install.packages("directlabels")
  library(directlabels)
}
print(direct.label(p))
print(direct.label(update(p,xlim=c(0,120)),last.points))
