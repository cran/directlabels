library(ks)
x <- rmvnorm.mixt(100)
N <- c("x","y")
H <- Hpi(x)
epts <- do.call(expand.grid,alply(x,2,function(v)seq(min(v),max(v),l=20)))
epts <- as.matrix(epts)
colnames(epts) <- N
fhat1 <- kdde(x=x, H=H, deriv.order=1, eval.points=epts)
library(ggplot2)
pts <- data.frame(x)
names(pts) <- N
arr <- with(fhat1,data.frame(eval.points,eval.points+estimate))
names(arr) <- c(N,"xend","yend")
garr <- arrow(length=unit(0.01,"native"), angle=30)
qplot(x,y,data=pts)+
  geom_segment(aes(xend=xend,yend=yend),data=arr,arrow=garr,alpha=0.5)
