library(lattice)
library(directlabels)

# Ne marche pas
p<-xyplot(1:10~1:10, col=grey(1:10/10))
direct.label(p)

# Ne marche pas non plus
p<-xyplot(1:10~11:20, col=grey(1:10/10), subscripts=TRUE)
direct.label(p)

# Marche, mais avec couleurs
p<-xyplot(1:10~1:10, col=grey(1:10/10), groups=1:10)
direct.label(p)

## desired result
N <- 10
x <- rnorm(N)
d <- data.frame(x,y=3*x+rnorm(N),i=1:N)
p <- qplot(x,y,data=d,geom="point")
p+geom_dl(aes(label=i),"smart.grid")
