## idea: use this instead of chull!!
library(alphahull)
ukoch <- rkoch(2000,side=1,niter=3)
ah <- ahull(ukoch,alpha=0.5)
as <- ashape(ukoch,alpha=5)
plot(as)
## the lines that are plotted are in as$edges, more specifically the
## segments runs from (x1,y1)-(x2,y2)

## choice of alpha: very easy, just take the max(box height, box width)

## only tricky part: need to make sure units are the same on x and y:
ukoch[,2] <- ukoch[,2]*10
plot(ukoch) ## looks the same
plot(ashape(ukoch,alpha=0.1))##not a good solution
## actually fitting ahull in this space
xyplot(X2~X1,data.frame(ukoch),aspect="iso")

##try on iris data.
p <- xyplot(jitter(Sepal.Length)~jitter(Petal.Length),iris,group=Species,aspect="iso")
library(directlabels)
direct.label(p,closest.on.ahull)
