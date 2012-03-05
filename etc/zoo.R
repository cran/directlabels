library(zoo)
set.seed(1)
z <- zoo(cbind(a = 1:5, b = 11:15, c = 21:25) + rnorm(5))
library(lattice)
p <- xyplot(z, screen = 1)
update(p,auto.key=list())
library(directlabels)
direct.label(p,"last.points") ## why in color?

## these 2 should give the same result:
d <- data.frame(time = time(z), coredata(z))
p2 <- xyplot(a + b + c ~ time, d, type = "l", col = 1:3)
direct.label(p2)

p <- xyplot(z, screen = 1, type = "l", col = 1:3)
direct.label(p,"last.points")

#By the way, this one seems to add color to labels when I would have
#thought it would not:

p3 <- xyplot(a + b + c ~ time, d, type = "l", col = 1)
direct.label(p3)

