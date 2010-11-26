concours <- read.csv("concours.csv")
p <- xyplot(Tx.Sup ~ Tx.TB, concours,group=Concours, type="o",aspect="iso")
library(directlabels)
direct.label(update(p,xlim=c(5,100)),last.qp)

## Just rotate the labels according to the slope of the line
calc.angle <- dl.indep({
  rd <- within(d,{
    dx <- x[1]-x[2]
    dy <- y[1]-y[2]
    rot <- atan(dy/dx)/pi*180
  })[1,]
  data.frame(d,rot=rd$rot)
})
direct.label(p,calc.angle)

## Label the middle of each line, rotated
angled.midpoints <- list(calc.angle,get.means)
direct.label(p,angled.midpoints)

## Don't collide with the points (doesnt work since there are *lines*)
direct.label(p,list(angled.midpoints,smart.grid))

## Fill in the lines with 30 evenly spaced points to use for collision
## detection
fillin <- dl.indep({
  x <- with(d,seq(x[1],x[2],l=30))
  dx <- with(d,x[1]-x[2])
  dy <- with(d,y[1]-y[2])
  y <- dy/dx*(x-d$x[1])+d$y[1]
  data.frame(x,y,d[,!names(d)%in%c("x","y")],row.names=NULL)
})
direct.label(p,fillin)

## Fill in the lines and rotate the labels
angled.fillin <- list(calc.angle,fillin)
direct.label(p,angled.fillin)

## label the midpoints, so that the filled in points don't collide
direct.label(p,list(angled.fillin,smart.grid))

## at the end of the lines they are more spread out:
big.boxes.last <- function(d,...)enlarge.box(calc.boxes(last.points(d)))
smart.grid.last <- empty.grid.fun(big.boxes.last)
direct.label(p,list(fillin,smart.grid.last))

## combine all of these ideas: fill in the lines, rotate the labels,
## and put them at the end of the lines. This seems to be the clearest
## labeling.
direct.label(p,list(angled.fillin,smart.grid.last))
