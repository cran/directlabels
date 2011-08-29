volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
p <- ggplot(volcano3d, aes(x, y, z = z))+
  stat_contour(aes(colour = ..level..))
library(directlabels)
p+geom_text(position=position_dl(list(top.points),FALSE,p))
p+geom_text(position=position_dl(list(top.points),FALSE,p),stat="contour")+
  scale_colour_continuous(legend=FALSE)
direct.label(p,top.points,TRUE)
direct.label(p,top.points)
direct.label(p)
top.and.bottom <- gapply.fun({
  m <- if(d$piece[1]%%2)"top.points" else "bottom.points"
  apply.method(m,d)
})
## BUG TOFIX: custom number of bins/binwidhts
direct.label(p,top.and.bottom)
p+geom_text(position=position_dl(list(top.points),TRUE,p),stat="contour")+
  scale_colour_continuous(legend=FALSE)
p+geom_text(position=position_dl(list(top.points),FALSE,p),stat="contour")+
  scale_colour_discrete(legend=FALSE)

##positive control
data(mpg,package="ggplot2")
scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
                 main="Fuel efficiency depends on car size")
direct.label(scatter,list(extreme.grid,dl.move("suv",15,15)))
