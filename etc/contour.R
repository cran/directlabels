volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
v <- ggplot(volcano3d, aes(x, y, z = z))
v2 <- v + stat_contour(aes(colour = ..level..))
v2+geom_text(position=position_dl(list(top.points),FALSE,v2))
v2+geom_text(position=position_dl(list(top.points),FALSE,v2),stat="contour")+
  scale_colour_continuous(legend=FALSE)
direct.label(v2,top.points,TRUE)
direct.label(v2,top.points)
direct.label(v2)
v2+geom_text(position=position_dl(list(top.points),TRUE,v2),stat="contour")+
  scale_colour_continuous(legend=FALSE)
v2+geom_text(position=position_dl(list(top.points),FALSE,v2),stat="contour")+
  scale_colour_discrete(legend=FALSE)

##positive control
data(mpg,package="ggplot2")
scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
                 main="Fuel efficiency depends on car size")
direct.label(scatter,list(extreme.grid,dl.move("suv",15,15)))
