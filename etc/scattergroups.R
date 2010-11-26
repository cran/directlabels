library(directlabels)
data(mpg,package="ggplot2")
## direct label simple ggplot2 scatterplot
scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
                 main="Fuel efficiency depends on car size")
slab <- direct.label(scatter,list(extreme.grid,dl.move("suv",15,15)))
print(slab)

## scatterplot in lattice:
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
mpg.scatter <- xyplot(jitter(.resid)~jitter(.fitted),mpgf,groups=factor(cyl))
plot(direct.label(mpg.scatter))


for(p in list({
data(mpg,package="ggplot2")
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
p <- xyplot(.resid~.fitted,mpgf,groups=factor(cyl))
},
{
data(mpg,package="ggplot2")
p <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
      main="Fuel efficiency depends on car size")
})){
  print(direct.label(p,"empty.grid"))
  print(direct.label(p,"empty.grid.2"))
  print(direct.label(p,"empty.grid.collide"))
  print(direct.label(p,"extreme.grid"))
  print(direct.label(p,"extreme.points"))
  print(direct.label(p,"follow.points"))
  print(direct.label(p,"get.means"))
  print(direct.label(p,"perpendicular.lines"))
}

