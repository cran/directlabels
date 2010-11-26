install.packages("directlabels",repos="http://r-forge.r-project.org/")
library(directlabels)

p <- qplot(data = mtcars, mpg, hp, geom = "point", colour = factor(cyl))

p1 <- p + opts(legend.position = "none")

p2 <- ggplot(mtcars, aes(x=mpg, group=cyl, colour=factor(cyl)))
p2 <- p2 + stat_density(fill = NA, position="dodge")
p2 <- p2 + opts(legend.position = "none", axis.title.x=theme_blank(), 
      axis.text.x=theme_blank())

p3 <- ggplot(mtcars, aes(x=hp, group=cyl, colour=factor(cyl)))
p3 <- p3 + stat_density(fill = NA, position="dodge") + coord_flip()
p3 <- p3 + opts(legend.position = "none", axis.title.y=theme_blank(), 
      axis.text.y=theme_blank())

legend <- p + opts(keep= "legend_box")

## Plot Layout Setup
Layout <- grid.layout( nrow = 2, ncol = 2,
   widths = unit (c(2,1), c("null", "null")),
   heights = unit (c(1,2), c("null", "null")) 
)
vplayout <- function (...) {
 grid.newpage()
 pushViewport(viewport(layout= Layout))
}
subplot <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

# Plotting
vplayout()
print(direct.label(p1), vp=subplot(2,1))
print(direct.label(p2,top.points), vp=subplot(1,1))
print(direct.label(p3,list(top.points,hjust=0,vjust=0.5)), vp=subplot(2,2))
##print(legend, vp=subplot(1,2))
