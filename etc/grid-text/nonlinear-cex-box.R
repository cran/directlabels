library(grid)
grid.newpage()
cex <- c(1,seq(1/2,2,l=50))
s <- "foo"
getwh <- function(cex.val){
  pushViewport(viewport(gp=gpar(cex=cex.val)))
  size <- c(width=convertWidth(stringWidth(s),"cm"),
            height=convertHeight(stringHeight(s),"cm"))
  popViewport()
  size
}
wh <- sapply(cex,getwh)
df <- do.call(rbind,lapply(rownames(wh),function(what){
  cm <- wh[what,]
  fraction <- cm/cm[1]
  data.frame(cex,cm,fraction,what)
}))
library(lattice)
library(latticeExtra)
p <- xyplot(fraction~cex|what,df,type=c("g","p"))+
  layer(panel.abline(0,1))
print(p)
