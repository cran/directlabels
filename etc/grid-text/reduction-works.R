library(grid)
grid.newpage()
s <- "foo"
getwh <- function(cex.val){
  pushViewport(viewport(gp=gpar(cex=cex.val)))
  size <- c(width=convertWidth(stringWidth(s),"cm"),
            height=convertHeight(stringHeight(s),"cm"))
  popViewport()
  size
}
wh <- getwh(1)
grid.text(s,0,0,hjust=0,vjust=0)
grid.rect(0,0,wh["width"],wh["height"],hjust=0,vjust=0,default.units="cm")
cex <- seq(1.1,3,l=50)
cm <- sapply(cex,getwh)
frac <- wh/cm
cex.est <- frac["height",]*cex
print(cex.est)
h.est <- sapply(cex.est,function(cex.val){
  grid.text(s,0,0,hjust=0,vjust=0,gp=gpar(cex=cex.val))
  getwh(cex.val)["height"]
})
stopifnot(all(h.est == wh["height"]))

