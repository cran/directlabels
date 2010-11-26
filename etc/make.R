pdfpng <- function(N,P,...){
  pdf.file <- paste(N,".pdf",sep="")
  png.file <- paste(N,".png",sep="")
  pdf(pdf.file,...)
  plot(P)
  dev.off()
  system(paste("convert",pdf.file,png.file))
  system(paste("display",png.file,"&"))
}

library(lattice)
library(proto)
library(ggplot2)
data(mpg)
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
pdfpng("scatter",dl(xyplot,mpgf,.resid~.fitted,factor(cyl)))

loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
pdfpng("density",dl(densityplot,loci,~ppp,type,n=500))

data(BodyWeight,package="nlme")
pdfpng("longitudinal",dl(xyplot,BodyWeight,weight~Time|Diet,Rat,
                         type='l',layout=c(3,1)),h=7,w=14)
pdfpng("longitudinal-custom-panel-groups",
       dl(xyplot,bw,weight~Time|Diet,Rat,type="l",layout=c(3,1),
          panel=panel.superpose,panel.groups=panel.model,method=first.points)
       ,h=7,w=14)
pdfpng("longitudinal-custom-panel",
       dl(xyplot,bw,weight~Time|Diet,Rat,type="l",layout=c(3,1),
          panel=panel.range)
       ,h=7,w=14)
pdfpng("longitudinal-custom-both",
       dl(xyplot,bw,weight~Time|Diet,Rat,type="l",layout=c(3,1),
          panel=panel.range,panel.groups=panel.model,method=first.points)
       ,h=7,w=14)

## Say we want to use a simple linear model to explain rat body weight:
fit <- lm(weight~Time+Diet+Rat,BodyWeight)
bw <- fortify(fit,BodyWeight)
## And we want to use this panel function to display the model fits:
panel.model <- function(x,subscripts,col.line,...){
  panel.xyplot(x=x,subscripts=subscripts,col.line=col.line,...)
  llines(x,bw[subscripts,".fitted"],col=col.line,lty=2)
}
## Just specify the custom panel function as usual:
dl(xyplot,bw,weight~Time|Diet,Rat,
   type='l',layout=c(3,1),panel=panel.model)
pdfpng("longitudinal-custom",
       dl(xyplot,bw,weight~Time|Diet,Rat,
          type='l',layout=c(3,1),panel=panel.model)
       ,h=7,w=14)

source("compare.R")
png("compare.png",h=1000,w=500)
compare.methods(c("get.means","perpendicular.lines","empty.grid","empty.grid.2"),
                xyplot,mpgf,.resid~.fitted,factor(class))
dev.off()

png("compare-long.png",h=800,w=1000)
compare.methods(c("first.points","last.points"),
                xyplot,BodyWeight,weight~Time|Diet,Rat,type="l",layout=c(3,1))
dev.off()


pdf("method.pdf");direct.label(dotplot(VADeaths,type="l",xlim=c(5,85)),method=list("last.points",rot=30));dev.off();system("evince method.pdf")

pdf("method2.pdf",h=4)
complicated <- list(dl.trans(x=x+10),
                    dl.indep(d[-2,]),
                    rot=c(30,180))
direct.label(
             dotplot(VADeaths,type="o")
             ,method=complicated)
dev.off()
system("convert -density 2000 -geometry 800x600 method2.pdf method2.png && display method2.png")
