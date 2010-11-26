## Compare with manual addition of direct labels:
vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
p2 <- qplot(deaths,age,data=vad,group=demographic,geom="line",colour=demographic)+xlim(5,80)
p2+geom_text(aes(label=demographic),subset=.(age==levels(age)[nlevels(age)]),hjust=0,angle=30)+opts(legend.position="none")
direct.label(p2,function(d,...)data.frame(subset(d,y==max(y)),hjust=0,rot=30))
direct.label(p2,list(dl.indep(d[which.max(d$x),]),hjust=0,rot=30))
direct.label(p2,list(last.points,rot=30))
ggsave("ex.pdf");system("xpdf ex.pdf")


## rat body weight data:
data(BodyWeight,package="nlme")
library(lattice)
qpdl <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type="l",layout=c(3,1))
label.lineplot <- function(x,y,group.number,col.line,...){
  panel.xyplot(x=x,y=y,group.number=group.number,col.line=col.line,...)
  i <- which.max(x)
  ltext(x[i],y[i],levels(BodyWeight$Rat)[group.number],
        adj=c(0,0.5),col=col.line)
}
update(qpdl,panel=panel.superpose,panel.groups=label.lineplot)
library(ggplot2)
qpdl <- qplot(Time,weight,data=BodyWeight,colour=Rat,geom="line",facets=.~Diet)
qpdl+geom_text(aes(label=Rat),subset=.(Time==min(Time)),hjust=1)+opts(legend.position="none")
library(directlabels)
direct.label(qpdl,function(d,...)data.frame(subset(d,x==min(x)),hjust=1))
direct.label(qpdl,list(dl.indep(d[which.max(d$x),]),hjust=1))
direct.label(qpdl,first.points)
direct.label(qpdl)
ggsave("rat-ggplot2.png",dpi=60);system("display rat-ggplot2.png")

f="../www/rat-ggplot2-firstlast.png";w=1.5;png(f,h=450*w,w=450*w,res=50,pointsize=16);dlcompare(list(p),list("first.points","last.points"),FALSE,"");dev.off();system(paste("display",f))

loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
## ggplot2 plot
dp <- qplot(ppp,data=loci,colour=type,geom="density")
## ggplot2 direct labels by hand
dens <- ddply(loci,.(type),function(l)
              subset(data.frame(density(l$ppp)[c("x","y")]),y==max(y)))
dp+geom_text(aes(x=x,y=y,label=type,vjust=0),dens)+opts(legend.position="none")
## lattice plot
dp <- densityplot(~ppp,loci,groups=type,n=500)
## lattice plot direct labels by hand
label.densityplot <- function(x,group.number,col.line,...){
  panel.densityplot(x=x,group.number=group.number,col.line=col.line,...)
  d <- density(x)
  i <- which.max(d$y)
  ltext(d$x[i],d$y[i],levels(loci$type)[group.number],adj=c(0.5,0),col=col.line)
}
update(dp,panel=panel.superpose,panel.groups=label.densityplot)

## direct.label is shorter and works with both ggplot2 and lattice
direct.label(dp,list(function(d,...)ddply(d,.(groups),subset,y==max(y)),vjust=0))
direct.label(dp,list(dl.indep(d[which.max(d$y),]),vjust=0))
direct.label(dp,top.points)
direct.label(dp)
