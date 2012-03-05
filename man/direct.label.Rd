\name{direct.label}
\alias{direct.label}
\alias{directlabels}
\title{Direct labels for color decoding}
\description{Add direct labels to a plot, and hide the color legend. Modern
plotting packages like lattice and ggplot2 show automatic legends
based on the variable specified for color, but these legends can
be confusing if there are too many colors. Direct labels are a
useful and clear alternative to a confusing legend in many common
plots.}
\usage{direct.label(p, method = NULL, debug = FALSE)}
\arguments{
  \item{p}{The "lattice" or "ggplot" object with things drawn in different
colors.}
  \item{method}{Positioning Method, which determines the positions of the direct
labels as a function of the plotted data. If NULL, we examine the
plot p and try to choose an appropriate default. See ?apply.method
for more information about Positioning Methods.}
  \item{debug}{Show debug output?}
}

\value{A plot with direct labels and no color legend.}

\author{Toby Dylan Hocking}




\examples{
library(ggplot2)
## direct label simple ggplot2 scatterplot
scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
                 main="Fuel efficiency depends on car size")
print(direct.label(scatter))
## Use a different Positioning Method, and edit one of the label
## positions by hand
print(direct.label(scatter,list("extreme.grid",dl.move("suv",15,15))))

## scatterplot in lattice
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
library(lattice)
oldopt <- lattice.options(panel.error=NULL)
mpg.scatter <- xyplot(jitter(.resid)~jitter(.fitted),mpgf,groups=factor(cyl))
plot(direct.label(mpg.scatter))
## debug=TRUE shows more output on the plot, and data.tables of the
## transformations from the original data to the direct labels
plot(direct.label(mpg.scatter,debug=TRUE))
## bigger text works better here. With the smart.grid Positioning
## Method, the search grid size is the same size as the text box, so
## the grid is bigger here.
plot(direct.label(mpg.scatter,list(cex=1.5,smart.grid),TRUE))

## direct labels are not as good when there are multiple panels, but
## as long as not every class appears in every panel, it still
## should be clearer than using a legend.
carpanels <- xyplot(jitter(hwy)~jitter(cty)|manufacturer,mpgf,groups=class,
main="City and highway fuel efficiency depends on manufacturer and car class")
print(direct.label(carpanels))


if(require(mlmRev)){
  data(Chem97)
  qqm <- qqmath(~gcsescore,Chem97,groups=gender,
                f.value=ppoints(25),auto.key=list())
  plot(direct.label(qqm,list("get.means","empty.grid"),TRUE))
  plot(direct.label(qqm,list("get.means","smart.grid"),TRUE))
  ## default for points is different for default for lines
  plot(direct.label(update(qqm,type=c("l","g"))))
  ## you can hard-core label positions if you really want to:
  plot(direct.label(qqm,static.labels(c(-2,0),c(6,4),c("F","M")),TRUE))
  plot(direct.label(qqmath(~gcsescore|gender,Chem97,groups=factor(score),
                           type=c('l','g'),f.value=ppoints(100))))

  ## densityplot labeling
  plot(direct.label(densityplot(~gcsescore,Chem97,groups=factor(score),
                                plot.points=FALSE,n=500)))
  ## Try with several panels:
  plot(direct.label(densityplot(~gcsescore|gender,Chem97,plot.points=FALSE,
                                groups=factor(score),layout=c(1,2),n=500)))
}
require(reshape2)
iris2 <- melt(iris,id="Species")
direct.label(densityplot(~value|variable,iris2,groups=Species,scales="free"))
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
plot(direct.label(densityplot(~ppp,loci,groups=type,n=500)))
lplot <- qplot(ppp,data=loci,colour=type,geom="density")
print(direct.label(lplot))
direct.label(lplot,static.labels(c(0,0.5,1),0,c("POS","NEU","BAL"),vjust=1.1))
## respect the manual color scale. these 2 should be the same:
lplot2 <- direct.label(lplot)+
  scale_colour_manual(values=c("red","black","blue"),guide="none")
print(lplot2)
print(direct.label(lplot+scale_colour_manual(values=c("red","black","blue"))))

## dotplot:
plot(direct.label(dotplot(VADeaths,type="o"),"angled.endpoints"))
plot(direct.label(dotplot(VADeaths,type="o"),
       list(cex=0.7,dl.trans(y=y+0.1),"top.qp")))
VAD2 <- VADeaths
colnames(VAD2) <- sub(" ","\n",colnames(VAD2))
plot(direct.label(dotplot(VAD2,type="o"),
                  list(dl.trans(y=y+0.1),top.qp)))
## Try the same plot with ggplot2
vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
p2 <- qplot(deaths,age,data=vad,
            group=demographic,geom="line",colour=demographic)
print(direct.label(p2,angled.endpoints)+xlim(5,80))
vad2 <- as.data.frame.table(VAD2)
names(vad2) <- names(vad)
p3 <- qplot(deaths,age,data=vad2,
            group=demographic,geom="line",colour=demographic)
direct.label(p3,"top.qp")
## contour plot --- labels each piece, not each group!
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
v <- ggplot(volcano3d, aes(x, y, z = z,colour=..level..))+
  stat_contour()
direct.label(v)
direct.label(v,"top.pieces")

## label 2 groups of longitudinal data:
dts <- cbind(male=mdeaths,female=fdeaths,time=1:length(mdeaths))
ddf <- melt(as.data.frame(dts),id="time")
names(ddf) <- c("time","sex","deaths")
plots <- list(lattice=xyplot(deaths~time,ddf,groups=sex,type="l"),
              ggplot2=qplot(time,deaths,data=ddf,colour=sex,geom="line"))
oask <- devAskNewPage(TRUE)
for(p in plots)print(direct.label(p)) ## should default to lines2
devAskNewPage(oask)

## lineplot labeling
sprayplot <- xyplot(decrease~treatment,OrchardSprays,groups=rowpos,type="a")
direct.label(sprayplot)
data(BodyWeight,package="nlme")
ratplot <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
print(direct.label(ratplot))#default is maxvar.qp
print(direct.label(ratplot,"last.points"))#squished together
print(direct.label(ratplot,"last.qp")) ## readable
## Say we want to use a simple linear model to explain rat body weight:
fit <- lm(weight~Time+Diet+Rat,BodyWeight)
bw <- fortify(fit,BodyWeight)
## And we want to use this panel function to display the model fits:
panel.model <- function(x,subscripts,col.line,...){
  panel.xyplot(x=x,subscripts=subscripts,col.line=col.line,...)
  llines(x,bw[subscripts,".fitted"],col=col.line,lty=2)
}
## Just specify the custom panel function as usual:
rat2 <- update(ratplot,
               panel=panel.superpose,
               panel.groups=panel.model)
print(direct.label(rat2,"last.qp"))

## complicated ridge regression lineplot ex. fig 3.8 from Elements of
## Statistical Learning, Hastie et al.
myridge <- function(f,data,lambda=c(exp(-seq(-15,15,l=200)),0)){
  require(MASS)
  fit <- lm.ridge(f,data,lambda=lambda)
  X <- data[-which(names(data)==as.character(f[[2]]))]
  Xs <- svd(scale(X)) ## my d's should come from the scaled matrix
  dsq <- Xs$d^2
  ## make the x axis degrees of freedom
  df <- sapply(lambda,function(l)sum(dsq/(dsq+l)))
  D <- data.frame(t(fit$coef),lambda,df) # scaled coefs
  require(reshape2)
  molt <- melt(D,id=c("lambda","df"))
  ## add in the points for df=0
  limpts <- transform(subset(molt,lambda==0),lambda=Inf,df=0,value=0)
  rbind(limpts,molt)
}
if(require(ElemStatLearn)){
  data(prostate)
  pros <- subset(prostate,train==TRUE,select=-train)
  m <- myridge(lpsa~.,pros)
  p <- xyplot(value~df,m,groups=variable,type="o",pch="+",
              panel=function(...){
                panel.xyplot(...)
                panel.abline(h=0)
                panel.abline(v=5,col="grey")
              },
              main="Ridge regression shrinks least squares coefficients",
              ylab="scaled coefficients",
              sub="grey line shows coefficients chosen by cross-validation",
              xlab=expression(df(lambda)))
  print(direct.label(update(p,xlim=c(0,9.25)),
                     list(cex=1.5,last.qp,dl.trans(x=x+0.1))))
}  
## some data from clustering algorithms
data(iris.l1.cluster,package="directlabels")
ilc <- transform(iris.l1.cluster,coefficient=alpha)
p <- ggplot(ilc,aes(lambda,coefficient,group=row,colour=Species))+
  geom_line(alpha=1/4)+
  facet_grid(col~.)
p2 <- p+xlim(-0.0025,max(iris.l1.cluster$lambda))
print(direct.label(p2,list("first.points","get.means","first.qp")))

data(normal.l2.cluster,package="directlabels")
p <- ggplot(normal.l2.cluster$path,aes(x,y))+
  geom_path(aes(group=row),colour="grey")+
  geom_point(aes(size=lambda),colour="grey")+
  geom_point(aes(colour=class),data=normal.l2.cluster$pts)+
  coord_equal()
print(direct.label(p))
print(direct.label(p,"extreme.grid"))
## respect the color scale. these should look the same:
print(direct.label(p+scale_colour_manual(values=rainbow(8))))
print(direct.label(p)+scale_colour_manual(values=rainbow(8),guide="none"))

lattice.options(oldopt)
}
