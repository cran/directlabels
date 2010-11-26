\name{positioning.functions}
\Rdversion{1.1}
\alias{positioning.functions}
\alias{positioning.methods}
\title{Built-in Positioning Methods for direct label placement}
\description{When adding direct labels to a grouped plot, label
  placement can be specified using a Positioning Method (or a list of
  them), of the form function(d,...), where d is a data frame of the
  points to plot, with columns x y groups. The job of the Positioning
  Method(s) is to return the position of each direct label you want to
  plot as a data frame, with 1 row for each label. Thus normally a
  Positioning Method will return 1 row for each group. Several
  built-in Positioning Methods are discussed below, but you can also
  create your own, either from scratch or by using dl.indep and
  dl.trans.}
\author{Toby Dylan Hocking <toby.hocking@inria.fr>}
\examples{
\dontrun{
### contourplot Positioning Methods
for(p in list({
## Example from help(contourplot)
require(stats)
require(lattice)
attach(environmental)
ozo.m <- loess((ozone^(1/3)) ~ wind * temperature * radiation,
               parametric = c("radiation", "wind"), span = 1, degree = 2)
w.marginal <- seq(min(wind), max(wind), length.out = 50)
t.marginal <- seq(min(temperature), max(temperature), length.out = 50)
r.marginal <- seq(min(radiation), max(radiation), length.out = 4)
wtr.marginal <- list(wind = w.marginal, temperature = t.marginal,
                     radiation = r.marginal)
grid <- expand.grid(wtr.marginal)
grid[, "fit"] <- c(predict(ozo.m, grid))
detach(environmental)
library(ggplot2)
p <- ggplot(grid,aes(wind,temperature,z=fit))+
  stat_contour(aes(colour=..level..))+
  facet_wrap(~radiation)

},
{
## example from help(stat_contour)
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
library(ggplot2)
p <- ggplot(volcano3d, aes(x, y, z = z))+
  stat_contour(aes(colour = ..level..))
})){
  print(direct.label(p,"bottom.points"))
  print(direct.label(p,"top.points"))
}

### densityplot Positioning Methods
for(p in list({
data(Chem97,package="mlmRev")
library(lattice)
p <- densityplot(~gcsescore|gender,Chem97,
            groups=factor(score),layout=c(1,2),
            n=500,plot.points=FALSE)
},
{
iris2 <- melt(iris,id="Species")
library(lattice)
p <- densityplot(~value|variable,iris2,groups=Species,scales="free")
},
{
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
library(ggplot2)
p <- qplot(ppp,data=loci,colour=type,geom="density")
})){
  print(direct.label(p,"top.bumptwice"))
  print(direct.label(p,"top.bumpup"))
  print(direct.label(p,"top.points"))
}

### dotplot Positioning Methods
for(p in list({
library(lattice)
p <- dotplot(VADeaths,xlim=c(8,85),type="o")
},
{
vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
library(ggplot2)
p <- qplot(deaths,age,data=vad,group=demographic,geom="line",colour=demographic)+
  xlim(8,80)
})){
  print(direct.label(p,"angled.endpoints"))
}

### lineplot Positioning Methods
for(p in list({
data(BodyWeight,package="nlme")
library(lattice)
p <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',
       layout=c(3,1),xlim=c(-10,75))
},
{
data(Chem97,package="mlmRev")
library(lattice)
p <- qqmath(~gcsescore|gender,Chem97,groups=factor(score),
       type=c('l','g'),f.value=ppoints(100))
},
{
data(Chem97,package="mlmRev")
library(lattice)
p <- qqmath(~gcsescore,Chem97,groups=gender,
       type=c("l","g"),f.value=ppoints(100))
},
{
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
  molt <- melt(D,id=c("lambda","df"))
  ## add in the points for df=0
  limpts <- transform(subset(molt,lambda==0),lambda=Inf,df=0,value=0)
  rbind(limpts,molt)
}
data(prostate,package="ElemStatLearn")
pros <- subset(prostate,train==TRUE,select=-train)
m <- myridge(lpsa~.,pros)
library(lattice)
p <- xyplot(value~df,m,groups=variable,type="o",pch="+",
       panel=function(...){
         panel.xyplot(...)
         panel.abline(h=0)
         panel.abline(v=5,col="grey")
       },
       xlim=c(-1,9),
       main="Ridge regression shrinks least squares coefficients",
       ylab="scaled coefficients",
       sub="grey line shows coefficients chosen by cross-validation",
       xlab=expression(df(lambda)))
},
{
dts <- cbind(male=mdeaths,female=fdeaths,time=1:length(mdeaths))
ddf <- melt(as.data.frame(dts),id="time")
names(ddf) <- c("time","sex","deaths")
library(ggplot2)
p <- qplot(time,deaths,data=ddf,colour=sex,geom="line")+xlim(-5,77)
})){
  print(direct.label(p,"first.bumpup"))
  print(direct.label(p,"first.points"))
  print(direct.label(p,"first.qp"))
  print(direct.label(p,"last.bumpup"))
  print(direct.label(p,"last.points"))
  print(direct.label(p,"last.qp"))
  print(direct.label(p,"lines2"))
  print(direct.label(p,"maxvar.points"))
  print(direct.label(p,"maxvar.qp"))
}

### scatterplot Positioning Methods
for(p in list({
data(mpg,package="ggplot2")
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
library(lattice)
p <- xyplot(jitter(.resid)~jitter(.fitted)|manufacturer,mpgf,groups=class)
},
{
data(mpg,package="ggplot2")
m <- lm(cty~displ,data=mpg)
mpgf <- fortify(m,mpg)
library(lattice)
p <- xyplot(jitter(.resid)~jitter(.fitted),mpgf,groups=factor(cyl))
},
{
library(lattice)
p <- xyplot(jitter(Sepal.Length)~jitter(Petal.Length),iris,groups=Species)
},
{
data(mpg,package="ggplot2")
library(lattice)
p <- xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,
       main="Fuel efficiency depends on car size")
},
{
library(ggplot2)
data(mpg,package="ggplot2")
p <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
      main="Fuel efficiency depends on car size")
})){
  print(direct.label(p,"closest.on.ahull"))
  print(direct.label(p,"closest.on.chull"))
  print(direct.label(p,"empty.grid"))
  print(direct.label(p,"empty.grid.2"))
  print(direct.label(p,"extreme.grid"))
  print(direct.label(p,"follow.points"))
  print(direct.label(p,"smart.grid"))
}

}
}
