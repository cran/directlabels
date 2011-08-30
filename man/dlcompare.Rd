\name{dlcompare}
\alias{dlcompare}
\title{Direct label comparison plot}
\description{Compare several plots and/or label placement methods. This creates
a custom grid graphics display based on lattice and/or ggplot2
output. This is possible because the direct.label function is
generic. Plots will be on the columns and positioning methods will
be on the rows.}
\usage{
dlcompare(plots,
pos.funs,
rects = TRUE,
row.items = "plots",
debug = FALSE)
}
\arguments{
  \item{plots}{List of ggplot2 or lattice plots. List names will be used to
annotate the plot.}
  \item{pos.funs}{List of label placement methods to apply to each plot. List names,
or function names if specified as character strings, will be used
to annotate the plot.}
  \item{rects}{Draw rectangles around each plot, creating a grid?}
  \item{row.items}{If "plots" then put plots on the rows and method on the
columns. Otherwise, do the opposite.}
  \item{debug}{Show debug output?}
}



\author{Toby Dylan Hocking <toby.hocking@inria.fr>}




\examples{
library(lattice)
library(ggplot2)
oldopt <- lattice.options(panel.error=NULL)
dts <- cbind(male=mdeaths,female=fdeaths,time=1:length(mdeaths))
ddf <- melt(as.data.frame(dts),id="time")
names(ddf) <- c("time","sex","deaths")
plots <- list(lattice=
              xyplot(deaths~time,ddf,groups=sex,type="l",xlim=c(-15,80)),
              ggplot2=
              qplot(time,deaths,data=ddf,colour=sex,geom="line")+xlim(-10,80))
if(names(dev.cur())!="postscript"){##to avoid error on pkg check
  ## Try some more exotic labeling options.
  exotic <- list("last.points",
                 rot=c(0,180),
                 fontsize=c(10,20),
                 fontface=c("bold","italic"),
                 fontfamily=c("mono","serif"),
                 alpha=c(0.25,1))
  dlcompare(plots,list(exotic))
}

pos.funs <- list("first.points","lines2")
##pdf("compare.pdf",width=10,height=10)
dlcompare(plots,pos.funs)
dlcompare(plots,pos.funs,rects=FALSE) ## hide the grid
dlcompare(plots,pos.funs,rects=FALSE,row.items="posfuns") ## exchange axes
##dev.off();system("xpdf compare.pdf")
## All of these subsets should produce valid comparison plots.
dlcompare(plots[1],pos.funs[1])
dlcompare(plots[1],pos.funs)
dlcompare(plots,pos.funs[1])
named.funs <- list(first.points=first.points,lines2=lines2)
mixed.funs <- list("first.points",lines2=lines2,last.points)
not.named <- structure(named.funs,names=NULL)
unlabeled.plots <- structure(plots,names=NULL)
dlcompare(plots,mixed.funs)
dlcompare(plots,mixed.funs[3])
data(BodyWeight,package="nlme")
ratplot <- xyplot(weight~Time|Diet,BodyWeight,
                  groups=Rat,type="l",layout=c(3,1))
##   ggrat <- qplot(Time,weight,data=BodyWeight,
##                  colour=Rat,geom="line",facets=.~Diet)
##   png("~/R/directlabels/www/rat-ggplot2-firstlast.png",h=400,w=900)
##   dlcompare(list(ggrat+ylim(130,max(BodyWeight$weight))),
##             list("first.qp","last.qp"),row.items="pfs",rects=FALSE)
##   dev.off()
## Compare scatterplot labeling methods.
scatters <-
  list(xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,aspect=1),
       xyplot(jitter(Sepal.Length)~jitter(Petal.Length),iris,groups=Species),
       qplot(jitter(Petal.Length),jitter(Sepal.Length),
             data=iris,colour=Species))
##pdf("scattercompare.pdf",width=10,height=5)
s.methods <- list("empty.grid","smart.grid")
dlcompare(scatters[1:2],s.methods)
dlcompare(scatters[2:3],s.methods)
##dev.off();system("xpdf scattercompare.pdf")
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
if(require(ElemStatLearn)){
  data(prostate)
  pros <- subset(prostate,train==TRUE,select=-train)
  m <- myridge(lpsa~.,pros)
  p <- xyplot(value~df,m,groups=variable,type="o",pch="+",
              panel=function(...){
                panel.xyplot(...)
                panel.abline(h=0)
              },
              xlim=c(0,10),
              auto.key=list(space="right",lines=TRUE,points=FALSE),
              ylab="scaled coefficients",
              xlab=expression(df(lambda)))
  dlcompare(list(p,ratplot),list("last.bumpup","last.qp"))
}
## direct labels are way less confusing here
p2 <- qplot(df,value,data=m,group=variable,colour=variable,
            geom=c("line","point"))+geom_hline(yintercept=0)+xlim(0,9)
pfuns <- list("legend","direct labels"=
              list(cex=2,last.qp,dl.trans(x=x+0.1)),
              list(cex=c(0.5,1,2,4),last.qp,dl.trans(x=x+0.1),
                   calc.boxes,draw.rects))
dlcompare(list(p),pfuns,rects=FALSE,row.items="posfuns")
dlcompare(list(p,p2),pfuns[1:2],rects=FALSE)
vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
## color + legend
leg <- ggplot(vad,aes(deaths,age,colour=demographic))+
  geom_line(aes(group=demographic))
dlcompare(list(leg),list(list(cex=0.5,"top.qp"),list("last.points",rot=30)))

lattice.options(oldopt)
}
