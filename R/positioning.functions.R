direct.label <- structure(function
### Add direct labels to a plot. This is a S3 generic and there are
### appropriate methods for "trellis" and "ggplot" objects.
(p,
### The plot to which you would like to add direct labels.
 method=NULL,
### The direct label placement method as described in
### ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  if(!is.null(method)&&class(method)=="character"&&method=="legend")
    UseMethod("uselegend")
  else
    UseMethod("direct.label")
### The plot object, with direct labels added.
},ex=function(){
  library(ggplot2)
  ## direct label simple ggplot2 scatterplot
  scatter <- qplot(jitter(hwy),jitter(cty),data=mpg,colour=class,
                   main="Fuel efficiency depends on car size")
  print(direct.label(scatter,list(extreme.grid,dl.move("suv",15,15))))

  ## scatterplot in lattice
  m <- lm(cty~displ,data=mpg)
  mpgf <- fortify(m,mpg)
  library(lattice)
  mpg.scatter <- xyplot(jitter(.resid)~jitter(.fitted),mpgf,groups=factor(cyl))
  plot(direct.label(mpg.scatter))

  ## debug=TRUE shows more output on the plot, and a data table of the
  ## direct labels
  plot(direct.label(mpg.scatter,debug=TRUE))

  ## bigger text works better here. With the smart.grid Positioning
  ## Method, the search grid size is the same size as the text box, so
  ## the grid is bigger here.
  plot(direct.label(mpg.scatter,list(cex=1.5,smart.grid),TRUE))

  ## try custom panel function and title
  mpgs2 <- update(mpg.scatter,
                  panel=function(...){
                    panel.abline(0,col="grey");panel.xyplot(...)},
                  main="foobar2")
  plot(direct.label(mpgs2,list(cex=2,smart.grid)))

  data(Chem97,package="mlmRev")
  qqm <- qqmath(~gcsescore,Chem97,groups=gender,f.value=ppoints(25),auto.key=TRUE)
  plot(direct.label(qqm,empty.grid.fun(get.means)))
  ## default for points is different for default for lines
  plot(direct.label(update(qqm,type=c("l","g"))))
  ## you can hard-core label positions if you really want to:
  static.labels <- data.frame(x=c(-2,0),y=c(6,4),groups=c("F","M"))
  plot(direct.label(qqm,method=static.labels,debug=TRUE))
  plot(direct.label(qqmath(~gcsescore|gender,Chem97,groups=factor(score),
                           type=c('l','g'),f.value=ppoints(100))))

  ## densityplot labeling
  plot(direct.label(densityplot(~gcsescore,Chem97,groups=factor(score),
                                plot.points=FALSE,n=500)))
  ## Try with several panels:
  plot(direct.label(densityplot(~gcsescore|gender,Chem97,plot.points=FALSE,
                                groups=factor(score),layout=c(1,2),n=500)))
  iris2 <- melt(iris,id="Species")
  direct.label(densityplot(~value|variable,iris2,groups=Species,scales="free"))
  loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                     type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
  plot(direct.label(densityplot(~ppp,loci,groups=type,n=500)))
  print(direct.label(qplot(ppp,data=loci,colour=type,geom="density"),top.points))

  ## dotplot:
  plot(direct.label(dotplot(VADeaths,type="o"),angled.endpoints))
  plot(direct.label(dotplot(VADeaths,type="o"),
                    list(cex=0.7,dl.trans(y=y+0.1),top.qp)))
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

  ## contour plot
  volcano3d <- melt(volcano)
  names(volcano3d) <- c("x", "y", "z")
  v <- ggplot(volcano3d, aes(x, y, z = z))
  v2 <- v + stat_contour(aes(colour = ..level..))
  direct.label(v2)
  direct.label(v2,"top.points")

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
  print(direct.label(ratplot))#default is maxvar.points
  print(direct.label(ratplot,last.points))#squished together
  print(direct.label(ratplot,last.qp)) ## not so good actually
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
  print(direct.label(rat2,last.points))

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
                     list(last.qp,cex=0.75,dl.trans(x=x+0.1))))
})

label.positions <- function
### Calculates table of positions of each label based on input data
### for each panel and Positioning Functions. This is meant for
### internal use inside a direct.label method, and is a wrapper around
### eval.list which makes sure the inputs are good and the outputs are
### plottable. eval.list is more efficient and should be used in the
### context of other Positioning Methods (i.e. dl.combine) and
### label.positions should be used when you actually when to plot the
### result (i.e. in lattice+ggplot2 backends).
(d,
### Data frame to which we will sequentially apply the Positioning
### Functions.
 method,
### Method for direct labeling, described in ?eval.list.
 debug=FALSE,
### Show debug output? If TRUE, the resulting table of label positions
### will be printed.
 ...
### Passed to Positioning Function(s).
 ){
  if(nrow(d)==0)return(d)## empty data frames can cause many bugs
  ## make sure input data is in good format
  d <- transform(d,
                 x=as.numeric(x),
                 groups=as.factor(groups))
  if("y"%in%names(d))d <- transform(d,y=as.numeric(y))
  ##save original levels for later in case PFs mess them up.
  levs <- levels(d$groups)
  d <- eval.list(method,d,debug=debug,...)
  if(nrow(d)==0)return(d)## empty data frames can cause many bugs
  ## rearrange factors in case pos fun messed up the order:
  d$groups <- factor(as.character(d$groups),levs)
  ## defaults for grid parameter values:
  for(p in c("hjust","vjust")){
    d[,p] <- if(p %in% names(d))as.character(d[,p]) else NA
    d[is.na(d[,p]),p] <- 0.5
  }
  if(!"rot"%in%names(d))d$rot <- NA
  d$rot[is.na(d$rot)] <- 0
  if(!"cex"%in%names(d))d$cex <- NA
  d$cex[is.na(d$cex)] <- 1
  d <- unique(d)
  if(debug)print(d)
  d
### Data frame of direct label positions. Each row describes the
### position of 1 label to be drawn later.
}

eval.list <- function # Evaluate Positioning Method list
### Run all the Positioning Functions on a given data set. This
### function contains all the logic for parsing the method= argument
### and sequentially applying the Positioning Functions to the input
### data to obtain the label positions. This is useful since it is
### often much less verbose to define Positioning Methods in list form
### instead of function form, ex lasso.labels.
(method,
### Direct labeling Positioning Method, specified in one of the
### following ways: (1) a Positioning Function, (2) the name of a
### Positioning Function as a character string, or (3) a list
### containing any number of (1), (2), or additionally named
### values. Starting from the data frame of points to plot for the
### panel, the elements of the list are applied in sequence, and each
### row of the resulting data frame is used to draw a direct
### label. See examples in ?direct.label and ?positioning.functions.
 d,
### Data frame to which we apply the Positioning Methods.
 ...
### Passed to Positioning Functions.
 ){
  if(!is.list(method))method <- list(method)
  isconst <- function(){
    m.var <- names(method)[1]
    !(is.null(m.var)||m.var=="")
  }
  islist <- function()is.list(method[[1]])
  isref <- function()(!isconst())&&is.character(method[[1]])
  while(length(method)){
    ## Resolve any PF names or nested lists
    while(islist()||isref()){
      if(islist()){
        method <- c(method[[1]],method[-1])
      }else{ #must be character -> get the fun(s)
        method <- c(lapply(method[[1]],get),method[-1])
      }
    }
    if(isconst())
      d[[names(method)[1]]] <- method[[1]]
    else{
      old <- d
      d <- method[[1]](d,...)
      attr(d,"orig.data") <-
        if(is.null(attr(old,"orig.data")))old
        else attr(old,"orig.data")
    }
    method <- method[-1]
  }
  d
### The final data frame returned after applying all of the items in
### the Positioning Function list.
}


### Transformation function for 1d densityplots.
trans.densityplot <- dl.indep({
  dens <- density(d$x,na.rm=TRUE)
  data.frame(x=dens$x,y=dens$y)
})
trans.density <- trans.densityplot

### Transformation function for 1d qqmath plots. This is a copy-paste
### from panel.qqmath. (total hack)
trans.qqmath <- function(d,distribution,f.value,qtype=7,...){
  ddply(d,.(groups),function(d){
    x <- as.numeric(d$x)
    distribution <- if (is.function(distribution)) 
      distribution
    else if (is.character(distribution)) 
      get(distribution)
    else eval(distribution)
    nobs <- sum(!is.na(x))
    if (is.null(f.value)) 
      data.frame(x = distribution(ppoints(nobs)), y = sort(x))
    else data.frame(x = distribution(
                      if (is.numeric(f.value))f.value
                      else f.value(nobs)),
                    y = quantile(x,
                      if (is.numeric(f.value))f.value
                      else f.value(nobs),
                      names = FALSE, type = qtype, na.rm = TRUE))
  })
}

### Place points on top of the mean value of the rug.
rug.mean <- function(d,...,end)
  ddply(d,.(groups),function(d)
        data.frame(x=mean(d$x),
                   y=as.numeric(convertY(unit(end,"npc"),"native")),
                   vjust=0))

### Label points at the top, making sure they don't collide.
top.qp <- list(top.points,calc.boxes,qp.labels("x","w"))

### Label points at the zero before the first nonzero y value.
lasso.labels <-
  list(rot=60,
       dl.indep({
         d <- d[order(d$x),]
         i <- which(d$y!=0)[1]
         hjust <- as.integer(d[i,"y"]>0)
         data.frame(d[i-1,],hjust,vjust=hjust)
       }),
       calc.boxes,
       ## calculate how wide the tilted box is
       dl.trans(h.inches=convertHeight(unit(h,"native"),"inches",TRUE)),
       dl.trans(hyp.inches=h.inches/sin(2*pi*rot/360)),
       dl.trans(hyp=convertWidth(unit(hyp.inches,"inches"),"native",TRUE)),
       ## avoid collisions between tilted boxes
       qp.labels("x","hyp"))

default.picker <- function
### Look at options() for a user-defined default Positioning Function
### picker, and use that (or the hard-coded default picker), with the
### calling environment to figure out a good default.
(f
### Object class to look for (trellis or ggplot).
 ){
  varname <- paste("defaultpf.",f,sep="")
  p <- getOption(paste("directlabels.",varname,sep=""))
  if(is.null(p))p <- get(varname)
  do.call(p,as.list(parent.frame()))
}

### Calculate a 2d density estimate then follow the gradient to a
### point outside the convex hull.
dens.gradient <- function(d,...){
  require(ks)
  est <- drvkde(with(d,cbind(x,y)),1:2,1,se=FALSE)
  ##print(dens)
  d
}
