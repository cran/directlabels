ScaleIdentityDooby <- proto(ScaleDiscrete, {  
  doc <- TRUE
  common <- c("colour","fill","size","shape","linetype")
  new <- function(., name=NULL, breaks=NULL, labels=NULL, formatter = NULL, variable="x") {
    .$proto(name=name, breaks=breaks, .labels=labels, .input=variable, .output=variable, formatter = formatter)
  }

  train <- function(., data, drop = FALSE) {
    .$breaks <- union(.$breaks, unique(data))
  }
  trained <- function(.) !is.null(.$.labels)  

  map_df <- function(., data) {
    if (!all(.$input() %in% names(data))) return(data.frame())
    data[, .$input(), drop=FALSE]
  }
  output_breaks <- function(.) .$breaks
  labels <- function(.) .$.labels

  # Documentation -----------------------------------------------

  objname <- "identity"
  desc <- "Use values without scaling"
  icon <- function(.) textGrob("f(x) = x", gp=gpar(cex=1.2))
  
  examples <- function(.) {
    colour <- c("red", "green", "blue", "yellow")
    qplot(1:4, 1:4, fill = colour, geom = "tile")
    qplot(1:4, 1:4, fill = colour, geom = "tile") + scale_fill_identity()
    
    # To get a legend, you also need to supply the labels to
    # be used on the legend
    qplot(1:4, 1:4, fill = colour, geom = "tile") +
      scale_fill_identity(labels = letters[1:4], name = "trt")
    
    # cyl scaled to appropriate size
    qplot(mpg, wt, data = mtcars, size = cyl)

    # cyl used as point size
    qplot(mpg, wt, data = mtcars, size = cyl) + scale_size_identity()
  
  }
  
})
myridge <- function(f,data,lambda=c(exp(-seq(-15,15,l=200)),0)){
  fit <- lm.ridge(f,data,lambda=lambda)
  X <- data[-which(names(data)==as.character(f[[2]]))]
  Xs <- svd(scale(X)) ## my d's should come from the scaled matrix
  dsq <- Xs$d^2
  ## make the x axis degrees of freedom
  df <- sapply(lambda,function(l)sum(dsq/(dsq+l)))
  coefmat <- rbind(coef(fit)[,-1],t(fit$coef))
  rownames(coefmat) <- NULL
  D <- data.frame(coefmat,lambda,df,
             scale=rep(c("unscaled","scaled"),each=length(df)))
  molt <- melt(D,id=c("lambda","df","scale"))
  ## add in the points for df=0
  limpts <- transform(subset(molt,lambda==0),lambda=Inf,df=0,value=0)
  res <- rbind(limpts,molt)
  list(cast=cast(res,...~scale),
       plot=res,
       lambda=lambda,
       df=df,
       dsq=dsq,
       svd=Xs,
       fit=fit)
}
library(ElemStatLearn)
pros <- subset(prostate,train==TRUE,select=-train)
m <- myridge(lpsa~.,pros)
direct.label(xyplot(scaled~unscaled,m$cast,groups=variable),extreme.grid)
direct.label(cloud(scaled~unscaled*df,m$cast,groups=variable),get.means)
direct.label(qplot(unscaled,scaled,data=m$cast,colour=variable,size=df))
direct.label(xyplot(value~df|scale,m$plot,groups=variable,type="o",pch="+"),list(last.points,dl.trans(x=x+0.1)))

l <- myridge(GNP.deflator~.,longley)
direct.label(xyplot(scaled~unscaled,l$cast,groups=variable),extreme.grid)
