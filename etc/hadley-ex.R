library(ggplot2)
label.positions <- function
### Calculates table of positions of each label. It does not draw
### anything, but is called for its return value. Normally you don't
### have to call label.positions explicitly. Instead, it is called for
### you by direct.label.
(x,
### x values of points to draw.
 y,
### y values of points to draw.
 subscripts,
### Subscripts of groups to consider.
 groups,
### Vector of groups.
 debug=FALSE,
### Show debug output? If TRUE, the resulting table of label positions
### will be printed.
 method,
### Method for direct labeling, specified in one of the following
### ways: (1) a Positioning Function, (2) the name of a Positioning
### Function as a character string, or (3) a list containing any
### number of (1), (2), or additionally named values. Starting from
### the data frame of points to plot for the panel, the elements of
### the list are applied in sequence, and each row of the resulting
### data frame is used to draw a direct label. See examples in
### ?direct.label and ?positioning.functions. NULL indicates to choose
### a Positioning Function based on the high-level plot function
### chosen (this is done in panel.superpose.dl).
 ...
### Passed to positioning method(s).
 ){
  levs <- levels(groups)
  groups <- groups[subscripts]
  d <- data.frame(x,groups)
  if(!missing(y))d$y <- y
  if(class(method)=="function")method <- list(method)
  for(m.num in seq_along(method)){
    m <- method[[m.num]]
    m.var <- names(method)[m.num]
    if(!(is.null(m.var)||m.var==""))d[[m.var]] <- m else{
      if(class(m)=="character"){
        method.name <- paste(m," ",sep="")
        m <- get(m)
      }else method.name <- ""
      d <- try(m(d,debug=debug,...))
      if(class(d)=="try-error")
        stop("direct label placement method ",method.name,"failed")
    }
  }
  ## rearrange factors in case pos fun messed up the order:
  d$groups <- factor(as.character(d$groups),levs)
  ## defaults for grid parameter values:
  for(p in c("hjust","vjust"))
    d[,p] <- if(p %in% names(d))as.character(d[,p]) else 0.5
  if(!"rot"%in%names(d))d$rot <- 0
  d <- unique(d)
  if(debug)print(d)
  d
### Data frame of direct label positions. Each row describes the
### position of 1 label to be drawn later.
}
direct.label <- function(x,...)UseMethod("direct.label")
dl.indep <- function # Direct label groups independently
### Makes a function you can use to specify the location of each group
### independently.
(expr
### Expression that takes a subset of the d data frame, with data from
### only a single group, and returns the direct label position.
 ){
  foo <- substitute(expr)
  f <- function(d,...)eval(foo)
  src <- paste("dl.indep(",deparse(foo),")",sep="")
  structure(function(d,...)ddply(d,.(groups),f,...),"source"=src)
### A Positioning Function.
}
last.points <-
  dl.indep(data.frame(d[which.max(d$x),],hjust=0,vjust=0.5))


direct.label.ggplot <- function
### Direct label a ggplot2 grouped plot.
(p,
### The ggplot object.
 method=NULL,
### Method for direct labeling as described in ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  rename.vec <- sapply(p$mapping[c("group","x","y")],deparse)
  d <- structure(p$data[,rename.vec],names=c("groups","x","y"))
  labtab <- label.positions(d$x,d$y,1:nrow(d),d$groups,debug,method)
  dlgeom <- geom_text(aes(x=x,y=y,group=groups,colour=groups,label=groups,
                          angle=rot,hjust=hjust,vjust=vjust),labtab)
  p+dlgeom
### The ggplot object with direct labels added.
}


vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
p <- qplot(deaths,age,data=vad,group=demographic,geom="line",colour=demographic)
direct.label(p,list(last.points,rot=30))
## Doesn't work, why?
direct.label(p,list(last.points,rot=30))+scale_colour_identity()
## This at least should work, right?
p+scale_colour_identity()

