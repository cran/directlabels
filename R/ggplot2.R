uselegend.ggplot <- function
### Show the ggplot2 legend, for comparison.
(p,
### The ggplot object.
 ...
### Ignored.
 ){
  p
}

geom_dl <- structure(function
### Geom that will plot direct labels.
(mapping=NULL,
### aes(label=variable_that_will_be_used_as_groups_in_Positioning_Methods).
 method,
### Positioning Method.
 ...
### passed to GeomDirectLabel$new. ie stat= position= debug=
 ){
  require(ggplot2)
  ## Geom for direct labeling that creates dlgrobs in the draw()
  ## method.
  GeomDirectLabel <- proto(Geom, {
    draw_groups <- function(., ...) .$draw(...)
    draw <- function(., data, scales, coordinates,
                     method=NULL,debug=FALSE, ...) {
      data$rot <- as.integer(data$angle)
      data$groups <- data$label
      dlgrob(subset(coordinates$transform(data, scales),select=-group),
             method,debug=debug,
             axes2native=function(data){
               coordinates$transform(data, scales)
             })
    }
    draw_legend <- function(.,data,...){
      data <- aesdefaults(data,.$default_aes(),list(...))
      with(data,{
        textGrob("dl",0.5,0.5,rot=angle,
                 gp=gpar(col=alpha(colour,alpha),fontsize=size*.pt))
      })
    }
    objname <- "dl"
    desc <- "Direct labels"
    default_stat <- function(.) StatIdentity
    required_aes <- c("x", "y", "label")
    default_aes <- function(.)
      aes(colour="black", size=5 , angle=0, hjust=0.5, vjust=0.5, alpha = 1)
  })
  GeomDirectLabel$new(mapping,method=method,...)
### Layer that will plot direct labels.
},ex=function(){
  library(ggplot2)
  vad <- as.data.frame.table(VADeaths)
  names(vad) <- c("age","demographic","deaths")
  ## color + legend
  leg <- ggplot(vad,aes(deaths,age,colour=demographic))+
    geom_line(aes(group=demographic))
  print(leg)
  direct.label(leg,list("last.points",rot=30))
  direct.label(leg,list("last.points",rot=30),TRUE)
  leg+geom_dl(aes(label=demographic),list("last.points",rot=30),debug=TRUE)
  ## no color, just direct labels!
  p <- ggplot(vad,aes(deaths,age))+
    geom_line(aes(group=demographic))+
    geom_dl(aes(label=demographic),method="top.qp")
  print(p)
  ## add color:
  p+aes(colour=demographic)+
    scale_colour_discrete(legend=FALSE)
  ## add linetype:
  p+aes(linetype=demographic)+
    scale_linetype(legend=FALSE)
  ## no color, just direct labels
  data(BodyWeight,package="nlme")
  bwbase <- ggplot(BodyWeight,aes(Time,weight,label=Rat))+
    geom_line(aes(group=Rat))+
    facet_grid(~Diet)
  bw <- bwbase+geom_dl(method="last.qp")
  print(bw)
  ## add some more direct labels
  bw2 <- bw+geom_dl(method="first.qp")
  print(bw2)
  ## add color
  bw2+aes(colour=Rat)+
    scale_colour_discrete(legend=FALSE)
  ## add color and legend
  bwleg <- bwbase+aes(colour=Rat)
  direct.label(bwleg)
})

direct.label.ggplot <- function
### Direct label a ggplot2 grouped plot.
(p,
### The ggplot object.
 method=NULL,
### Method for direct labeling as described in ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  require(proto)
  require(ggplot2)
  SCALE <- scale_colour_discrete
  ## First look through layers for a colour aesthetic
  maps <- lapply(p$layers,function(L){
    m <- p$mapping
    m[names(L$mapping)] <- L$mapping
    m
  })
  cvars <- lapply(maps,"[[","colour")
  has.colour <- !sapply(cvars,is.null)
  if(any(has.colour)){
    i <- which(has.colour)[1] ##just pick the first one
    L <- p$layers[[i]]
    colvar <- cvars[[i]]
    ## FIXME: kind of a hack.
    if(colvar=="..level..")SCALE <- scale_colour_continuous
    ##colvar <- gsub("^[.][.](.*)[.][.]$","\\1",colvar)
  }else stop("Need colour aesthetic to infer default direct labels.")
  ## Try to figure out a good default based on the colored geom
  geom <- L$geom$objname
  if(is.null(method))method <- default.picker("ggplot")
  aes_args <- list(label=as.symbol(colvar),colour=as.symbol(colvar))
  dlgeom <- geom_dl(do.call(aes,aes_args),method,
                    stat=L$stat,debug=debug,data=L$data)
  scale.types <- sapply(p$scales$.scales,"[[",".output")
  scale.i <- which("colour"==scale.types)
  if(length(scale.i)){
    p$scales$.scales[[scale.i[1]]]$legend <- FALSE
  }else{
    p <- p+SCALE(legend=FALSE)
  }
  p+dlgeom
### The ggplot object with direct labels added.
}

defaultpf.ggplot <- function
### Default method selection method for ggplot2 plots.
(geom,p,...){
  switch(geom,
         density="top.bumptwice",
         line={
           varnames <- c(groups="colour",x="x")
           if("y" %in% names(p$mapping))varnames <- c(varnames,y="y")
           rename.vec <- sapply(p$mapping[varnames],deparse)
           rename.vec <- gsub("[a-z]+[(]([^)]+)[)]","\\1",rename.vec)
           d <- structure(p$data[,rename.vec],names=names(varnames))
           if(nlevels(d$groups)==2)"lines2" else "maxvar.qp"
         },
         point="smart.grid",
         path="bottom.pieces",
         stop("No default label placement for this type of ggplot."))
}

