uselegend.ggplot <- function
### Show the ggplot2 legend, for comparison.
(p,
### The ggplot object.
 ...
### Ignored.
 ){
  p
}

### Geoms which need translation before applying Positioning Function.
need.trans.ggplot <- c()
direct.label.ggplot <- function
### Direct label a ggplot2 grouped plot.
(p,
### The ggplot object.
 method=NULL,
### Method for direct labeling as described in ?label.positions.
 debug=FALSE
### Show debug output?
 ){
  ##lvar <- if("group" %in% names(p$mapping))"group" else "colour"
  geom <- p$layers[[1]]$geom$objname
  if(is.null(method))method <- default.picker("ggplot")
  if(geom%in%need.trans.ggplot)method <-
    c(paste("trans.",geom,sep=""),method)
  ##print(p$layers[[1]]$mapping)
  dlgeom <- geom_text(position=position_dl(list(method),debug,p),
                      stat=p$layers[[1]]$stat)
  ##print(dlgeom)
##   for(i in seq_along(p$scales$.scales))
##     if(p$scales$.scales[[i]]$.output=="colour")
##       p$scales$.scales[[i]]$legend <- FALSE
  ## TDH 13 oct 2010 this doesn't work so well,
  ##lets go back to overwriting the color scale
  SCALE <- if(geom=="path")scale_colour_continuous else scale_colour_discrete
  p+dlgeom+SCALE(legend=FALSE)
### The ggplot object with direct labels added.
}

defaultpf.ggplot <- function
### Default method selection method for ggplot2 plots.
(geom,p,...){
  switch(geom,
         density="top.points",
         line={
           varnames <- c(groups="colour",x="x")
           if("y" %in% names(p$mapping))varnames <- c(varnames,y="y")
           rename.vec <- sapply(p$mapping[varnames],deparse)
           rename.vec <- gsub("[a-z]+[(]([^)]+)[)]","\\1",rename.vec)
           d <- structure(p$data[,rename.vec],names=names(varnames))
           if(nlevels(d$groups)==2)"lines2" else "maxvar.points"
         },
         point="empty.grid.2",
         path="bottom.points",
         stop("No default label placement for this type of ggplot."))
}

### Position class for direct label placement.
PositionDl <- proto(ggplot2:::Position,{
  method <- NULL
  debug <- FALSE
  orig <- NULL
  new <- function(., method=NULL, debug=FALSE, orig=NULL) {
    .$proto(method=method,debug=debug,orig=orig)
  }
  adjust <- function(.,data,scales){
    ##print(head(data))
    if(is.null(data$colour)){
      colvar <- .$orig$layers[[1]]$mapping$colour
      if(!is.null(colvar)){
        colvar <- gsub("^[.][.](.*)[.][.]$","\\1",colvar)
        data$colour <- data[,colvar]
      }else stop("Need colour aesthetic to direct label.")
    }
    d <- transform(data,groups=if("piece"%in%names(data))piece else colour)
    labtab <- label.positions(d,.$method[[1]],.$debug)
    targs <- list(label=if("colour"%in%names(labtab))"colour" else "groups",
                  angle="rot",
                  size="fontsize",
                  ##face="fontface",
                  ##family="fontfamily",
                  alpha="alpha")
    possibly.missing <- c("colour","group")
    toadd <- possibly.missing[(!possibly.missing%in%names(labtab))]
    targs[toadd] <- "groups"
    targs <- targs[targs%in%names(labtab)]
    targs <- sapply(targs,as.name)
    r <- do.call("transform",c(list(labtab),targs))
    if(is.numeric(data$colour)&&!is.numeric(r$colour))
      r$colour <- as.numeric(as.character(r$colour))
    ##print(head(r))
    ##Positive control:
    ##data.frame(head(data),label="foo")
    r
  }
  objname <- "dl"
})
### Position for internal use with geom_text.
position_dl <- PositionDl$build_accessor()

