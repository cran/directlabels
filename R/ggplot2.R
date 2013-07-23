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
  require(proto)
  ## Geom for direct labeling that creates dlgrobs in the draw()
  ## method.
  GeomDirectLabel <- proto(ggplot2:::Geom, {
    draw_groups <- function(., ...) .$draw(...)
    draw <- function(., data, scales, coordinates,
                     method=NULL,debug=FALSE, ...) {
      data$rot <- as.integer(data$angle)
      data$groups <- data$label
      axes2native <- function(data){
        ggplot2:::coord_transform(coordinates,data,scales)
      }
      converted <- axes2native(data)
      dldata <- converted[,names(converted)!="group"]
      dlgrob(dldata,
             method,debug=debug,
             axes2native=axes2native)
    }
    draw_legend <- function(.,data,...){
      data <- ggplot2:::aesdefaults(data,.$default_aes(),list(...))
      with(data,{
        textGrob("dl",0.5,0.5,rot=angle,
                 gp=gpar(col=alpha(colour,alpha),fontsize=size*.pt))
      })
    }
    objname <- "dl"
    desc <- "Direct labels"
    default_stat <- function(.) ggplot2:::StatIdentity
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
    geom_line(aes(group=demographic))+
    xlim(8,80)
  print(direct.label(leg,list("last.points",rot=30)))
  ## this is what direct.label is doing internally:
  labeled <- leg+
    geom_dl(aes(label=demographic),list("last.points",rot=30))+
    scale_colour_discrete(guide="none")
  print(labeled)
  ## no color, just direct labels!
  p <- ggplot(vad,aes(deaths,age))+
    geom_line(aes(group=demographic))+
    geom_dl(aes(label=demographic),method="top.qp")
  print(p)
  ## add color:
  p+aes(colour=demographic)+
    scale_colour_discrete(guide="none")
  ## add linetype:
  p+aes(linetype=demographic)+
    scale_linetype(guide="none")
  ## no color, just direct labels
  library(nlme)
  bwbase <- ggplot(BodyWeight,aes(Time,weight,label=Rat))+
    geom_line(aes(group=Rat))+
    facet_grid(.~Diet)
  bw <- bwbase+geom_dl(method="last.qp")
  print(bw)
  ## add some more direct labels
  bw2 <- bw+geom_dl(method="first.qp")
  print(bw2)
  ## add color
  colored <- bw2+aes(colour=Rat)+
    scale_colour_discrete(guide="none")
  print(colored)
  ## or just use direct.label if you use color:
  direct.label(bwbase+aes(colour=Rat),dl.combine("first.qp","last.qp"))

  ## iris data example
  giris <- ggplot(iris,aes(Petal.Length,Sepal.Length))+
    geom_point(aes(shape=Species))
  giris.labeled <- giris+
    geom_dl(aes(label=Species),method="smart.grid")+
    scale_shape_manual(values=c(setosa=1,virginica=6,versicolor=3),
                       guide="none")
  ##png("~/R/directlabels/www/scatter-bw-ggplot2.png",h=503,w=503)
  print(giris.labeled)
  ##dev.off()
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
  require(ggplot2)
  ## First look through layers for a colour aesthetic, TODO: look for
  ## fill aesthetic as well!
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
    colvar <- as.character(cvars[[i]])
  }else stop("Need colour aesthetic to infer default direct labels.")
  ## Try to figure out a good default based on the colored geom
  geom <- L$geom$objname
  if(is.null(method))method <- default.picker("ggplot")
  data <- if( (!is.null(L$data)) && (length(L$data) > 0) ){
    L$data
  }else{
    NULL
  }
  dlgeom <- geom_dl(aes_string(label=colvar,colour=colvar),method,
                    stat=L$stat,debug=debug,data=data)
  dlgeom$stat_params <- L$stat_params
  p+dlgeom+guides(color="none")
### The ggplot object with direct labels added.
}

defaultpf.ggplot <- function
### Default method selection method for ggplot2 plots.
(geom,p,L,colvar,...){
  switch(geom,
         density="top.bumptwice",
         line={
           groups <- L$data[[colvar]]
           if(is.null(groups))groups <- p$data[[colvar]]
           if(nlevels(groups)==2)"lines2" else "maxvar.qp"
         },
         point="smart.grid",
         path="bottom.pieces",
         stop("No default label placement for this type of ggplot."))
}

