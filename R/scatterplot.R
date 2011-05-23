### Calculate closest point on the alpha hull with size of the boxes,
### and put it outside that point. (only technically correct for
### aspect="iso" TODO: check and correct for perspective changes.)
### TODO: doesn't work with ggplot2 since we can't calculate bounding
### box.
closest.on.ahull <- function(d,...){
  edges.to.outside(ahull.points(d),visualcenter(d),...)
}

### Calculate closest point on the convex hull and put it outside that
### point. TODO: doesn't work with ggplot2 since we can't calculate
### bounding box. Assume d is the center for each point cloud and then
### use orig.data to calculate hull.
closest.on.chull <- function(d,...){
  edges.to.outside(chull.points(d),visualcenter(d),...)
}

empty.grid <- function
### Label placement method for scatterplots that ensures labels are
### placed in different places. A grid is drawn over the whole
### plot. Each cluster is considered in sequence and assigned to the
### point on this grid which is closest to the point given by
### the input data points. Makes use of attr(d,"orig.data").
(d,
### Data frame of target points on the scatterplot for each label.
 debug=FALSE,
### Show debugging info on the plot?
 ...
### ignored.
 ){
  NREP <- 10
  all.points <- attr(d,"orig.data")[,c("x","y")]
  if(any(table(d$groups)>1))d <- get.means(d)
  label.targets <- d
  gl <- function(v){
    s <- seq(min(all.points[,v]),max(all.points[,v]),l=NREP)
    if(expand){
      dif <- s[2]-s[1]
      s <- seq(min(all.points[,v])-expand*dif,
               max(all.points[,v])+expand*dif,
               l=NREP+2*expand)
    }
    list(centers=s,diff=s[2]-s[1])
  }
  hgrid <- function(x,w){
    hboxes <- floor(diff(range(all.points[,x]))/r[,w])
    (-expand:(hboxes+expand-1))*r[,w]+min(all.points[,x])+r[,w]/2
  }
  if(debug)with(label.targets,{
    grid.points(x,y,default.units="native",gp=gpar(col="green"))
  })
  draw <- function(g){
    gridlines <- with(g,list(x=unique(c(left,right)),y=unique(c(top,bottom))))
    drawlines <- function(a,b,c,d)
      grid.segments(a,b,c,d,"native",gp=gpar(col="grey"))
    with(gridlines,drawlines(min(x),y,max(x),y))
    with(gridlines,drawlines(x,min(y),x,max(y)))
  }
  res <- data.frame()
  for(v in label.targets$groups){
    r <- subset(label.targets,groups==v)
    no.points <- data.frame()
    expand <- 0
    while(nrow(no.points)==0){
      boxes <- if("left"%in%names(label.targets)){
        list(x=hgrid("x","w"),y=hgrid("y","h"),w=r$w,h=r$h)
      }else{
        L <- sapply(c("x","y"),gl,simplify=FALSE)
        list(x=L$x$centers,y=L$y$centers,w=L$x$diff,h=L$y$diff)
      }
      boxes <- calc.borders(do.call(expand.grid,boxes))
      boxes <- cbind(boxes,data=inside(boxes,all.points))
      no.points <- transform(subset(boxes,data==0))
      expand <- expand+1 ## look further out if we can't find any labels inside
    }
    if(debug)draw(boxes)
    no.points <- transform(no.points,len=(r$x-x)^2+(r$y-y)^2)
    best <- subset(no.points,len==min(len))[1,]
    res <- rbind(res,transform(r,x=best$x,y=best$y))
    ## add points to cloud
    newpts <- with(best,data.frame(x=c(left,left,right,right,x,x,x,left,right),
                                   y=c(bottom,top,top,bottom,top,bottom,y,y,y)))
    all.points <- rbind(all.points,newpts)
  }
  if(debug)with(all.points,grid.points(x,y))
  res
### Data frame with columns groups x y, 1 line for each group, giving
### the positions on the grid closest to each cluster.
}

### Use bounding box information with a small empty.grid to find the a
### non-colliding label that is close to a point on the convex hull,
### which is close to the visual center of the data. TODO: does not
### work with ggplot2 since the backend does not support bounding box
### calculation.
smart.grid <- list("big.boxes","empty.grid")

### Use empty.grid with perpendicular.lines.
perpendicular.grid <- list("perpendicular.lines","empty.grid")

### Use empty.grid with extreme.points.
extreme.grid <- list("extreme.points","empty.grid")

