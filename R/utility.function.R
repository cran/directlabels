label.endpoints <- function
### Make a Positioning Method that labels a certain x value.
(FUN,
### FUN(d$x) should return an index of which point to label. for
### example you can use which.min or which.max.
 hjust
### hjust of the labels.
 ){
  function(d,...)ddply(d,.(groups),function(d,...){
    i <- FUN(d$x)
    if(length(i))data.frame(d[i,],hjust,vjust=0.5)
    else data.frame()
  })
### A Positioning Method like first.points or last.points.
}

dl.combine <- structure(function # Combine output of several methods
### Apply several Positioning methods to the original data frame.
(...
### Several Positioning Functions.
 ){
  FUNS <- list(...)
  pf <- function(d,...){
    dfs <- lapply(FUNS,eval.list,d)
    res <- data.frame()
    for(df in dfs){
      if(nrow(res))res <- merge(df,res,all=TRUE)
      else res <- df
    }
    res
  }
  pf
### A Positioning Function that returns the combined data frame after
### applying each specified Positioning Function.
},ex=function(){
  ## Simple example: label the start and endpoints
  data(BodyWeight,package="nlme")
  library(lattice)
  ratplot <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))
  plot(direct.label(ratplot,dl.combine(first.points,last.points)))
  ## can also do this by repeatedly calling direct.label (ugly)
  plot(direct.label(direct.label(ratplot,last.points),first.points))
  library(ggplot2)
  rp2 <- qplot(Time,weight,data=BodyWeight,geom="line",facets=.~Diet,colour=Rat)
  print(direct.label(direct.label(rp2,last.points),first.points))

  mylars <- function
  ## Least angle regression algorithm for calculating lasso solutions.
  (x,
   ## Matrix of predictor variables.
   y,
   ## Vector of responses.
   epsilon=1e-6
   ## If correlation < epsilon, we are done.
   ){
    xscale <- scale(x) # need to work with standardized variables
    b <- rep(0,ncol(x))# coef vector starts at 0
    names(b) <- colnames(x)
    ycor <- apply(xscale,2,function(xj)sum(xj*y))
    j <- which.max(ycor) # variables in active set, starts with most correlated
    alpha.total <- 0
    out <- data.frame()
    
    while(1){## lar loop
      xak <- xscale[,j] # current variables
      r <- y-xscale%*%b # current residual
      ## direction of parameter evolution
      delta <- solve(t(xak)%*%xak)%*%t(xak)%*%r
      ## Current correlations (actually dot product)
      intercept <- apply(xscale,2,function(xk)sum(r*xk))
      ## current rate of change of correlations
      z <- xak%*%delta
      slope <- apply(xscale,2,function(xk)-sum(z*xk))
      ## store current values of parameters and correlation
      out <- rbind(out,data.frame(variable=colnames(x),
                                  coef=b,
                                  corr=abs(intercept),
                                  alpha=alpha.total,
                                  arclength=sum(abs(b)),
                                  coef.unscaled=b/attr(xscale,"scaled:scale")))

      if(sum(abs(intercept)) < epsilon)#corr==0 so we are done
        return(transform(out,s=arclength/max(arclength)))
      
      ## If there are more variables we can enter into the regression,
      ## then see which one will cross the highest correlation line
      ## first, and record the alpha value of where the lines cross.
      d <- data.frame(slope,intercept)
      d[d$intercept<0,] <- d[d$intercept<0,]*-1
      d0 <- data.frame(d[j[1],])# highest correlation line
      d2 <- data.frame(rbind(d,-d),variable=names(slope))#reflected lines
      ## Calculation of alpha for where lines cross for each variable
      d2$alpha <- (d0$intercept-d2$intercept)/(d2$slope-d0$slope)
      subd <- d2[(!d2$variable%in%colnames(x)[j])&d2$alpha>epsilon,]
      subd <- subd[which.min(subd$alpha),]
      nextvar <- subd$variable
      alpha <- if(nrow(subd))subd$alpha else 1
      
      ## If one of the coefficients would hit 0 at a smaller alpha
      ## value, take it out of the regression and continue.
      hit0 <- xor(b[j]>0,delta>0)&b[j]!=0
      alpha0 <- -b[j][hit0]/delta[hit0]
      takeout <- length(alpha0)&&min(alpha0) < alpha
      if(takeout){
        i <- which.min(alpha0)
        alpha <- alpha0[i]
      }
      
      b[j] <- b[j]+alpha*delta ## evolve parameters
      alpha.total <- alpha.total+alpha
      ## add or remove a variable from the active set
      j <- if(takeout)j[j!=which(names(i)==colnames(x))]
      else c(j,which(nextvar==colnames(x)))
    }
  }

  ## Calculate lasso path
  data(prostate,package="ElemStatLearn")
  pros <- subset(prostate,select=-train,train==TRUE)
  ycol <- which(names(pros)=="lpsa")
  x <- as.matrix(pros[-ycol])
  y <- unlist(pros[ycol])
  res <- mylars(x,y)
  P <- xyplot(coef~arclength,res,groups=variable,type="l")
  plot(direct.label(P,dl.combine(lasso.labels,last.qp)))

  data(diabetes,package="lars")
  dres <- with(diabetes,mylars(x,y))
  P <- xyplot(coef~arclength,dres,groups=variable,type="l")
  plot(direct.label(P,dl.combine(lasso.labels,last.qp)))
})

dl.indep <- structure(function # Direct label groups independently
### Makes a function you can use to specify the location of each group
### independently.
(expr
### Expression that takes a subset of the d data frame, with data from
### only a single group, and returns the direct label position.
 ){
  foo <- substitute(expr)
  f <- function(d,...)eval(foo)
  src <- paste("dl.indep(",paste(deparse(foo),collapse="\n"),")",sep="")
  pf <- structure(function(d,...)ddply(d,.(groups),f,...),"source"=src)
  pf
### A Positioning Function.
},ex=function(){
  complicated <- list(dl.trans(x=x+10),
                      dl.indep(d[-2,]),
                      rot=c(30,180))
  library(lattice)
  direct.label(dotplot(VADeaths,type="o"),complicated,TRUE)
})

dl.trans <- structure(function # Direct label data transform
### Make a function that transforms the data. This is for conveniently
### making a function that calls transform on the data frame, with the
### arguments provided. See examples.
(...
### Arguments to pass to transform.
 ){
  L <- as.list(match.call())[-1]
  pf <- function(d,...)do.call("transform",c(list(d),L))
  pf
### A Positioning Function.
},ex=function(){
  complicated <- list(dl.trans(x=x+10),
                      dl.indep(d[-2,]),
                      rot=c(30,180))
  library(lattice)
  direct.label(dotplot(VADeaths,type="o"),complicated,TRUE)
})

dl.move <- structure(function # Manually move a direct label
### Sometimes there is 1 label that is placed oddly by another
### Positioning Function. This function can be used to manually place
### that label in a good spot.
(group,
### Group to change.
 x,
### Horizontal position of the new label.
 y,
### Vertical position of the new label. If missing(y) and !missing(x)
### then we will calculate a new y value using linear interpolation.
 ...
### Variables to change for the specified group
 ){
  L <- list(...)
  if(!missing(x))L$x <- x
  if(!missing(y))L$y <- y
  pf <- function(d,...){
    v <- d$groups==group
    for(N in names(L))
      d[v,N] <- L[[N]]
    ## maybe generalize this to be symmetric on x and y one day?
    if("x" %in% names(L) && (!"y" %in% names(L))){
      orig <- attr(d,"orig.data")
      orig <- orig[orig$groups==group,]
      ## do linear interpolation to find a good y-value
      f <- with(orig,approxfun(x,y))
      d[v,"y"] <- f(L$x)
    }
    d
  }
  pf
### A Positioning Function that moves a label into a good spot.
},ex=function(){
  data(mpg,package="ggplot2")
  library(lattice)
  scatter <- xyplot(jitter(cty)~jitter(hwy),mpg,groups=class,aspect=1)
  dlcompare(list(scatter),
            list("extreme.grid",
                 `+dl.move`=list(extreme.grid,dl.move("suv",15,15))))

  data(svmtrain,package="directlabels")
  library(ggplot2)
  p <- qplot(log10(gamma),rate,data=svmtrain,group=data,colour=data,
             geom="line",facets=replicate~nu)
  dlcompare(list(p+xlim(-8,7)),list("last.points",
    `+dl.move`=list(last.points,dl.move("KIF11",-0.9,hjust=1,vjust=1))))
})

### Make a Positioning Function with empty.grid, that calculates label
### position targets using f.
empty.grid.fun <- function(f)
  function(d,debug,...)empty.grid(d,debug,f)

### Jitter the label positions.
dl.jitter <- dl.trans(x=jitter(x),y=jitter(y))

### Calculate boxes around labels, for collision detection.
calc.boxes <- function(d,debug=FALSE,...){
  vp <- current.viewport()
  convert <- function(worh){
    conv <- get(paste("convert",worh,sep=""))
    stri <- get(paste("string",worh,sep=""))
    with(d,sapply(seq_along(groups),function(i){
      if("cex"%in%names(d))vp$gp <- gpar(cex=cex[i])
      pushViewport(vp)
      if(debug)grid.rect() ##highlight current viewport
      w <- conv(stri(as.character(groups[i])),"native")
      popViewport()
      w
    }))
  }
  w <- convert("Width")
  h <- convert("Height")
  calc.borders(transform(d,w=w,h=h))
}

### Calculate big boxes around the means of each cluster.
big.boxes <- function(d,...)enlarge.box(calc.boxes(visualcenter(d)))

### Point in the middle of the min and max for each group.
visualcenter <-
  dl.indep(unique(transform(d,x=diff(range(x))/2+min(x),
                            y=diff(range(y))/2+min(y))))

### Positioning Function for the mean of each cluster of points.
get.means <-
  dl.indep(unique(transform(d,x=mean(x),y=mean(y))))

calc.borders <- function
### Calculate bounding box based on newly calculated width and height.
(d,
### Data frame of point labels, with new widths and heights in the w
### and h columns.
 ...
### ignored.
 ){
  hjust <- vjust <- 0.5 ##defaults in case unassigned in d
  transform(d,
            top=y+(1-vjust)*h,bottom=y-vjust*h,
            right=x+(1-hjust)*w,left=x-hjust*w,
            h=h,w=w)
}

### Positioning Function that draws boxes around label positions. Need
### to have previously called calc.boxes. Does not edit the data
### frame.
draw.rects <- function(d,...){
  ## easy way -- not correct, doesn't use calc'ed borders
  ##with(d,grid.rect(x,y,w,h,hjust=hjust,vjust=vjust,
  ##                 default.units="native",gp=gpar(col="grey")))
  d_ply(d,.(groups),function(D){
    with(D,grid.lines(c(left,left,right,right,left),
                      c(bottom,top,top,bottom,bottom),
                      "native",gp=gpar(col="grey")))
  })
  d
}

### Sequentially bump labels up, starting from the bottom, if they
### collide with the label underneath. NOTE: behavior is undefined
### when used with ggplot2 since it relies on the calc.boxes()
### function which doesn't know how to calculate bounding boxes for
### ggplot2 labels (yet).
bumpup <- function(d,...){
  d <- calc.boxes(d)[order(d$y),]
  "%between%" <- function(v,lims)lims[1]<v&v<lims[2]
  obox <- function(x,y){
    tocheck <- with(x,c(left,(right-left)/2+left,right))
    tocheck %between% with(y,c(left,right))
  }
  for(i in 2:nrow(d)){
    dif <- d$bottom[i]-d$top[i-1]
    ## here we are trying to test if box i can possibly collide with
    ## the box below it! Originally we checked if the bottom points of
    ## this box fall in the box below it, but this causes problems
    ## since we are reassigning box positions. If all boxes start at
    ## the same place, 2 will get moved up, 3 will not since its
    ## bottom points are no longer inside box 2. Solution: Look at box
    ## left and right limits and see if they collide!

    ## GOTCHA: If all the boxes are exactly the same size, on top of
    ## each other, then if we only examine left and right points of
    ## each box, none of the boxes will be detected as
    ## overlapping. One way to fix this is change > to >= in %between%
    ## but this is a bad idea since you can have boxes right next to
    ## each other that we don't want to move, that would be detected
    ## as overlapping. Solution: use the midpoint of the box as well!
    overlap <- c(obox(d[i,],d[i-1,]),obox(d[i-1,],d[i,]))
    if(dif<0&&any(overlap)){
      d$bottom[i] <- d$bottom[i]-dif
      d$top[i] <- d$top[i]-dif
      d$y[i] <- d$y[i]-dif
    }
  }
  d
}

### Use a QP solver to find the best places to put the points on a
### line, subject to the constraint that they should not overlap
qp.labels <- function(var,spacer)function(d,...){
  if(!spacer%in%names(d))stop("need to have calculated ",spacer)
  require(quadprog)
  d <- d[order(d[,var],decreasing=TRUE),]
  ## sorts data so that m_1 is on top, m_n on bottom.
  n <- nrow(d)
  D <- diag(rep(1,n))
  A <- diag(rep(1,n))[,-n]-rbind(0,diag(rep(1,n-1)))
  h <- d[,spacer]
  b0 <- (h[-n]+h[-1])/2
  sol <- solve.QP(D,d[,var],A,b0)
  d[,var] <- sol$solution
  d
}

### Make text bounding box larger by some amount.
enlarge.box <- function(d,...){
  if(!"h"%in%names(d))stop("need to have already calculated height and width.")
  h <- unit(d$h,"native")
  d$h <- d$h*2
  d$w <- d$w+as.numeric(convertWidth(convertHeight(h,"inches"),"native"))
  calc.borders(d)
}

in1which <- function
### Calculate which points fall in a box.
(p,
### data frame of points with columns x and y and many rows.
 box
### data frame of 1 row with columns left right top bottom.
 ){
  p$x>box$left & p$x<box$right & p$y<box$top & p$y>box$bottom
}

### Calculate how many points fall in a box.
in1box <- function(p,box)sum(in1which(p,box))

inside <- function
### Calculate for each box how many points are inside.
(boxes,
### Data frame of box descriptions, each row is 1 box, need columns
### left right top bottom.
 points
### Data frame of points, each row is 1 point, need columns x y.
 ){
  sapply(1:nrow(boxes),function(i)in1box(points,boxes[i,]))
### Vector of point counts for each box.
}

perpendicular.lines <- function
### Draw a line between the centers of each cluster, then draw a
### perpendicular line for each cluster that goes through its
### center. For each cluster, return the point the lies furthest out
### along this line.
(d,
### Data frame with groups x y.
 debug=FALSE,
### If TRUE will draw points at the center of each cluster and some
### lines that show how the points returned were chosen.
 ...
### ignored.
 ){
  means <- rename(get.means(d),list(x="mx",y="my",groups="groups"))
  big <- merge(d,means,by="groups")
  fit <- lm(my~mx,means)
  b <- coef(fit)[1]
  m <- coef(fit)[2]
  big2 <- transform(big,x1=(mx+x+(my-y)*m)/2)
  big3 <- transform(big2,y1=m*(x1-x)+y)
  big4 <- transform(big3,
                    d=sqrt((x-x1)^2+(y-y1)^2),
                    dm=sqrt((x-mx)^2+(y-my)^2))
  big5 <- transform(big4,ratio=d/dm)
  winners <- ddply(big5,.(groups),subset,
                   subset=seq_along(ratio)==which.min(ratio))
  ## gives back a function of a line that goes through the designated center
  f <- function(v)function(x){
    r <- means[means$groups==v,]
    -1/m*(x-r$mx)+r$my
  }
  ##dd <- ddply(means,.(groups),summarise,x=x+sdx*seq(0,-2,l=5)[-1])
  ##dd$y <- mdply(dd,function(groups,x)f(groups)(x))$x
  if(debug){
    ## myline draws a line over the range of the data for a given fun F
    myline <- function(F)
      grid.lines(range(d$x),F(range(d$x)),default.units="native")
    ## Then draw a line between these means
    myline(function(x)m*x+b)
    ## Then draw perpendiculars that go through each center
    for(v in means$groups)myline(f(v))
  }
  winners[,c("x","y","groups")]
### Data frame with groups x y, giving the point for each cluster
### which is the furthest out along the line drawn through its center.
}

### Label the points furthest from the origin for each group.
extreme.points <- dl.indep({
  d <- transform(d,d=sqrt(x^2+y^2))
  d[which.max(d$d),]
})

edges.to.outside <- function
### Given a list of edges from the convex or alpha hull, and a list of
### cluster centers, calculate a point near to each cluster on the
### outside of the hull.
(edges,centers,debug=FALSE){
  if(debug){
    with(centers,lpoints(x,y,pch="+"))
    with(edges,lsegments(x1,y1,x2,y2))
  }
  closepts <- ddply(centers,.(groups),project.onto.segments,edges,debug)
  closepts$vjust <- ifelse(closepts$y-centers$y>0,0,1)
  closepts$hjust <- ifelse(closepts$x-centers$x>0,0,1)
  r <- big.boxes(closepts)
  transform(r,x=(right-left)/2+left,y=(top-bottom)/2+bottom,hjust=0.5,vjust=0.5)
}

project.onto.segments <- function
### Given a point and a set of line segments representing a convex or
### alpha hull, calculate the closest point on the segments.
(m,
### m is 1 row, a center of a point cloud, we need to find the
### distance to the closest point on each segment of the convex
### hull.
 hull.segments,
### Data frame describing the line segments of the convex or alpha
### hull.
 debug=FALSE
 ){
  these <- within(hull.segments,{
    s <- (y2-y1)/(x2-x1)
    ## the closest point on the line formed by expanding this line
    ## segment (this expression is calculated by finding the minimum
    ## of the distance function).
    xstar <- (m$x + m$y*s + x1*s^2 - s*y1)/(s^2+1)
    minval <- apply(cbind(x1,x2),1,min)
    maxval <- apply(cbind(x1,x2),1,max)
    ## xopt is the closest point on the line segment
    xopt <- ifelse(xstar<minval,minval,ifelse(xstar>maxval,maxval,xstar))
    yopt <- s*(xopt-x1)+y1
    ## distance to each point on line segment from the center
    d <- (m$x-xopt)^2+(m$y-yopt)^2
  })
  i <- which.min(these$d)
  h <- with(these[i,],data.frame(x=xopt,y=yopt))
  if(debug)with(h,lsegments(m$x,m$y,h$x,h$y))
  h
}

### Make a Positioning Method from a set of points on a vertical line
### that will be spaced out using qp.labels
vertical.qp <- function(M)list(M,calc.boxes,qp.labels("y","h"))

