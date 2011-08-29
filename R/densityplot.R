### Positioning Method for the top of a group of points.
top.points <-
  gapply.fun(transform(d[which.max(d$y),],hjust=0.5,vjust=0))

### Label the tops, but bump labels up to avoid collisions.
top.bumpup <- list("top.points","bumpup")

### Label the tops, bump labels up to avoid other labels, then to the
### side to avoid collisions with points.
top.bumptwice <- function(d,debug=FALSE,...){
  labtab <- apply.method(top.bumpup,d)
  if(debug)draw.rects(labtab)
  gapply(labtab,function(l,...){
    x <- sort(c(range(d$x),subset(d,y>l$bottom&y<l$top)$x))
    if(length(x)==2)return(l)
    dif <- diff(x)
    ok <- dif>with(l,right-left)
    if(!any(ok))ok[1] <- TRUE
    i <- which(ok)
    intervals <- data.frame(left=x[i]+l$w/2,right=x[i+1]-l$w/2,i=seq_along(i))
    dists <- transform(melt(intervals,id="i"),dist=value-l$x)
    best <- dists[which.min(abs(dists$dist)),]
    besti <- intervals[best$i,]
    l$x <- if(l$x<besti$right & l$x>besti$left)l$x else best$value
    l
  })
}
    
