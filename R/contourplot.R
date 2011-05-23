### Positioning Method for the top of a group of points.
top.points <- top.points

### Positioning Method for the bottom of a group of points.
bottom.points <-
  gapply.fun(data.frame(d[which.min(d$y),],hjust=0.5,vjust=1))
