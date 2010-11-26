### Positioning Function for the top of a group of points.
top.points <- top.points

### Positioning Function for the bottom of a group of points.
bottom.points <-
  dl.indep(data.frame(d[which.min(d$y),],hjust=0.5,vjust=1))
