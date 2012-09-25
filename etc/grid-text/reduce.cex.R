data(BodyWeight,package="nlme")
library(lattice)
p <- xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',
            layout=c(3,1),xlim=c(-10,75))
install.packages("directlabels")
library(directlabels)

calc.boxes <- function
### Calculate boxes around labels, for collision detection.
(d,
 debug=FALSE,
 ...
 ){
  vp <- current.viewport()
  convert <- function(worh){
    conv <- get(paste("convert",worh,sep=""))
    stri <- get(paste("string",worh,sep=""))
    with(d,sapply(seq_along(groups),function(i){
      if("cex"%in%names(d))vp$gp <- gpar(cex=cex[i])
      pushViewport(vp)
      if(debug)grid.rect() ##highlight current viewport
      w <- conv(stri(as.character(groups[i])),"cm")
      popViewport()
      w
    }))
  }
  ## abs since we have a weird bug with ggplot2 sometimes
  d$w <- abs(convert("Width"))
  d$h <- abs(convert("Height"))
  calc.borders(d)
}

reduce.cex <-   function(d,...){

  d <- first.points(d)
  d$cex <- 1
  d <- calc.boxes(d)
  d$h <- d$h * 1.2
  d <- calc.borders(d)

  ## sorts data so that target_1 <= target_2 <= ... <= target_n.
  d <- d[order(d$y),]

  ## check limits to see if there is enough space, given specified
  ## cex.
  l <- convertY(unit(c(0,1),"npc"),"cm",valueOnly=TRUE)

  h.available <- l[2] - l[1]
  h.occupied <- sum(d$h)
  if(h.occupied > h.available){ ## then there is not enough space.
    ## HELP: I thought this should be 1.0!!!
    d$cex <- d$cex * h.available / h.occupied  * 1.0
    d <- calc.boxes(d)
  }
  cat(sprintf("available: %5.2f occupied: %5.2f after: %5.2f\n",h.available,h.occupied,sum(d$h)))

  require(quadprog)
  ## These are the standard form matrices described in the
  ## directlabels poster.
  target <- d$y
  k <- nrow(d)
  D <- diag(rep(1,k))
  Ik <- diag(rep(1,k-1))
  A <- rbind(0,Ik)-rbind(Ik,0)
  b0 <- (d$top-target)[-k] + (target-d$bottom)[-1]

  ## bottom limit constraint.
  c.vec <- rep(0,k)
  c.vec[1] <- 1
  A <- cbind(A,c.vec)
  b0 <- c(b0,l[1]+target[1]-d$bottom[1])

  ## top limit constraint.
  c.vec <- rep(0,k)
  c.vec[k] <- -1
  A <- cbind(A,c.vec)
  b0 <- c(b0,d$top[k]-target[k]-l[2])

  sol <- solve.QP(D,target,A,b0)
  d$y <- sol$solution
  d
}

direct.label(p, reduce.cex)

