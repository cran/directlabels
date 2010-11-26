n <- 10
y <- rexp(n)
save(y,file="qptest.Rdata")

plot(rep(0,n),y)
m <- mean(y)
points(0,m,pch="m")
h <- 0.2
w <- 1
rect(-w/2,m-h/2,w/2,m+h/2,lty="dashed")

In <- diag(rep(1,n))
zero <- diag(rep(0,n))
one.vec <- rep(1,n)
zero.vec <- rep(0,n)
A <- rbind(cbind(t(one.vec),t(zero.vec),t(zero.vec),t(zero.vec)),
           cbind(In,In,zero,In),
           cbind(-In,zero,In,In))
b0 <- as.vector(cbind(y,zero.vec,zero.vec,one.vec*h/2))
d <- rep(0,2*n+1)
d[1] <- m
D <- diag(rep(0,2*n+1))
D[1,1] <- 1
epsilon <- 0.001
diag(D) <- diag(D)+epsilon
sol <- solve.QP(D,d,A,b0,n)

##simpler, only 1 point:
m <- 0.9
y <- 1
f <- function(x)(x-m)^2
curve(f(x),0,2)
h <- 0.5
pts <- c(y-h/2,y,y+h/2,m)
pch <- c("[","y","]","m")
points(pts,f(pts),pch=pch)
A <- cbind(diag(rep(1,3)),c(0,1,1))
b0 <- c(y,0,0,h/2)
Q <- t(c(1,1,-1))
D <- t(Q)%*%Q
diag(D) <- diag(D)+epsilon
d <- m*Q
sol <- solve.QP(D,d,A,b0,1)
sol
