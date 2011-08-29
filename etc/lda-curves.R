## this script shows a plot at the end which is rather difficult to
## direct label.
N <- 200
P <- 10
set.seed(1)
X <- replicate(P,rnorm(N))
K <- 4
klass <- rep(1:K,each=N/K)
X[1:(N/K),1] <- rnorm(N/K,10)
X[1:(N/K),2] <- rnorm(N/K,2)
X[(N/K+1):(2*N/K),5] <- rnorm(N/K,5)
X[(2*N/K+1):(3*N/K),7] <- rnorm(N/K,3)
## setting: we know the classes, but we don't know which variables are
## useful for separating them. How to screen?

set.seed(8)
## answer: first divide in train and test, then try LDA on each
## variable.
prop <- function(x)sum(x)/length(x)
train <- 1:N%in%sample(1:N,N/2)
test <- !train
library(MASS)
all.vars <- 1:P
active.set <- c()
step <- 1
results <- data.frame()
done <- FALSE
accuracy <- 0
while(!done){
  props <- do.call(rbind,lapply(all.vars[!all.vars%in%active.set],function(j){
    train.vars <- c(active.set,j)
    LDA <- lda(X[train,train.vars,drop=FALSE],klass[train])
    correct <- predict(LDA,X[,train.vars,drop=FALSE])$class==klass
    ctrain <- prop(correct[train])
    ctest <- prop(correct[test])
    data.frame(prop=c(ctest,ctrain),
               set=c("test","train"),
               variable=j,
               step)
  }))
  df <- subset(props,set=="test")
  thisacc <- max(df$prop)
  if(thisacc>accuracy){
    picked <- df$variable[which.max(df$prop)]
    cat(sprintf("adding variable %s increase %f -> %f\n",
                picked,accuracy,thisacc))
    accuracy <- thisacc
    props <- transform(props,picked=variable==picked)
    active.set <- c(active.set,picked)
    if(length(active.set)==length(all.vars))done <- TRUE
    results <- rbind(results,props)
    step <- step+1
  }else{
    done <- TRUE
  }
}
results$variable <- factor(results$variable)
p <- ggplot(results,aes(step,prop,colour=variable))+
  geom_line(aes(group=interaction(variable,set),linetype=set))
direct.label(p,list(last.points))
