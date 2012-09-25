## Show the relationship between the precise breakpoint error, the
## usual breakpoint annotation error, and the signal reconstruction
## error.

load("data/variable.density.show.RData")

positive.part <- function(x)ifelse(x>0,x,0)
hat <- function(x){
  sprintf("$\\hat{\\text{%s}}$",x)
}
##annNames <- c(FN=hat("FN"),FP=hat("FP"),errors="$E$",I="$I$")
annNames <- c(FN="FN",FP="FP",errors="E",I="I")
annErr <- function(regions,sig){
  counts <- matrix(NA,length(sig$breaks),nrow(regions))
  FP <- matrix(NA,length(sig$breaks),nrow(regions))
  FN <- matrix(NA,length(sig$breaks),nrow(regions))
  usual.FP <- matrix(NA,length(sig$breaks),nrow(regions))
  usual.FN <- matrix(NA,length(sig$breaks),nrow(regions))
  
  allowable <- list("1breakpoint"=1,"0breakpoints"=0)
  for(j in 1:nrow(regions)){
    r <- regions[j,]
    for(i in 1:length(sig$breaks)){
      br <- sig$breaks[[i]]
      counts[i,j] <- sum(r$min <= br & br < r$max)
    }
    target <- allowable[[as.character(r$ann)]]
    FP[,j] <- positive.part(counts[,j]-max(target))
    FN[,j] <- positive.part(min(target)-counts[,j])
    usual.FP[,j] <- counts[,j] > max(target)
    usual.FN[,j] <- counts[,j] < min(target)
  }
  makeList <- function(FN,FP){
    L <- list(FN=rowSums(FN),FP=rowSums(FP))
    L$cost <- with(L,FP+FN)
    names(L) <- annNames[1:length(L)]
    L
  }
  list(usual=makeList(usual.FN,usual.FP),
       modified=makeList(FN,FP))
}

## calculate the error curves.
err.df <- do.call(rbind,lapply(variable.density.show,function(sig){
  df <- data.frame()
  base <- with(sig,{
    data.frame(bases.per.probe,segments=seq_along(cost))
  })
  makeDF <- function(cost,type,error){
    data.frame(base,cost,type,error)
  }

  log10.signal.error <- apply(sig$smooth,2,function(x){
    log10(mean( (x-sig$mu)^2 ))
  })
  df <- rbind(df,makeDF(log10.signal.error,"Signal","E"))

  sig[["E"]] <- sig$cost
  sig[["I"]] <- sig$I
  for(error in c("FP","FN","I","E")){
    cost <- sig[[error]]
    df <- rbind(df,makeDF(cost,"Breakpoint",error))
  }
  neg.regions <- with(sig$regions[order(sig$regions$min),],{
    data.frame(min=c(1,max),
               max=c(min,max(sig$locations)),
               annotation="0breakpoints")
  })
  regions <- rbind(sig$regions[,names(neg.regions)],neg.regions)
  half <- regions[1:nrow(regions)%%2==1,]
  half1 <- sig$regions[c(1,3,5),]
  annSets <- list(Complete=regions,
                  "Incomplete"=half,
                  "Positive"=half1)
  print(annSets)

  for(set in names(annSets)){
    ann <- annErr(annSets[[set]],sig)
    for(what in "modified"){##names(ann)){
      L <- ann[[what]]
      for(error in names(L)){
        type <- sprintf("%s %s",set,what)
        df <- rbind(df,makeDF(L[[error]],set,error))
      }
    }
  }
  
  df
}))

## The order here specifies the plotting order!
curves <- levels(err.df$error)
sort.order <- rep(0,length(curves))
names(sort.order) <- curves
sort.order[curves == "$E$"] <- 1
sort.order <- sort(sort.order)
err.df$error <- factor(err.df$error,names(sort.order))
## err.df$type <- factor(err.df$type,
##                       c("signal",
##                         "breakpoint",
##                         "annotation modified",
##                         "annotation usual"))

library(ggplot2)
library(directlabels)

source("scripts/fp.fn.colors.R")
fp.fn.colors[annNames] <- fp.fn.colors[names(annNames)]
fp.fn.sizes[annNames] <- fp.fn.sizes[names(annNames)]
fp.fn.linetypes[annNames] <- fp.fn.linetypes[names(annNames)]

## just to adjust the axes:
levs <- levels(err.df$type)
levs <- levs[levs!="Signal"]
makeBlank <- function(fun,ydiff){
  with(subset(err.df,cost==fun(cost)),{
    data.frame(bases.per.probe,segments,cost=cost+ydiff,
               type=levs)
  })
}
sig.blank <- with(subset(err.df,type=="Signal"),{
  data.frame(bases.per.probe,segments,cost=max(cost)+0.5,type)
})
blank <- rbind(makeBlank(max,2),
               makeBlank(min,-1),
               sig.blank)

adj <- 0.1
alter <- function(pre,xdiff){
  force(xdiff)
  list(sprintf("%s.points",pre),
       calc.boxes,
       function(d,...){
         d$x <- d$x+xdiff
         ## expand hat box size:
         by <- ifelse(grepl("hat",d$label),1.9,1.3)
         d$h <- d$h * by
         d
       },
       calc.borders,
       qp.labels("y","bottom","top",
                 make.tiebreaker("x","y"),
                 limits=ylimits)
       )
}
mymethod <- dl.combine(alter("first",-adj),alter("last",adj))


library(plyr)
min.df <- ddply(subset(err.df,error=="$E$"),
                .(bases.per.probe,type),subset,
                cost==min(cost))
                
library(grid)
kplot <- ggplot(err.df,aes(segments,cost))+
  geom_blank(data=blank)+
  geom_line(aes(colour=error,size=error,linetype=error))+
  geom_point(pch=1,data=min.df)+
  facet_grid(type~bases.per.probe,scales="free",
             labeller=function(var,val){
               vname <- if(var=="bases.per.probe"){
                 "bases/probe = "
               }else{
                 ""
               }
               sprintf("%s%s",vname,val)
               })+
  ##theme(title="Model selection error curves for 2 simulated signals")+
  scale_linetype_manual(values=fp.fn.linetypes)+
  scale_colour_manual(values=fp.fn.colors)+
  scale_size_manual(values=fp.fn.sizes)+
  geom_dl(aes(label=error,colour=error),method=mymethod)+
  guides(colour="none",linetype="none",size="none")+
  scale_x_continuous("segments of estimated signal",
                     limits=c(-1,22),breaks=c(1,5,7,20),
                     minor_breaks=NULL)+
  scale_y_continuous("",minor_breaks=NULL)+
  theme_bw()+theme(panel.margin=unit(0,"lines"))
print(kplot)

library(tikzDevice)
options(tikzMetricPackages=c("\\usepackage[T1]{fontenc}\n",
          "\\usetikzlibrary{calc}\n",
          "\\usepackage{amsmath}\n"))
print(getOption("tikzMetricPackages"))

show <- function(p){
  tikz(f <- "test.tex",5,5)
  print(kplot)
  dev.off()
  cmd <- sprintf("pdflatex %s && xpdf %s",f,f)
  system(cmd)
}
