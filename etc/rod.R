nofacet <- read.csv("~/R/directlabels/pkg/directlabels/etc/rod.txt",header=TRUE)
library(ggplot2)
library(directlabels)
loci <- data.frame(ppp=c(rbeta(800,10,10),rbeta(100,0.15,1),rbeta(100,1,0.15)),
                   type=factor(c(rep("NEU",800),rep("POS",100),rep("BAL",100))))
PBPL1 <- melt(nofacet)
xticks <- seq ( from =-800 , by =100 , length =1700)
p <- densityplot(~value,PBPL1,groups=variable, pch=".", lwd=2,
                 xlab ="Cost of abatement (t/CO2-e)",
                 scales = list (x= list (at= xticks)))
rods.labels <-
  list(fontface="bold",hjust=0,top.points,
       dl.move("Dredge.Nozzles...Propellers.2",hjust=0,vjust=2),
       dl.move("Wind.Turbine..Acciona..as.Retailer",hjust=1,vjust=0),
       dl.move("Wind.Turbine..Nordex.",hjust=0,vjust=2),
       dl.move("Solar..300kw.Option.",hjust=1.1,vjust=0))
direct.label(p,rods.labels)

PBPLC<-ggplot(PBPL1,aes(x=value,fill=variable,colour=variable))
p<-PBPLC + geom_density(size=0.3,alpha=0.5) + theme_bw() +
  scale_x_continuous(breaks=seq(-1000,1000,100),formatter="dollar") +
  labs(x="Cost of abatement ($/tCO2-e)", y="Density") +
  opts(axis.text.x=theme_text(size=10,angle=50,hjust=1),
       legend.position = "none")
direct.label(p,list(rods.labels,fontsize=4))

bwplot(variable~value,PBPL1)

##earlier code
PBPLC<-ggplot(loci,aes(x=ppp,fill=type,colour=type))
p<-PBPLC+geom_density(size=0.3,alpha=0.5) +
  theme_bw() +
  scale_x_continuous(breaks=seq(-1000,1000,100),formatter="dollar") +
  labs(x="Cost of abatement ($/tCO2-e)", y="Density") +
  opts(axis.text.x=theme_text(size=10,angle=50,hjust=1))+
  opts(legend.position="none")
print(direct.label(p,"top.bumpup"))
