proportions <- read.table("proportions.txt")
library(ggplot2)
p <- ggplot(proportions,aes(year,value))+
  geom_line(aes(groups=disease,colour=disease))
library(directlabels)
for(f in Sys.glob("../R/*.R"))source(f)
direct.label(p,"last.qp")
direct.label(p,"first.qp")
