data(nci,package="ElemStatLearn")
library(MASS)
LDA <- lda(t(nci),colnames(nci))
dirs <- t(nci) %*% LDA$scaling[,1:2]
df <- data.frame(dirs,type=rownames(dirs),row.names=NULL)
library(ggplot2)
ggplot(df,aes(LD1,LD2))+
  geom_text(aes(label=type,colour=type))
## better to just direct label every point than to use directlabels
