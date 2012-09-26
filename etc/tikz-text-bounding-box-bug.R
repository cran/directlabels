load(url("http://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/directlabels/data/SegCost.RData?revision=603&root=directlabels"))
SegCost$error <- factor(SegCost$error,c("FP","FN","E","I"))
err.df <- subset(SegCost,type!="Signal")

fp.fn <- list(colors=c(FP="skyblue",FN="#E41A1C",I="black",E="black"),
              sizes=c(FP=2.5,FN=2.5,I=1,E=1),
              linetypes=c(FP="solid",FN="solid",I="dashed",E="solid"))
patterns <- c(normal="%s",
              math="$%s$",
              text="$\\text{%s}$",
              hat="$\\hat{\\text{%s}}$",
              frac="$\\frac{%s}{5}$")
for(N in names(fp.fn)){
  for(pattern in patterns){
    newvals <- fp.fn[[N]]
    names(newvals) <- sprintf(pattern,names(newvals))
    fp.fn[[N]] <- c(fp.fn[[N]],newvals)
  }
}
library(ggplot2)
library(grid)
## The usual ggplot without direct labels.
library(tikzDevice)
options(tikzMetricPackages=c("\\usepackage[T1]{fontenc}\n",
          "\\usetikzlibrary{calc}\n",
          "\\usepackage{amsmath}\n"),
        tikzLatexPackages=c("\\usepackage{tikz}\n",                        
          "\\usepackage[active,tightpage,psfixbb]{preview}\n",
          "\\PreviewEnvironment{pgfpicture}\n",
          "\\setlength\\PreviewBorder{0pt}\n",
          "\\usepackage{amsmath}\n"))
library(directlabels)
show <- function(what){
  pattern <- patterns[[what]]
  df <- transform(err.df,error=sprintf(pattern,error))
kplot <- ggplot(df,aes(segments,cost))+
  geom_line(aes(colour=error,size=error,linetype=error))+
  facet_grid(type~bases.per.probe)+
  scale_linetype_manual(values=fp.fn$linetypes)+
  scale_colour_manual(values=fp.fn$colors)+
  scale_size_manual(values=fp.fn$sizes)+
  scale_x_continuous(limits=c(0,20),breaks=c(1,7,20),minor_breaks=NULL)+
  theme_bw()+
  theme(panel.margin=unit(0,"lines"))+
  guides(colour="none",linetype="none",size="none")
  m <- list("first.qp","calc.borders","draw.rects")
  tikz("test.tex",5,5,standAlone=TRUE)
  print(direct.label(kplot,m))
  dev.off()
  system("pdflatex test && evince test.pdf")
}

show("normal")
show("math")
show("text")
show("hat")

show("frac")
show("newline")
