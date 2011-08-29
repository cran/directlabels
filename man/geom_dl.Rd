\name{geom_dl}
\alias{geom_dl}
\title{geom dl}
\description{Geom that will plot direct labels.}
\usage{
geom_dl(mapping = NULL,
method,
...)
}
\arguments{
  \item{mapping}{aes(label=variable_that_will_be_used_as_groups_in_Positioning_Methods).}
  \item{method}{Positioning Method.}
  \item{\dots}{passed to GeomDirectLabel$new. ie stat= position= debug=}
}

\value{Layer that will plot direct labels.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>}




\examples{
library(ggplot2)
vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
## color + legend
leg <- ggplot(vad,aes(deaths,age,colour=demographic))+
  geom_line(aes(group=demographic))
print(leg)
direct.label(leg,list("last.points",rot=30))
direct.label(leg,list("last.points",rot=30),TRUE)
leg+geom_dl(aes(label=demographic),list("last.points",rot=30),debug=TRUE)
## no color, just direct labels!
p <- ggplot(vad,aes(deaths,age))+
  geom_line(aes(group=demographic))+
  geom_dl(aes(label=demographic),method="top.qp")
print(p)
## add color:
p+aes(colour=demographic)+
  scale_colour_discrete(legend=FALSE)
## add linetype:
p+aes(linetype=demographic)+
  scale_linetype(legend=FALSE)
## no color, just direct labels
data(BodyWeight,package="nlme")
bwbase <- ggplot(BodyWeight,aes(Time,weight,label=Rat))+
  geom_line(aes(group=Rat))+
  facet_grid(~Diet)
bw <- bwbase+geom_dl(method="last.qp")
print(bw)
## add some more direct labels
bw2 <- bw+geom_dl(method="first.qp")
print(bw2)
## add color
bw2+aes(colour=Rat)+
  scale_colour_discrete(legend=FALSE)
## add color and legend
bwleg <- bwbase+aes(colour=Rat)
direct.label(bwleg)
}
