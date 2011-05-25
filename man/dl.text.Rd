\name{dl.text}
\alias{dl.text}
\title{dl text}
\description{To be used as panel.groups= argument in panel.superpose. Analyzes
arguments to determine correct text color for this group, and then
draws the direct label text.}
\usage{
dl.text(labs, group.number, col.line = NULL, col.points = NULL, col = NULL, col.symbol = NULL, type = NULL, ...)
}
\arguments{
  \item{labs}{table of labels and positions constructed by label.positions}
  \item{group.number}{which group we are currently plotting, according to levels(labs$groups)}
  \item{col.line}{line color}
  \item{col.points}{point color}
  \item{col}{general color}
  \item{col.symbol}{symbol color}
  \item{type}{plot type}
  \item{\dots}{ignored}
}



\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





