\name{label.positions}
\alias{label.positions}
\title{label positions}
\description{Calculates table of positions of each label based on input data
for each panel and Positioning Method. This is meant for internal
use inside a direct.label method, and is a wrapper around
apply.method which makes sure the inputs are good and the outputs
are plottable. apply.method is more efficient and should be used
in the context of other Positioning Methods (i.e. dl.combine) and
label.positions should be used when you actually when to plot the
result (i.e. in lattice+ggplot2 backends).}
\usage{
label.positions(d, method, debug = FALSE, ...)
}
\arguments{
  \item{d}{Data frame to which we will sequentially apply the Positioning
Method.}
  \item{method}{Method for direct labeling, described in
\code{\link{apply.method}}.}
  \item{debug}{Show debug output? If TRUE, the resulting table of label positions
will be printed.}
  \item{\dots}{Passed to Positioning Function(s).}
}

\value{Data frame of direct label positions. Each row describes the
position of 1 label to be drawn later.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





