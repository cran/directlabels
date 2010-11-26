\name{empty.grid}
\alias{empty.grid}
\title{empty grid}
\description{Label placement method for scatterplots that ensures labels are
placed in different places. A grid is drawn over the whole
plot. Each cluster is considered in sequence and assigned to the
point on this grid which is closest to the point given by
loc.fun().}
\usage{
empty.grid(d, debug = FALSE, loc.fun = get.means, ...)
}
\arguments{
  \item{d}{Data frame of points on the scatterplot with columns groups x y.}
  \item{debug}{Show debugging info on the plot? This is passed to loc.fun.}
  \item{loc.fun}{Function that takes d and returns a data frame with 1 column for
each group, giving the point we will use to look for a close point
on the grid, to put the group label.}
  \item{\dots}{ignored.}
}

\value{Data frame with columns groups x y, 1 line for each group, giving
the positions on the grid closest to each cluster.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





