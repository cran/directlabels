\name{perpendicular.lines}
\alias{perpendicular.lines}
\title{perpendicular lines}
\description{Draw a line between the centers of each cluster, then draw a
perpendicular line for each cluster that goes through its
center. For each cluster, return the point the lies furthest out
along this line.}
\usage{
perpendicular.lines(d,
debug = FALSE,
...)
}
\arguments{
  \item{d}{Data frame with groups x y.}
  \item{debug}{If TRUE will draw points at the center of each cluster and some
lines that show how the points returned were chosen.}
  \item{\dots}{ignored.}
}

\value{Data frame with groups x y, giving the point for each cluster
which is the furthest out along the line drawn through its center.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





