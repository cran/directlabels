\name{apply.method}
\alias{apply.method}
\title{Apply a Positioning Method}
\description{Run a Positioning Method list on a given data set. This function
contains all the logic for parsing a Positioning Method and
sequentially applying its elements to the input data to obtain the
label positions. This is useful since it is often much less
verbose to define Positioning Methods in list form instead of
function form, ex lasso.labels.}
\usage{
apply.method(method,
d,
...,
debug = FALSE)
}
\arguments{
  \item{method}{Direct labeling Positioning Method, which is a list comprised of
any of the following: (1) a Positioning Function, (2) a character
string which is the name of an object that could be used, (3)
named values, or (4) a Positioning Method list. Starting from the
data frame of points to plot for the panel, the elements of the
list are applied in sequence, and each row of the resulting data
frame is used to draw a direct label.}
  \item{d}{Data frame to which we apply the Positioning Method.}
  \item{\dots}{Named arguments, passed to Positioning Functions.}
  \item{debug}{
}
}

\value{The final data frame returned after applying all of the items in
the Positioning Method list.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





