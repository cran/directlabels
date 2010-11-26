\name{eval.list}
\alias{eval.list}
\title{Evaluate Positioning Method list}
\description{Run all the Positioning Functions on a given data set. This
function contains all the logic for parsing the method= argument
and sequentially applying the Positioning Functions to the input
data to obtain the label positions. This is useful since it is
often much less verbose to define Positioning Methods in list form
instead of function form, ex lasso.labels.}
\usage{
eval.list(method, d, ...)
}
\arguments{
  \item{method}{Direct labeling Positioning Method, specified in one of the
following ways: (1) a Positioning Function, (2) the name of a
Positioning Function as a character string, or (3) a list
containing any number of (1), (2), or additionally named
values. Starting from the data frame of points to plot for the
panel, the elements of the list are applied in sequence, and each
row of the resulting data frame is used to draw a direct
label. See examples in ?direct.label and ?positioning.functions.}
  \item{d}{Data frame to which we apply the Positioning Methods.}
  \item{\dots}{Passed to Positioning Functions.}
}

\value{The final data frame returned after applying all of the items in
the Positioning Function list.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





