\name{extract.posfun}
\alias{extract.posfun}
\title{Extract Positioning Function for documentation}
\description{Use inlinedocs to extract comments and definitions from code, then
for each item found add the value and its name to the list.}
\usage{
extract.posfun(f)
}
\arguments{
  \item{f}{R code file, which should contain only Positioning Functions that
can be used with examples defined in the doc/ subdirectory with
the same name.}
}

\value{List of lists, each of which describes one Positioning Function
defined in f.}

\author{Toby Dylan Hocking <toby.hocking@inria.fr>}





